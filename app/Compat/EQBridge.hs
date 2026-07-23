{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Serialized ownership of Meloid's native EQ and its PipeWire routes.
module Compat.EQBridge (
  EQBridge,
  newEQBridge,
  startEQBridge,
  stopEQBridge,
  applyEQ,
  routeMPDToEQ,
) where

import Control.Applicative ((<|>))
import Control.Concurrent (MVar, modifyMVar, newMVar, threadDelay)
import Control.Exception (IOException, tryJust)
import Control.Monad (forM_, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Aeson ((.:), (.:?))
import Data.Aeson qualified as JSON
import Data.Aeson.Types qualified as AesonTypes
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.List (nub, sortOn, stripPrefix)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector qualified as Vec
import Foreign.PipeWire qualified as PipeWire
import GHC.IO.Exception (ExitCode (..))
import System.Process qualified as Sys
import Text.Printf (printf)
import Types.Schemas.EQConfig

newtype EQBridge = EQBridge (MVar BridgeState)

data BridgeState
  = BridgeStopped
  | BridgeRunning [EQRoute]

data EQRoute = EQRoute
  { routeChannel :: String
  , routeSource :: Int
  , routeTarget :: Int
  }
  deriving (Eq, Ord, Show)

data PWNode = PWNode
  { nodeId :: Int
  , nodeName :: Maybe String
  , nodeApplicationName :: Maybe String
  }
  deriving (Show)

data PWPort = PWPort
  { portId :: Int
  , portNodeId :: Int
  , portName :: String
  , portDirection :: String
  , portAudioChannel :: Maybe String
  }
  deriving (Show)

data PWLink = PWLink
  { linkId :: Int
  , linkOutput :: Int
  , linkInput :: Int
  }
  deriving (Show)

data PWGraph = PWGraph
  { graphNodes :: Map Int PWNode
  , graphPorts :: Map Int PWPort
  , graphLinks :: Map Int PWLink
  }

data PWObject
  = ObjectNode PWNode
  | ObjectPort PWPort
  | ObjectLink PWLink
  | ObjectOther

data RoutePlan = RoutePlan
  { planRoutes :: [EQRoute]
  , planInputs :: Map String Int
  , planOutputs :: Map String Int
  , planDirectLinks :: [PWLink]
  }

channels :: [String]
channels = ["FL", "FR"]

newEQBridge :: IO EQBridge
newEQBridge = EQBridge <$> newMVar BridgeStopped

startEQBridge :: EQBridge -> EQConfigSpecs -> ExceptT String IO ()
startEQBridge (EQBridge lock) config =
  ExceptT $
    modifyMVar lock $ \case
      BridgeRunning routes -> pure (BridgeRunning routes, Right ())
      BridgeStopped -> do
        collision <- runExceptT $ do
          graph <- readGraph
          when (any isEQNode $ Map.elems $ graphNodes graph) $
            throwE "Another Meloid EQ bridge is already present in the PipeWire graph"
        case collision of
          Left err -> pure (BridgeStopped, Left err)
          Right () ->
            PipeWire.startEQ >>= \case
              Left err -> pure (BridgeStopped, Left err)
              Right () -> do
                configured <- runExceptT $ applyEQ config
                case configured of
                  Right () -> pure (BridgeRunning [], Right ())
                  Left err -> PipeWire.stopEQ >> pure (BridgeStopped, Left err)

-- | Stop accepting route changes, restore direct playback, then destroy the DSP.
-- The native bridge is always stopped, while restoration failures are returned.
stopEQBridge :: EQBridge -> IO (Either String ())
stopEQBridge (EQBridge lock) =
  modifyMVar lock $ \case
    BridgeStopped -> pure (BridgeStopped, Right ())
    BridgeRunning routes -> do
      restored <- runExceptT $ restoreRoutes routes
      PipeWire.stopEQ
      pure (BridgeStopped, restored)

applyEQ :: EQConfigSpecs -> ExceptT String IO ()
applyEQ (EQConfigSpecs gains) = do
  either throwE pure $ validateEQGains gains
  ExceptT $ PipeWire.setEQ gains

-- | Reconcile the full stereo route while holding the bridge lock. Each
-- attempt either commits a verified route or rolls back to direct playback.
routeMPDToEQ :: EQBridge -> ExceptT String IO ()
routeMPDToEQ (EQBridge lock) =
  ExceptT $
    modifyMVar lock $ \case
      BridgeStopped ->
        pure (BridgeStopped, Left "The PipeWire EQ bridge is not running")
      BridgeRunning routes -> do
        result <- runExceptT $ retry 10 routes
        case result of
          Left err -> pure (BridgeRunning routes, Left err)
          Right updated -> pure (BridgeRunning updated, Right ())
 where
  retry :: Int -> [EQRoute] -> ExceptT String IO [EQRoute]
  retry attempts previous = do
    initial <- readGraph
    plan <- either throwE pure $ makeRoutePlan initial previous
    let wanted = desiredLinks plan
        created = Set.filter (not . linkPairExists initial) wanted
        directPairs = Set.fromList [(linkOutput link, linkInput link) | link <- planDirectLinks plan]
    result <-
      liftIO . runExceptT $ do
        forM_ (Set.toList created) connectPorts
        connected <- readGraph
        verifyLinks "EQ links were not created" wanted connected
        forM_ (planDirectLinks plan) $ disconnectLink . linkId
        committed <- readGraph
        verifyLinks "The committed EQ route is incomplete" wanted committed
        when (any (linkPairExists committed) directPairs) $
          throwE "PipeWire recreated a direct MPD link while committing the EQ route"
        liftIO $ removeStaleEQLinks plan committed
    case result of
      Right () -> pure $ planRoutes plan
      Left err
        | attempts <= 1 -> liftIO (rollbackRoute directPairs created) >> throwE err
        | otherwise -> do
            liftIO $ rollbackRoute directPairs created
            liftIO $ threadDelay 100000
            retry (attempts - 1) previous

makeRoutePlan :: PWGraph -> [EQRoute] -> Either String RoutePlan
makeRoutePlan graph previous = do
  filterNode <- exactlyOne "Meloid EQ node" $ filter isEQNode (Map.elems $ graphNodes graph)
  let mpdNodeIds = Set.fromList $ nodeId <$> filter isMPDNode (Map.elems $ graphNodes graph)
  when (Set.null mpdNodeIds) $ Left "Could not find MPD's PipeWire node"
  inputs <- Map.fromList <$> traverse (filterPort filterNode "input") channels
  outputs <- Map.fromList <$> traverse (filterPort filterNode "output") channels
  sources <- Map.fromList <$> traverse (mpdPort mpdNodeIds) channels
  planned <- fmap concat . traverse (routesForChannel inputs sources) $ channels
  let direct =
        [ link
        | link <- Map.elems $ graphLinks graph
        , route <- planned
        , linkOutput link == routeSource route
        , linkInput link == routeTarget route
        ]
  pure
    RoutePlan
      { planRoutes = sortOn (\route -> (routeChannel route, routeTarget route)) planned
      , planInputs = inputs
      , planOutputs = outputs
      , planDirectLinks = direct
      }
 where
  ports = Map.elems $ graphPorts graph

  filterPort node direction channel = do
    port <-
      exactlyOne
        ("Meloid EQ " <> direction <> " port for " <> channel)
        [ candidate
        | candidate <- ports
        , portNodeId candidate == nodeId node
        , portDirection candidate == direction
        , canonicalChannel candidate == Just channel
        ]
    pure (channel, portId port)

  mpdPort nodeIds channel = do
    port <-
      exactlyOne
        ("MPD output port for " <> channel)
        [ candidate
        | candidate <- ports
        , portNodeId candidate `Set.member` nodeIds
        , portDirection candidate == "output"
        , canonicalChannel candidate == Just channel
        ]
    pure (channel, portId port)

  routesForChannel inputs sources channel = do
    source <- lookupRequired "MPD source" channel sources
    input <- lookupRequired "EQ input" channel inputs
    let directTargets =
          nub
            [ linkInput link
            | link <- Map.elems $ graphLinks graph
            , linkOutput link == source
            , linkInput link /= input
            ]
        rememberedTargets =
          nub
            [ routeTarget route
            | route <- previous
            , routeChannel route == channel
            , validTarget channel (routeTarget route)
            ]
        targets
          | null directTargets = rememberedTargets
          | otherwise = directTargets
    when (null targets) $
      Left $ "No playback destination is available for MPD channel " <> channel
    pure [EQRoute channel source target | target <- targets]

  validTarget channel target =
    case Map.lookup target (graphPorts graph) of
      Nothing -> False
      Just port -> maybe True (== channel) $ canonicalChannel port

desiredLinks :: RoutePlan -> Set (Int, Int)
desiredLinks plan =
  Set.fromList $
    [ (routeSource route, input)
    | route <- planRoutes plan
    , Just input <- [Map.lookup (routeChannel route) (planInputs plan)]
    ]
      <> [ (output, routeTarget route)
         | route <- planRoutes plan
         , Just output <- [Map.lookup (routeChannel route) (planOutputs plan)]
         ]

verifyLinks :: String -> Set (Int, Int) -> PWGraph -> ExceptT String IO ()
verifyLinks message wanted graph =
  unless (all (linkPairExists graph) wanted) $ throwE message

linkPairExists :: PWGraph -> (Int, Int) -> Bool
linkPairExists graph (output, input) =
  any
    (\link -> linkOutput link == output && linkInput link == input)
    (Map.elems $ graphLinks graph)

connectPorts :: (Int, Int) -> ExceptT String IO ()
connectPorts (output, input) =
  void $ readProcess "pw-link" ["-w", show output, show input] ""

disconnectLink :: Int -> ExceptT String IO ()
disconnectLink ident =
  void $ readProcess "pw-link" ["-d", show ident] ""

rollbackRoute :: Set (Int, Int) -> Set (Int, Int) -> IO ()
rollbackRoute direct created =
  void . runExceptT $ do
    graph <- readGraph
    forM_ (Set.toList direct) $ \pair ->
      unless (linkPairExists graph pair) $ connectPorts pair
    refreshed <- readGraph
    forM_
      [ linkId link
      | link <- Map.elems $ graphLinks refreshed
      , (linkOutput link, linkInput link) `Set.member` created
      ]
      disconnectLink

removeStaleEQLinks :: RoutePlan -> PWGraph -> IO ()
removeStaleEQLinks plan graph =
  void . runExceptT $
    forM_
      [ linkId link
      | link <- Map.elems $ graphLinks graph
      , managed link
      , (linkOutput link, linkInput link) `Set.notMember` desiredLinks plan
      ]
      disconnectLink
 where
  inputIds = Set.fromList $ Map.elems $ planInputs plan
  outputIds = Set.fromList $ Map.elems $ planOutputs plan
  managed link =
    linkInput link `Set.member` inputIds
      || linkOutput link `Set.member` outputIds

restoreRoutes :: [EQRoute] -> ExceptT String IO ()
restoreRoutes remembered = do
  graph <- readGraph
  let mpdNodeIds = Set.fromList $ nodeId <$> filter isMPDNode (Map.elems $ graphNodes graph)
      currentSources =
        Map.fromList
          [ (channel, portId port)
          | port <- Map.elems $ graphPorts graph
          , portNodeId port `Set.member` mpdNodeIds
          , portDirection port == "output"
          , Just channel <- [canonicalChannel port]
          , channel `elem` channels
          ]
      routes =
        [ EQRoute (routeChannel route) source (routeTarget route)
        | route <- remembered
        , Just source <- [Map.lookup (routeChannel route) currentSources]
        , Map.member (routeTarget route) (graphPorts graph)
        ]
      direct = Set.fromList [(routeSource route, routeTarget route) | route <- routes]
  forM_ (Set.toList direct) $ \pair ->
    unless (linkPairExists graph pair) $ connectPorts pair
  restored <- readGraph
  verifyLinks "Could not restore MPD's direct PipeWire route" direct restored
  let eqPortIds =
        Set.fromList
          [ portId port
          | port <- Map.elems $ graphPorts restored
          , maybe False isEQNode $ Map.lookup (portNodeId port) (graphNodes restored)
          ]
  forM_
    [ linkId link
    | link <- Map.elems $ graphLinks restored
    , linkInput link `Set.member` eqPortIds || linkOutput link `Set.member` eqPortIds
    ]
    disconnectLink

isEQNode :: PWNode -> Bool
isEQNode node = nodeName node == Just "meloid_eq_filter"

isMPDNode :: PWNode -> Bool
isMPDNode node =
  nodeApplicationName node == Just "Music Player Daemon"
    || maybe False (`elem` ["mpd.PipeWire", "Music Player Daemon"]) (nodeName node)

canonicalChannel :: PWPort -> Maybe String
canonicalChannel port =
  normalizeChannel <$> (portAudioChannel port <|> channelFromName (portName port))
 where
  channelFromName name =
    firstMatch ["output_", "input_", "playback_", "capture_"] name

  firstMatch [] _ = Nothing
  firstMatch (prefix : rest) name =
    case stripPrefix prefix name of
      Just suffix -> Just suffix
      Nothing -> firstMatch rest name

normalizeChannel :: String -> String
normalizeChannel "0" = "FL"
normalizeChannel "1" = "FR"
normalizeChannel channel = channel

exactlyOne :: String -> [a] -> Either String a
exactlyOne description = \case
  [value] -> Right value
  [] -> Left $ "Could not find " <> description
  _ -> Left $ "Found multiple candidates for " <> description

lookupRequired :: String -> String -> Map String a -> Either String a
lookupRequired description key =
  maybe (Left $ "Could not find " <> description <> " for " <> key) Right . Map.lookup key

readGraph :: ExceptT String IO PWGraph
readGraph = do
  output <- readProcess "pw-dump" [] ""
  value <-
    either
      (throwE . ("Could not decode pw-dump output: " <>))
      pure
      (JSON.eitherDecode $ BL8.pack output)
  either
    (throwE . ("Could not parse pw-dump output: " <>))
    pure
    (AesonTypes.parseEither parse value)
 where
  parse = JSON.withArray "PipeWire object list" $ \values -> do
    objects <- traverse object $ Vec.toList values
    pure
      PWGraph
        { graphNodes = Map.fromList [(nodeId node, node) | ObjectNode node <- objects]
        , graphPorts = Map.fromList [(portId port, port) | ObjectPort port <- objects]
        , graphLinks = Map.fromList [(linkId link, link) | ObjectLink link <- objects]
        }

  object = JSON.withObject "PipeWire object" $ \value -> do
    kind <- value .: "type"
    ident <- value .: "id"
    let props = value .: "info" >>= (.: "props")
    case kind :: String of
      "PipeWire:Interface:Node" -> do
        properties <- props
        ObjectNode <$> (PWNode ident <$> properties .:? "node.name" <*> properties .:? "application.name")
      "PipeWire:Interface:Port" -> do
        info <- value .: "info"
        properties <- info .: "props"
        ObjectPort
          <$> ( PWPort ident
                  <$> properties .: "node.id"
                  <*> properties .: "port.name"
                  <*> info .: "direction"
                  <*> properties .:? "audio.channel"
              )
      "PipeWire:Interface:Link" -> do
        properties <- props
        ObjectLink <$> (PWLink ident <$> properties .: "link.output.port" <*> properties .: "link.input.port")
      _ -> pure ObjectOther

readProcess :: FilePath -> [String] -> String -> ExceptT String IO String
readProcess cmd args input = do
  (code, stdout, stderr) <-
    ExceptT $
      tryJust @IOException (\err -> Just $ show err) $
        Sys.readProcessWithExitCode cmd args input
  when (code /= ExitSuccess) $
    throwE $ printf "%s failed with exit code %s: %s" cmd (show code) stderr
  pure stdout
