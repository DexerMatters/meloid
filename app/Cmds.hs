module Cmds (
  applyCommand,
) where

import Brick.Types
import Control.Monad (void)
import Data.Functor (($>))
import Data.List (find)
import Lens.Micro.Mtl
import Network.MPD qualified as MPD
import Text.Read (readMaybe)
import Types

commands :: [Command]
commands =
  [ Command
      { cmdName = ":q"
      , cmdDescription = "Quit the application"
      , cmdStages =
          ExecutionStage (const $ sendRequest SignalQuit $> Right "Bye!")
      }
  , Command
      { cmdName = "h"
      , cmdDescription = "Open help"
      , cmdStages =
          -- TODO: This is a stub.
          -- It should display a dialog where the commands are listed
          ExecutionStage (const . pure . Left $ "Unimplemented")
      }
  , Command
      { cmdName = ":play"
      , cmdDescription = "Resume playback"
      , cmdStages = ExecutionStage $ const $ do
          stPlaying . psPaused .= False
          sendRequest $ MPDOperation [MPD.pause False]
          pure $ Right "Playback resumed"
      }
  , Command
      { cmdName = ":pause"
      , cmdDescription = "Pause playback"
      , cmdStages = ExecutionStage $ const $ do
          stPlaying . psPaused .= True
          sendRequest $ MPDOperation [MPD.pause True]
          pure $ Right "Playback paused"
      }
  , Command
      { cmdName = ":toggle"
      , cmdDescription = "Toggle play/pause"
      , cmdStages = ExecutionStage $ const $ do
          stPlaying . psPaused %= not
          paused <- use $ stPlaying . psPaused
          sendRequest $ MPDOperation [MPD.pause paused]
          pure $ Right (if paused then "Playback paused" else "Playback resumed")
      }
  , Command
      { cmdName = ":next"
      , cmdDescription = "Skip to next song"
      , cmdStages = ExecutionStage $ const $ do
          stPlaying . psPaused .= False
          sendRequest $ MPDOperation [MPD.next]
          pure $ Right "Skipped to next song"
      }
  , Command
      { cmdName = ":prev"
      , cmdDescription = "Return to previous song"
      , cmdStages = ExecutionStage $ const $ do
          stPlaying . psPaused .= False
          sendRequest $ MPDOperation [MPD.previous]
          pure $ Right "Returned to previous song"
      }
  , Command
      { cmdName = ":shuffle"
      , cmdDescription = "Shuffle current queue"
      , cmdStages = ExecutionStage $ const $ do
          sendRequest $ MPDOperation [MPD.shuffle Nothing]
          sendRequest SignalCurrentQueue
          pure $ Right "Shuffled current queue"
      }
  , Command
      { cmdName = ":clear"
      , cmdDescription = "Clear current queue"
      , cmdStages = ExecutionStage $ const $ do
          sendRequest $ MPDOperation [MPD.clear]
          sendRequest SignalCurrentQueue
          pure $ Right "Cleared queue"
      }
  , Command
      { cmdName = ":volume"
      , cmdDescription = "Set or adjust volume (e.g. 50, +10, -5)"
      , cmdStages = InputStage "Volume Level" validateVolume (ExecutionStage runVolumeChange)
      }
  , Command
      { cmdName = ":vol"
      , cmdDescription = "Alias for :volume"
      , cmdStages = InputStage "Volume Level" validateVolume (ExecutionStage runVolumeChange)
      }
  , Command
      { cmdName = "test"
      , cmdDescription = "Test command"
      , cmdStages =
          InputStage "Count" (const $ Right ()) $
            InputStage "Name" (const $ Right ()) $
              ExecutionStage (const . pure . Right $ "Done")
      }
  ]

validateVolume :: String -> Either String ()
validateVolume s = case s of
  ('+' : num) -> checkNum num
  ('-' : num) -> checkNum num
  num         -> checkNum num
 where
  checkNum x = case readMaybe x :: Maybe Int of
    Just _  -> Right ()
    Nothing -> Left "Must be a number (e.g. 50, +5, -10)"

runVolumeChange :: [String] -> EventM (MName St) St (Either String String)
runVolumeChange [] = pure $ Left "No volume level specified"
runVolumeChange (valStr : _) = do
  currentVol <- use $ stConfig . csVolume
  case valStr of
    ('+' : (readMaybe -> Just (diff :: Int))) -> do
      let newVol = max 0 (min 100 (fromIntegral currentVol + diff))
      setVol newVol
    ('-' : (readMaybe -> Just (diff :: Int))) -> do
      let newVol = max 0 (min 100 (fromIntegral currentVol - diff))
      setVol newVol
    (readMaybe -> Just (val :: Int)) -> do
      let newVol = max 0 (min 100 val)
      setVol newVol
    _ -> pure $ Left "Invalid volume format"
 where
  setVol nv = do
    stConfig . csVolume .= fromIntegral nv
    sendRequest $ MPDOperation [MPD.setVolume (fromIntegral nv)]
    pure $ Right $ "Volume set to " ++ show nv ++ "%"

-- | Apply a command
applyCommand :: String -> EventM (MName St) St ()
applyCommand command =
  case find (\c -> cmdName c == command) commands of
    Just (Command _ _ stages) -> zoom stCommandStages $ do
      cpsStages .= Just stages
      cpsParameters .= []
      cpsCommand .= command
    Nothing -> inlineOutput Error $ "Unknown command: " <> command
