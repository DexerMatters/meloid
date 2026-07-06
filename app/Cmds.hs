module Cmds (
  applyCommand,
) where

import Brick.Types
import Data.Functor (($>))
import Data.List (find)
import Lens.Micro.Mtl
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
      { cmdName = "test"
      , cmdDescription = "Test command"
      , cmdStages =
          InputStage "Count" (const $ Right ()) $
            InputStage "Name" (const $ Right ()) $
              ExecutionStage (const . pure . Right $ "Done")
      }
  ]

-- | Apply a command
applyCommand :: String -> EventM (MName St) St ()
applyCommand command =
  case find (\c -> cmdName c == command) commands of
    Just (Command _ _ stages) -> zoom stCommandStages $ do
      cpsStages .= Just stages
      cpsParameters .= []
      cpsCommand .= command
    Nothing -> inlineOutput Error $ "Unknown command: " <> command
