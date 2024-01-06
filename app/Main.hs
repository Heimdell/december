
import Phase.Raw
import Pass.Parsing
import Pass.ScopeCheck
import Pass.TypeCheck (runTC, typeCheckAndDump)
import Text.Parser.Yard.Run (parseFile)

import System.Environment

main :: IO ()
main = do
  getArgs >>= \case
    file : _ -> do
      res <- parseFile prog file
      case res of
        Left err -> do
          print err

        Right prog -> do
          case scopeCheck (Context mempty mempty) prog of
            Left scopeErr -> do
              print scopeErr

            Right prog -> do
              print prog

    _ -> do
      putStrLn "USAGE: dec <file>"