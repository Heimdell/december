
import AST
import Parser
import Text.Parser.Yard.Run (parseFile)

import System.Environment

main :: IO ()
main = do
  getArgs >>= \case
    file : _ -> do
      res <- parseFile prog file
      either print print res

    _ -> do
      putStrLn "USAGE: dec <file>"