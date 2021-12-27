module Run where

import RIO

import App
import Lsfcp.Parser.Expr

import qualified Text.Megaparsec as P
import Text.Pretty.Simple

run :: RIO App ()
run = do
  Options{..} <- view optionsL
  logInfo $ "Parsing: " <> fromString inputFile
  input <- readFileUtf8 inputFile
  case P.parse (program <* P.eof) inputFile input of
    Left err -> do
      logError $ "Parse error: " <> fromString (P.errorBundlePretty err)
      exitFailure
    Right prog -> do
      -- logInfo $ displayShow prog
      pPrintDarkBg prog
