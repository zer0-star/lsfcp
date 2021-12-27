{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import RIO
import RIO.Process

import App
import Run

import Options.Applicative.Simple
import qualified Paths_lsfcp

main :: IO ()
main = do
  (options, ()) <-
    simpleOptions
      $(simpleVersion Paths_lsfcp.version)
      "Header for command line arguments"
      "Program description, also for command line arguments"
      ( Options <$> strArgument (action "filenames")
          <*> pure ""
      )
      empty
  lo <- logOptionsHandle stderr True
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app =
          App
            { appLogFunc = lf
            , appProcessContext = pc
            , appOptions = options
            }
     in runRIO app run