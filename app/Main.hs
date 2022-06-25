module Main where

import qualified Server (server, testPar)

main :: IO ()
main = Server.server
