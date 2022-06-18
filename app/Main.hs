module Main where

import qualified Server (server)

main :: IO ()
main = Server.server
