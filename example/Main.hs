module Main where

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Network.Wai.Dispatch
import Application
import Routes

main = do
	putStrLn "Running..."
	run 3000 (logStdoutDev $ dispatch on404 routes)
