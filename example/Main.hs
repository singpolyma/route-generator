module Main where

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Network.Wai.Dispatch
import Application
import Routes

-- Normally you wouldn't do this with pure values
-- The arguments to routes are indended for things from IO that cannot
-- be global like this
something = "Woo, pass this around because we can!"

main = do
	putStrLn "Running..."
	run 3000 (logStdoutDev $ dispatch on404 $ routes something)
