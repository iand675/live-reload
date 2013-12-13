{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main where
import qualified Data.Aeson as JSON (encode)
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (fromException, handle)
import Control.Monad (forM_, forever)
import Control.Concurrent (forkIO, killThread, MVar, newMVar, newEmptyMVar, modifyMVar_, readMVar, putMVar, takeMVar)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Unsafe
import qualified Network.WebSockets as WS
import qualified Network.Wai as WAI
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.Wai.Application.Static as Static
import Data.FileEmbed (embedDir)
import qualified Language.Haskell.Interpreter as I
import qualified System.FSNotify as FS
import qualified System.FSNotify.Devel as FS
import System.Posix.Signals
import LiveReload
import qualified Server

data WatcherState = WatcherState
  { connections :: [WS.Connection]
  }

hotswappableApp :: Hotswap WAI.Application -> WAI.Application
hotswappableApp mApp req = do
  case WAI.pathInfo req of
    ["livereload.js"] -> (Static.staticApp $ Static.embeddedSettings $(embedDir "assets")) req
    _ -> do
      app <- liftIO $ getCurrentValue mApp
      app req

main :: IO ()
main = do
  runReloadServer
  putStrLn "Shutting down"

runReloadServer :: IO ()
runReloadServer = do
  putStrLn "Starting server"
  ilock <- newMVar ()
  ws <- newMVar $ WatcherState []
  hotswapServer <- newHotswap Server.pages ["Server"] ["Server"] "pages"
  let serverApp = hotswapServer { interpreterSetup = \s -> (liftIO $ putStrLn "updating interpreter settings") >> set [searchPath := ["src"]] >> s}
  FS.withManager $ \man -> do
    FS.treeExtAny man "./src" "hs" (const $ hotswap ilock serverApp)
    putStrLn "Warping"
    Warp.runSettings Warp.defaultSettings
      { Warp.settingsPort = 35729
      , Warp.settingsIntercept = WaiWS.intercept $ application ws
      } $ hotswappableApp serverApp

application :: MVar WatcherState -> WS.ServerApp
application watchState pending = do
  putStrLn "Accepting WS request"
  conn <- WS.acceptRequest pending
  modifyMVar_ watchState (\ws -> return $ ws { connections = conn : connections ws })
  WS.sendTextData conn $ JSON.encode $ Hello "reloader" ["http://livereload.com/protocols/official-7"]
  forever (WS.receiveData conn :: IO T.Text)


