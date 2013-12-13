{-# LANGUAGE OverloadedStrings #-}
module LiveReload where
import Control.Concurrent (MVar, swapMVar, readMVar, withMVar, newMVar)
import Control.Monad
import Control.Monad.Trans
import Data.Aeson
import Data.Typeable
import qualified Data.Text as T
import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Unsafe
import System.Posix.Signals

data LiveReloadCommand
  = Hello
    { serverName :: T.Text
    , protocols :: [T.Text]
    }
  | Url
    { url :: T.Text
    }
  | Reload
    { path :: T.Text
    , liveCSS :: Bool
    }
  | Alert
    { message :: T.Text
    }

instance ToJSON LiveReloadCommand where
  toJSON x = object $ case x of
    (Hello hName hProtocols) ->
      [ "command" .= ("hello" :: T.Text)
      , "protocols" .= hProtocols
      , "name" .= hName
      ]
    (Url uUrl) ->
      [ "command" .= ("url" :: T.Text)
      , "url" .= uUrl
      ]
    (Reload rPath rLiveCSS) ->
      [ "command" .= ("reload" :: T.Text)
      , "path" .= rPath
      , "liveCSS" .= rLiveCSS
      ]
    (Alert aMessage) ->
      [ "command" .= ("alert" :: T.Text)
      , "message" .= aMessage
      ]

data Hotswap a = Hotswap
  { interpreterSetup :: Interpreter () -> Interpreter ()
  , reloadModules :: [String]
  , loadFromModule :: [String]
  , reloadExpression :: String
  , errorHandler :: InterpreterError -> IO ()
  , updateHandler :: a -> a -> IO ()
  , currentValue :: MVar a
  }

type InterpreterLock = MVar ()

newHotswap :: (Typeable a) => a -> [ModuleName] -> [ModuleName] -> String -> IO (Hotswap a)
newHotswap x modules mainModule expr = do
  curr <- newMVar x
  return $ Hotswap
    { currentValue = curr
    , reloadExpression = expr
    , loadFromModule = mainModule
    , reloadModules = modules
    , interpreterSetup = id
    , errorHandler = print
    , updateHandler = \_ _ -> return ()
    }

interpWithDb = unsafeRunInterpreterWithArgs ["-package-db=/Users/ian.duncan/Code/submarine/ui/.cabal-sandbox/x86_64-osx-ghc-7.6.3-packages.conf.d"]

hotswap :: (Typeable a) => InterpreterLock -> Hotswap a -> IO ()
hotswap lock r = do
  putStrLn "Acquiring interpreter lock"
  withMVar lock $ \_ -> do
    oldSigInt <- installHandler keyboardSignal Default Nothing
    oldSigTerm <- installHandler softwareTermination Default Nothing
    interpreted <- interpWithDb $ do
      interpreterSetup r $ loadModules $ reloadModules r
      setTopLevelModules $ loadFromModule r
      result <- interpret (reloadExpression r) infer
      liftIO $ putStrLn "swapping"
      old <- liftIO $ swapMVar (currentValue r) result
      return (old, result)
    installHandler keyboardSignal oldSigInt Nothing
    installHandler softwareTermination oldSigTerm Nothing
    case interpreted of
      Left err -> errorHandler r $ err
      Right (old, new) -> (updateHandler r) old new

getCurrentValue :: Hotswap a -> IO a
getCurrentValue = readMVar . currentValue
