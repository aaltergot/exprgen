{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module App (app) where

import           ExprGen
import           System.Environment (getArgs)
import           System.Console.GetOpt
import           System.Exit
import           System.Random
import qualified System.IO as IO (withFile)
import           System.IO hiding (withFile)
import           Control.Monad
import           Control.Monad.Except
import           Control.Exception.Enclosed
import           Data.Text (Text, pack, unpack, append)
import           Control.Monad.State
import           Text.Read (readEither)

data Exception = Exception Text 
               | ConfigReadException Text
               | ConfigParseException Text
               | NumArgNotFound
               | MalformedNum Text
               | ModArgNotFound
               | MalformedMod Text
               | MalformedSeed Text
               deriving (Show)

class Throwable a where throw :: Monad m => a -> ExceptT Exception m b
instance Throwable Text where throw = throwError . Exception
instance Throwable String where throw = throwError . Exception . pack

asText :: Show a => a -> Text
asText = pack . show

(+++) :: Text -> Text -> Text
(+++) = append

--conditional append
(++?) :: Text -> Bool -> Text -> Text
(++?) t b r = t +++ if b then r else ""

rethrowBaseExceptionAs :: Monad m => (Text -> Exception) -> Exception -> ExceptT Exception m a
rethrowBaseExceptionAs f (Exception msg) = throwError $ f msg
rethrowBaseExceptionAs _ e = throwError e

data Flag = Help
          | Verbose
          | File String
          | Num String
          | Mod String
          | Seed String
          deriving (Show, Eq, Ord)

flags = [ Option ['h'] ["help"] (NoArg Help) "Print this help message"
        , Option ['v'] ["verbose"] (NoArg Verbose) "Verbose output"
        , Option ['f'] ["file"] (ReqArg File "<PATH>") "Input file path"
        , Option ['n'] ["num"] (ReqArg Num "<N>") "Number of terms"
        , Option ['m'] ["mod"] (ReqArg Mod "<M>") "Term module"
        , Option ['s'] ["seed"] (ReqArg Seed "<S>") "Random generator seed"
        ]

usageHeader :: String
usageHeader = "Usage: exprgen --file=/path/to/file [--seed=<S>]\n" ++
              "   or: exprgen --num=<N> --mod=<M> [--seed=<S>]\n\n" ++
              "Notes: <N>,<M> - integers above zero; <S> - arbitrary integer\n" ++
              "       \"--file\" option has precedence over \"--num\"+\"--mod\"\n" ++
              "       Input file contains <N> on first line, <M> on second\n" 

translateException :: Exception -> Bool -> Text
translateException (Exception msg) _ =
  "Execution failure: " +++ msg
translateException (ConfigReadException msg) v =
  "Unable to read input file." ++? v $ msg
translateException (ConfigParseException msg) v =
  "Incorrect input file. See \"--help\" page." ++? v $ asReason msg
translateException NumArgNotFound _ = 
  "\"--num\" is required"
translateException (MalformedNum msg) v = 
  "Malformed argument for option \"--num\"" ++? v $ asReason msg
translateException ModArgNotFound _  = 
  "\"--mod\" is required"
translateException (MalformedMod msg) v =
  "Malformed argument for option \"--mod\"" ++? v $ asReason msg
translateException (MalformedSeed msg) v =
  "Malformed argument for option \"--seed\"" ++? v $ asReason msg

asReason :: Text -> Text
asReason = append "\nReason: "


withFile :: FilePath 
         -> IOMode 
         -> (Handle -> ExceptT Exception IO r) 
         -> ExceptT Exception IO r
withFile path mode h =
  catchAny 
    (ExceptT $ IO.withFile path mode $ \h1 -> runExceptT $  h h1) 
    (throw . asText)

readInt :: String -> Except Exception Int
readInt s = case readEither s of 
              Left msg -> throw msg
              Right i -> return i

readIntAboveZero :: String -> Except Exception Int
readIntAboveZero s = readInt s >>= \i -> if i > 0 then return i else throw (s ++ " < 1")

readNum :: String -> Except Exception Int
readNum s = readIntAboveZero s `catchError` rethrowBaseExceptionAs MalformedNum

readMod :: String -> Except Exception Int
readMod s = readIntAboveZero s `catchError` rethrowBaseExceptionAs MalformedMod

makeConfig :: String -> String -> Except Exception ExprGenConfig
makeConfig nStr mStr = do
  n <- readNum nStr
  m <- readMod mStr
  return $ ExprGenConfig n m

readConfigFile :: FilePath -> ExceptT Exception IO ExprGenConfig
readConfigFile path = flip catchError (rethrowBaseExceptionAs ConfigReadException) $
  withFile path ReadMode $ \h -> do
    (nStr:mStr:_) <- catchAny 
      (liftIO $ sequence $ [hGetLine, hGetLine] <*> [h]) 
      (throwError . ConfigParseException . pack . show)
    ExceptT $ return $ runExcept $ makeConfig nStr mStr

lookupFilePath :: [Flag] -> Maybe FilePath
lookupFilePath [] = Nothing
lookupFilePath (File s : fs) = Just s
lookupFilePath (_:fs) = lookupFilePath fs

lookupNum :: [Flag] -> Except Exception Int
lookupNum [] = throwError NumArgNotFound
lookupNum (Num s : _) = readNum s
lookupNum (_:fs) = lookupNum fs

lookupMod :: [Flag] -> Except Exception Int
lookupMod [] = throwError ModArgNotFound
lookupMod (Mod s : _) = readMod s
lookupMod (_:fs) = lookupMod fs

lookupSeed :: [Flag] -> Except Exception (Maybe Int)
lookupSeed [] = return Nothing
lookupSeed (Seed s : _) = Just <$> readInt s `catchError` rethrowBaseExceptionAs MalformedSeed
lookupSeed (_:fs) = lookupSeed fs

extractConfig :: [Flag] -> Except Exception ExprGenConfig
extractConfig fs = do
  n <- lookupNum fs
  m <- lookupMod fs
  return $ ExprGenConfig n m 

exceptIO :: Except e a -> ExceptT e IO a
exceptIO = ExceptT . return . runExcept

app :: IO ()
app = do
  argv <- getArgs
  case getOpt Permute flags argv of
    (_, _, errs) | not $ null errs -> putStrLn (concat errs ++ usageInfo usageHeader flags)
    (args, values, []) -> do
      let verbose = Verbose `elem` args
      unless (null values) (putStrLn $ "Unrecognized arguments: " ++ show values)
      result <- runExceptT $ do
        config <- 
          case lookupFilePath args of
            Just path -> do 
              liftIO $ putStrLn $ "Reading input file: " ++ path
              readConfigFile path
            Nothing -> exceptIO $ extractConfig args
        gen <- fmap mkStdGen <$> exceptIO (lookupSeed args) >>= (liftIO . maybe newStdGen return)
        when verbose $ liftIO $ putStrLn $ "Generator: " ++ show gen
        return $ evalState (genExpr config) gen
      putStrLn $ 
        case result of 
          Right expr -> unpack expr
          Left ex -> unpack $ translateException ex verbose +++
                       if not verbose then "\nTry \"--verbose\" for more info." else ""
