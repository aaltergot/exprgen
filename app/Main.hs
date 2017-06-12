{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Exception (Exception, displayException)
import Control.Exception.Safe (throwM, SomeException, handleAny)
import Control.Monad (when)
import Control.Monad.Except (MonadError(throwError), runExcept, Except)
import Control.Monad.Random (evalRandT)
import Data.Monoid ((<>))
import Data.Text (unpack)
import Data.Typeable (Typeable)
import Options.Applicative hiding (str, value)
import Text.Read (readEither)
import System.Exit (exitFailure)
import System.IO (IOMode (ReadMode), hGetLine, hPutStrLn, stderr, withFile)
import System.Random (newStdGen, mkStdGen)

import ExprGen.Expr (ExprException(..))
import ExprGen.Gen (Config(..), Result(..), generateExpr)

-- * Reading

newtype AppException = AppException String
  deriving (Show, Typeable)

instance Exception AppException where
  displayException (AppException msg) = msg

readLiteralsNum :: (MonadError AppException m) => String -> m Int
readLiteralsNum s = case readEither s of
  Left _ -> throwError $ AppException ("Invalid literals number: " <> s)
  Right i | i > 0 -> return i
          | otherwise -> throwError $ AppException "The number of literals should be greater than 0"

readValueMod :: (MonadError AppException m) => String -> m Int
readValueMod s = case readEither s of
  Left _ -> throwError $ AppException ("Invalid values module: " <> s)
  Right i | i > 0 -> return i
          | otherwise -> throwError $ AppException "The values modulus should be greater than 0"

readSeed :: (MonadError AppException m) => String -> m Int
readSeed s = case readEither s of
  Left _ -> throwError $ AppException ("Invalid seed value: " <> s)
  Right i -> return i

makeConfig :: (MonadError AppException m) => String -> String -> m Config
makeConfig nStr mStr = do
  n <- readLiteralsNum nStr
  m <- readValueMod mStr
  return $ Config n m

readInputFile :: FilePath -> IO Config
readInputFile path = do
  (nStr:mStr:_) <- handleAny exceptionHandler $
    withFile path ReadMode (\h -> sequence $ [hGetLine, hGetLine] <*> [h])
  case runExcept $ makeConfig nStr mStr of
    Left e -> throwM e
    Right c -> return c
  where
    exceptionHandler :: SomeException -> IO a
    exceptionHandler e =
      throwM $ AppException ("Failed to read input file (Reason: " <> displayException e <> ")")


-- * CLI stuff

data AppOpts = AppOpts
 { configOpts :: ConfigOpts
 , seed :: Maybe Int
 , verbose :: Bool
 } deriving (Show)

data ConfigOpts =
  CliConfig { literalsNum :: Int, valueMod :: Int }
  | InputFile FilePath
  deriving (Show)

toReaderM :: (String -> Except AppException a) -> ReadM a
toReaderM f = eitherReader $ \s -> case runExcept (f s) of
  Left e -> Left $ displayException e
  Right v -> Right v

appOptsParser :: Parser AppOpts
appOptsParser = AppOpts <$> configOptsParser <*> seedParser <*> verboseParser
  where
    configOptsParser :: Parser ConfigOpts
    configOptsParser = cliConfigParser <|> inputFileParser

    cliConfigParser :: Parser ConfigOpts
    cliConfigParser = CliConfig
      <$> option (toReaderM readLiteralsNum)
        ( long "num"
        <> short 'n'
        <> metavar "<N>"
        <> help "Literals and expression value modulus"
        )
      <*> option (toReaderM readValueMod)
        ( long "mod"
        <> short 'm'
        <> metavar "<M>"
        <> help "Number of literals"
        )

    inputFileParser :: Parser ConfigOpts
    inputFileParser = InputFile <$> strOption
      ( long "file"
      <> short 'f'
      <> metavar "<FILE>"
      <> help "Input file path"
      )

    seedParser :: Parser (Maybe Int)
    seedParser = optional $ option (toReaderM readSeed)
      ( long "seed"
      <> short 's'
      <> metavar "<S>"
      <> help "Random generator seed"
      )

    verboseParser :: Parser Bool
    verboseParser = switch (long "verbose" <> short 'v' <> help "Verbose output")

appOptsInfo :: ParserInfo AppOpts
appOptsInfo = info (appOptsParser <**> helper)
  ( fullDesc
  <> progDesc
    ( "<N>,<M> - integers above zero; <S> - arbitrary integer; "
    <> "<FILE> contains <N> on first line, <M> on second."
    )
  <> header
    ( "Generate random arithmetic expression, that have <N> literals, each literal is "
    <> "integer and does not exceed <M> in absolute value. Expression evaluation result "
    <> "should not exceed <M> in absolute value as well."
    )
  <> failureCode 1
  )


main :: IO ()
main = do
  AppOpts{..} <- execParser appOptsInfo
  handleAny exceptionHandler $ do
    config <- case configOpts of
      CliConfig n m -> return $ Config n m
      InputFile fp -> readInputFile fp
    gen <- case seed of
      Nothing -> newStdGen
      Just s -> return $ mkStdGen s
    when verbose $ putStrLn $ "Generator: " ++ show gen
    Result{..} <- case runExcept (evalRandT (generateExpr config) gen) of
      Right r -> return r
      Left InvalidOperationsList -> throwM $ AppException "UNREACHABLE: Invalid operations list"
    when verbose $ do
      putStrLn $ "Expression: " <> show expression
      putStrLn $ "Operations: " <> show operations
      putStrLn $ "Value: " <> show value
    putStrLn $ unpack formatted
  where
    exceptionHandler :: SomeException -> IO ()
    exceptionHandler e = do
      hPutStrLn stderr (displayException e)
      exitFailure
