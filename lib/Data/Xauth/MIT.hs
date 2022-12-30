{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ApplicativeDo #-}

module Data.Xauth.MIT where

import Control.Applicative


import Data.List (nub,sort)
import Data.String (IsString, fromString)

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Attoparsec.ByteString (endOfInput, parseOnly)

import Data.Xauth.Parser
import System.Environment
import GHC.IO.Exception

import "mtl" Control.Monad.Except
import Control.Exception.Base

import Data.Xauth.ErrorMessage

type MitCookie = ByteString
type Display = ByteString
isMitMagic :: ByteString -> Bool
isMitMagic b = (b == ".") || (b == "MIT-MAGIC-COOKIE-1")

io2em :: IOException -> ErrorMessage
io2em =  fromString . displayException

onLeft :: (a -> b) ->  Either a c ->  Either b c
onLeft h = either (Left . h) Right

tryGetEnv :: String -> ExceptT ErrorMessage  IO String
tryGetEnv = ExceptT . (fmap (onLeft io2em) ) . try . getEnv

tryReadBinaryFile :: FilePath -> ExceptT ErrorMessage  IO ByteString
tryReadBinaryFile  = ExceptT . (fmap (onLeft io2em) ) . try . B.readFile


tryWithFile :: Display ->
               FilePath -> -- unresolved filepath, used in the error messages
               FilePath ->     --   resolved filepath
               ExceptT ErrorMessage IO MitCookie

tryWithFile dn unresolvedFilePath filePath = do
   b <- tryReadBinaryFile filePath
   ExceptT $ pure $  case parseOnly (xAuthParser <* endOfInput) b of

     Left  s      -> Left $ EM $ "The file " ++ unresolvedFilePath ++
                     "(resolved to " ++  filePath ++ ") is corrupted." ++
                     "The parse output: " ++ s

     Right xauths -> case filter ((dn==) . display ) xauths of

       [] -> Left $ EM $ "The file " ++ unresolvedFilePath ++
                    "(resolved to " ++  filePath ++ ") doesnot contain " ++
                    "any record for the display " ++ ( init $ tail $ show dn)

       x@uthsWithOurDisplay -> case filter ( isMitMagic . authName) x of
         [] -> Left $ EM $ "According to the file" ++ unresolvedFilePath ++
                      "(resolved to " ++ filePath ++ ")" ++
                      "the Xserver connected to the display :" ++
                      ( init $ tail $ show dn) ++
                      "permits the following authorization methods: " ++
                      ( show $ nub $ sort $ authName <$> x)
         xauth:_ -> Right $ authData xauth

em2s :: IO (Either ErrorMessage MitCookie) ->
        IO (Either String MitCookie)

em2s = fmap (onLeft unEM)


mit :: Display -> IO (Either String MitCookie)
mit dn = fmap (onLeft unEM) $  runExceptT $
  (ExceptT $ pure $ Left preambleMessage  ) <|>
  (tryGetEnv "XAUTHORITY" >>= tryWithFile dn "$XAUTHORITY") <|>
  do
    home <- tryGetEnv "HOME"
    tryWithFile dn "$HOME/.Xauthority" $ home ++ "/.Xauthority"

--
-- | The next function tries to figure out the MIT cookie
-- that corresponds to a given display
-- The argument is the char encoding of the display number.
--mit :: ByteString -> IO (Either String ByteString)
--mit displayNumber  = do
--  -- mxau <- lookupEnv "XAUTHORITY"
--  -- check if there is $XAUTHORITY set
--  -- lookup $XAUTHORITY :: IO (Maybe String)
--  -- if it is set check if the corresponding file exists
--  -- ** bad taste, since it can stop to exist meanwhile
--  -- ** we should trust to @try@.
--  -- if it exists check if there is a record with given display number in
--  --    $XAUTHORITY
--  -- if exists a record with given display number check if there is a
--  --    a record with MIT-COOKIE among all such records
--  -- if the previous step successful -- output the corresponding cookie
--  -- if all the previous steps fail, collect errors and continue
--  -- check if the environment variable $HOME is  set
--  -- if $HOME is set check if the file .Xauthority in $HOME exists
--  -- if it exists, check if a record with given display number exists
--  -- if such records exist, check if among them exists a record with
--  --    MIT-COOKIE
--  -- if exists output the corresponding cookie to the Right
--  -- if fail collect all errors to the Left.
--
--  -- if both previous passes fail
--  pure $ Right ""
