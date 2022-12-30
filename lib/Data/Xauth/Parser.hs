
module Data.Xauth.Parser ( xAuthParser
                         , Xauth
                         , familyValue
                         , hostAddress
                         , display
                         , authName
                         , authData, bs2i
                         , anyInt16
                         , extractByteString
                         ) where

import Prelude hiding (take)
import Data.Bits
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

data Xauth = Xauth { familyValue :: Int
                   , hostAddress :: ByteString
                   , display     :: ByteString
                   , authName    :: ByteString
                   , authData    :: ByteString
                   } deriving (Show, Read)

-- | Convert a @ByteString@ into an integer using BigEndian encoding.
bs2i :: ByteString -> Integer
bs2i = B.foldl' (\acc next -> (acc `shiftL` 8) .|. (toInteger next) ) 0

-- | Convert a pair of bytes into an @Int@ value using BigEndian encoding.
anyInt16 :: Parser Int
anyInt16 = fromInteger . bs2i <$> take 2

-- | The following parser reads two first bytes to determine
-- the length of the following ByteString. It discards the length
-- and yelds the ByteString.
extractByteString :: Parser ByteString
extractByteString  = take =<< anyInt16

xAuthRecord :: Parser Xauth
xAuthRecord = Xauth <$> anyInt16
                    <*> extractByteString -- hostAddress
                    <*> extractByteString -- display
                    <*> extractByteString -- authName
                    <*> extractByteString -- authData



-- | The following parser parses the content of the @.Xauthority@ file.
-- It should be used with @parseOnly@ since with @parse@ it returns
-- @Partial _@.
xAuthParser :: Parser [Xauth]
xAuthParser = many' xAuthRecord
