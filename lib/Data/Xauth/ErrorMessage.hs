
module Data.Xauth.ErrorMessage  where

import Data.String

newtype ErrorMessage = EM { unEM :: String }

instance IsString ErrorMessage where
  fromString = EM

instance Semigroup ErrorMessage where
  (EM s) <> (EM t )
    | s==""     = EM t
    | t==""     = EM s
    | otherwise = EM ( s ++ "\n" ++ t)

instance Monoid ErrorMessage where
   mempty = EM ""


preambleMessage :: ErrorMessage
preambleMessage = EM $
 "The authorization with X11 server using MIT-MAGIC-COOKIE-1 method failed.\n" ++
 "To authorize with this method either the file specified in the \n" ++
 "environment variable $XAUTHORITY or the file $HOME/.Xauthority \n" ++
 "should contain a record with MIT-MAGIC-COOKIE-1 that corresponds to your\n" ++
 "display.\n\n The specific reasons of failure are: \n"



