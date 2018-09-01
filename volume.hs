{-# LANGUAGE OverloadedStrings #-}
import System.Process.Typed
import System.IO
import System.Exit
import Data.ByteString.Lazy ( ByteString
                            , splitWith)
import qualified Data.ByteString.Lazy.Char8 as C

-- import 
main :: IO()
main = getAmixerInfo >>=
       \s -> case s of
         Left ams -> printVolume ams
         Right (errno, str) -> print str
                               >> exitWith (ExitFailure errno)
       where printVolume s = case getVolume s of
                               Just v -> print v
                               Nothing -> print 'M'
                   

deviceInfo :: String
deviceInfo = "Master,0"

-- Volume percentage or Mute
getVolume :: ByteString -> Maybe ByteString
getVolume s = let sl = C.splitWith (\c -> (c == '[') || (c == ']')) s in
                if (length sl == 9)
                then if (sl !! 3 == "on")
                     then Just $ sl !! 1
                     else Nothing
                else error "Parse Error in Amixer Output"

amixerExe :: ProcessConfig () () ()
amixerExe = proc "amixer" ["sget", deviceInfo]

getAmixerInfo :: IO (Either ByteString (Int, ByteString))
-- ^ gets info from amixer program
getAmixerInfo = readProcess amixerExe >>=
                \(exitCode, out, err) -> case exitCode of
                  ExitSuccess -> return $ Left out
                  ExitFailure i -> return $ Right (i, err)
