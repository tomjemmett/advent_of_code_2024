module Main where

import Configuration.Dotenv
import Data.ByteString.Internal (packChars)
import Data.ByteString.Lazy.Char8 (unpack)
import Days
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status (statusCode)
import System.Environment (getArgs, getEnv)
import Text.Printf (printf)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then mapM_ runDay [1 .. 25]
    else runDay (read $ args !! 0)

downloadInput :: Int -> IO ()
downloadInput day = do
  manager <- newManager tlsManagerSettings

  let url = printf "https://adventofcode.com/2024/day/%d/input" day
  request' <- parseRequest url

  loadFile defaultConfig
  session <- getEnv "SESSION_ID"
  let sessionCookie = "session=" ++ session

  let request = request' {requestHeaders = (hCookie, packChars sessionCookie) : requestHeaders request'}

  response <- httpLbs request manager

  case statusCode $ responseStatus response of
    200 -> do
      let filePath = printf "inputs/day%02d.txt" day
          content = unpack $ responseBody response
      writeFile filePath content

      putStrLn $ printf "Downloaded day %d" day
    _ -> do
      error $ "error downloading input: " ++ show (responseStatus response)