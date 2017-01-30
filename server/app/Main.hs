module Main where

import           Data.Monoid
import qualified Network.Wai.Handler.Warp as Warp

import           Lib                      (app)

main :: IO ()
main = do
  let port = 3000
  putStrLn $ "Listening to port " <> show port <> " ..."
  Warp.run port app
