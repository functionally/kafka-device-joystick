{-|
Module      :  Main
Copyright   :  (c) 2016 Brian W Bush
License     :  MIT
Maintainer  :  Brian W Bush <consult@brianwbush.info>
Stability   :  Experimental
Portability :  Linux

Producer of events from a Linux joystick.
-}


{-# LANGUAGE OverloadedStrings #-}


module Main (
-- * Main entry
  main
) where


import Data.String (IsString(fromString))
import Network.UI.Kafka.Joystick (joystickLoop)
import System.Environment (getArgs)


-- The main action.
main :: IO ()
main =
  do
    args <- getArgs
    case args of
      [device, client, host, port, topic, sensor] ->
        do
          putStrLn $ "Device:        " ++ device
          putStrLn $ "Kafka client:  " ++ client
          putStrLn $ "Kafka address: (" ++ host ++ "," ++ port ++ ")"
          putStrLn $ "Kafka topic:   " ++ topic
          putStrLn $ "Sensor name:   " ++ sensor
          (_, loop) <-
            joystickLoop
              device
              (fromString client)
              (fromString host, toEnum $ read port)
              (fromString topic)
              sensor
          result <- loop
          either print return result
      _ -> putStrLn "USAGE: kafka-device-joystick client host port topic sensor"
