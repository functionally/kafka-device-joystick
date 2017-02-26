{-|
Module      :  $Header$
Copyright   :  (c) 2016-17 Brian W Bush
License     :  MIT
Maintainer  :  Brian W Bush <consult@brianwbush.info>
Stability   :  Experimental
Portability :  Linux

Produce events for a Kafka topic from a Linux joystick.
-}


module Main (
-- * Main entry
  main
) where


import Data.Yaml.Config (loadYamlSettingsArgs, useEnv)
import Network.UI.Kafka (TopicConnection(TopicConnection))
import System.Environment (getArgs)

import qualified Network.UI.Kafka.Joystick as Raw (joystickLoop)
import qualified Network.UI.Kafka.Joystick.Interpretation as Interpreted (joystickLoop)


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
            Raw.joystickLoop
              device
              (TopicConnection client (host, read port) topic)
              sensor
          result <- loop
          either print return result
      [_] ->
        do
          interpretation <- loadYamlSettingsArgs [] useEnv
          (_, loop) <- Interpreted.joystickLoop interpretation
          either print return =<< loop
      _ ->
        do
          putStrLn "USAGE: kafka-device-joystick device client host port topic sensor"
          putStrLn "or"
          putStrLn "USAGE: kafka-device-joystick interpretation.yaml"
