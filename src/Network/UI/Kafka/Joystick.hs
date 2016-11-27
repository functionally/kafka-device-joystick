{-|
Module      :  Network.UI.Kafka.Joystick
Copyright   :  (c) 2016 Brian W Bush
License     :  MIT
Maintainer  :  Brian W Bush <consult@brianwbush.info>
Stability   :  Experimental
Portability :  Linux

Produce events for a Kafka topic from a Linux joystick, which must conform to the Linux Joystick API \<<https://www.kernel.org/doc/Documentation/input/joystick-api.txt>\>.
-}


{-# LANGUAGE RecordWildCards #-}


module Network.UI.Kafka.Joystick (
-- * Event handling
  joystickLoop
) where


import Control.Monad (guard)
import Data.ByteString.Lazy.Char8 (hGet)
import Data.Maybe (catMaybes)
import Network.Kafka (KafkaAddress, KafkaClientId)
import Network.Kafka.Protocol (TopicName)
import Network.UI.Kafka (ExitAction, LoopAction, Sensor, producerLoop)
import Network.UI.Kafka.Types (Button(..), Event(..), Toggle(..))
import System.Hardware.Linux.Joystick (Joystick(..), byteLength, interpretJoystick, maxValue)
import System.IO (IOMode(ReadMode), hClose, openFile)


-- | Produce events for a Kafka topic from a Linux Joystick.
joystickLoop :: FilePath                    -- ^ The path to the joystick device, e.g. "\/dev\/input\/js0".
             -> KafkaClientId               -- ^ A Kafka client identifier for the producer.
             -> KafkaAddress                -- ^ The address of the Kafka broker.
             -> TopicName                   -- ^ The Kafka topic name.
             -> Sensor                      -- ^ The name of the sensor producing events.
             -> IO (ExitAction, LoopAction) -- ^ Action to create the exit and loop actions.
joystickLoop path client address topic sensor =
  do
    joystick <- openFile path ReadMode
    (exit, loop) <-
      producerLoop client address topic sensor
        $ translate
        . interpretJoystick
        <$> hGet joystick byteLength
    return
      (
        do
          exit
          hClose joystick
      , loop
      )


-- | Translate a Linux Joystick event into events for Kafka.
translate :: Joystick -- ^ The joystick event.
          -> [Event]  -- ^ The corresponding events for Kafka.
translate Joystick{..} =
  catMaybes
    [
      do
        guard button
        return
          $ ButtonEvent (IndexButton number, if value == 0 then Up else Down)
    , do
        guard axis
        return
          $ AnalogEvent number (fromIntegral value / fromIntegral maxValue)
    ]
