{-|
Module      :  $Header$
Copyright   :  (c) 2016-19 Brian W Bush
License     :  MIT
Maintainer  :  Brian W Bush <code@functionally.io>
Stability   :  Production
Portability :  Linux

Produce interpreted events for a Kafka topic from a Linux joystick, which must conform to the Linux Joystick API \<<https://www.kernel.org/doc/Documentation/input/joystick-api.txt>\>.
-}


{-# LANGUAGE RecordWildCards #-}


module Network.UI.Kafka.Joystick.Interpretation (
-- * Event handling
  joystickLoop
) where


import Data.ByteString.Lazy.Char8 (hGet)
import Network.UI.Kafka (ExitAction, LoopAction)
import Network.UI.Kafka.Interpretation (Interpretation(..), interpretationLoop)
import System.Hardware.Linux.Joystick (Joystick(..), byteLength, interpretJoystick, maxValue)
import System.IO (IOMode(ReadMode), hClose, openFile)


-- | Interpret events from a Linux SpaceNav.
joystickLoop :: Interpretation FilePath Double -- ^ Instructions for interpretation.
             -> IO (ExitAction, LoopAction)    -- ^ Action to create the exit and loop actions.
joystickLoop interpretation@TrackInterpretation{..} =
  do
    let
      analogHandler (Joystick _ setting number False True  _) = Just (number, fromIntegral setting / fromIntegral maxValue)
      analogHandler _                                         = Nothing
      buttonHandler (Joystick _ pressed number True  False _) = Just (number, pressed /= 0)
      buttonHandler _                                         = Nothing
    joystick <- openFile device ReadMode
    (exit, loop) <-
      interpretationLoop analogHandler buttonHandler interpretation
        $ interpretJoystick
        <$> hGet joystick byteLength
    return
      (
        do
          exit
          hClose joystick
      , loop
      )
