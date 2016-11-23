{-|
Module      :  System.Hardware.LinuxJoystick
Copyright   :  (c) 2016 Brian W Bush
License     :  MIT
Maintainer  :  Brian W Bush <consult@brianwbush.info>
Stability   :  Experimental
Portability :  Linux

Interpret events from a Linux joystick, which must conform to the Linux Joystick API \<<https://www.kernel.org/doc/Documentation/input/joystick-api.txt>\>.
-}


{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}


module System.Hardware.LinuxJoystick (
-- * Types and values
  Joystick(..)
, minValue
, maxValue
-- * Event handling
, interpretJoystick
, readJoystick
) where


import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Serialize (Serialize)
import Data.ByteString.Lazy.Char8 as BS (ByteString, length, readFile, splitAt, unpack)
import Data.Bits ((.&.), (.|.), complement, shift)
import GHC.Generics (Generic)


-- | The minimum for 'value'.
minValue :: Int
minValue = - maxValue


-- | The maximum for 'value'.
maxValue :: Int
maxValue = 32767


-- | Joystick data. See \<<https://www.kernel.org/doc/Documentation/input/joystick-api.txt>\> for details.
data Joystick =
  Joystick
  {
    timestamp :: Int  -- ^ The event timestamp in milliseconds.
  , value     :: Int  -- ^ The data value.
  , number    :: Int  -- ^ The button or axis number.
  , button    :: Bool -- ^ Whether the button is being reported.
  , axis      :: Bool -- ^ Whether the axis is being reported.
  , initial   :: Bool -- ^ Whether the initial value is being reported.
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON Joystick

instance ToJSON Joystick

instance Binary Joystick

instance Serialize Joystick


-- | Interpret Limux Joystick bytes accoding to \<<https://www.kernel.org/doc/Documentation/input/joystick-api.txt>\>.
interpretJoystick :: ByteString -- ^ The eight bytes.
                  -> Joystick   -- ^ The corresponding joystick data.
interpretJoystick x
  | BS.length x /= 8 = error "LinuxJoystick.interpretJoystick: eight bytes required."
  | otherwise        = let
                        [x0, x1, x2, x3, x4, x5, x6, x7] = fromEnum <$> unpack x
                        timestamp = toEnum $ ((x3 `shift` 8 .|. x2) `shift` 8 .|. x1) `shift` 8 .|. x0
                        value = twosComplement $ x5 `shift` 8 .|. x4
                        typ = x6
                        number = x7
                        button = 0x01 .&. typ /= 0
                        axis = 0x02 .&. typ /= 0
                        initial = 0x80 .&. typ /= 0
                      in
                        Joystick{..}


-- | Decode a two's complement.
twosComplement :: Int -- ^ The two's complement.
               -> Int -- ^ THe corresponding integer.
twosComplement x =
  fromEnum (x .&. complement mask) - fromEnum (x .&. mask)
    where
      mask = 0x8000


-- | Read a stream of joystick data.
readJoystick :: FilePath      -- ^ The joystick device, e.g., "\/dev\/input\/js0".
             -> IO [Joystick] -- ^ Action to read the joystick data.
readJoystick path =
  let
    chunks :: ByteString -> [ByteString]
    chunks x =
      let
        (y, ys) = BS.splitAt 8 x
      in
        y : chunks ys
  in
    map interpretJoystick
      . chunks 
      <$> BS.readFile path
