{-|
Module      :  System.Hardware.Linux.Joystick
Copyright   :  (c) 2016 Brian W Bush
License     :  MIT
Maintainer  :  Brian W Bush <consult@brianwbush.info>
Stability   :  Experimental
Portability :  Linux

Interpret events from a Linux joystick, which must conform to the Linux Joystick API \<<https://www.kernel.org/doc/Documentation/input/joystick-api.txt>\>.
-}


{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}


module System.Hardware.Linux.Joystick (
-- * Types and values
  Joystick(..)
, minValue
, maxValue
, byteLength
-- * Event handling
, interpretJoystick
, readJoystick
) where


import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Binary (Binary(..), decode, encode)
import Data.Binary.Get (getWord16host, getWord32host)
import Data.Binary.Put (putWord16host, putWord32host)
import Data.Bits ((.&.), complement, shift)
import Data.ByteString.Lazy.Char8 as BS (ByteString, length, readFile, splitAt)
import Data.Serialize (Serialize)
import Data.Word (Word8, Word16, Word32)
import GHC.Generics (Generic)


-- | The minimum for 'value'.
minValue :: Int
minValue = - maxValue


-- | The maximum for 'value'.
maxValue :: Int
maxValue = 1 `shift` 15 - 1


-- | The number of bytes in a joystick event.
byteLength :: Integral a => a
byteLength =
  fromIntegral
    . BS.length
    . encode
    $ RawJoystick 0 0 0 0



data RawJoystick =
  RawJoystick
  {
    rawTime :: Word32
  , rawValue :: Word16
  , rawType  :: Word8
  , rawNumber :: Word8
  }
  deriving (Eq, Ord, Read, Show)

instance Binary RawJoystick where
  get =
    do
      rawTime   <- getWord32host
      rawValue  <- getWord16host
      rawType   <- get
      rawNumber <- get
      return RawJoystick{..}
  put RawJoystick{..} =
    do
      putWord32host rawTime
      putWord16host rawValue
      put rawType
      put rawNumber


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


-- | Interpret Linux Joystick bytes accoding to \<<https://www.kernel.org/doc/Documentation/input/joystick-api.txt>\>.
interpretJoystick :: ByteString -- ^ The eight bytes.
                  -> Joystick   -- ^ The corresponding joystick data.
interpretJoystick x =
  let
    RawJoystick{..} = decode x
    timestamp = fromIntegral rawTime
    value = twosComplement rawValue
    number = fromIntegral rawNumber
    typ = fromIntegral rawType :: Int
    button = 0x01 .&. typ /= 0
    axis = 0x02 .&. typ /= 0
    initial = 0x80 .&. typ /= 0
  in
    Joystick{..}


-- | Decode a two's complement.
twosComplement :: Word16 -- ^ The two's complement.
               -> Int    -- ^ The corresponding integer.
twosComplement x =
  fromIntegral (x' .&. complement mask) - fromIntegral (x' .&. mask)
    where
      x' = fromIntegral x :: Int
      mask = 1 `shift` 15


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
