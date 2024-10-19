module OpenGames.Engine.ValueUpdate where

import Data.Ix
import Data.Array.MArray
import Data.Array.IO

type Values x = IOUArray x Double
