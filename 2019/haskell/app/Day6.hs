{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Day6 where

import qualified Data.Attoparsec.Text          as P
import           Data.Char                      ( isAlphaNum )
import           Data.Hashable                  ( Hashable )
import qualified Data.HashMap.Strict           as HM
import           Data.List                      ( intersect )
import           Data.Text                      ( Text )

import           Lib                            ( readCustomLines )

newtype Object = Object String
    deriving (Eq, Show, Hashable)

-- First object orbits the second, ~~this is the inverse of the input format~~
newtype Orbit = Orbit { unOrbit :: (Object, Object) }
    deriving (Eq, Show)

objectP :: P.Parser Object
objectP = Object <$> P.many1' (P.satisfy isAlphaNum)

orbitP :: P.Parser Orbit
orbitP = go <$> (objectP <* P.char ')') <*> objectP
    where go o1 o2 = Orbit (o2, o1)

buildOrbits :: HM.HashMap Object Object -> Object -> [Object]
buildOrbits orbitMap obj =
    maybe [] (\x -> x : buildOrbits orbitMap x) (HM.lookup obj orbitMap)

problem1 :: Text -> IO ()
problem1 inputData = print totalOrbitz
  where
    input    = readCustomLines orbitP inputData
    orbitMap = HM.fromList . map unOrbit $ input
    totalOrbitz =
        sum . map (length . buildOrbits orbitMap) . HM.keys $ orbitMap

problem2 :: Text -> IO ()
problem2 inputData = print transfers
  where
    input         = readCustomLines orbitP inputData
    orbitMap      = HM.fromList . map unOrbit $ input
    usRoute       = buildOrbits orbitMap (Object "YOU")
    santaBoiRoute = buildOrbits orbitMap (Object "SAN")
    commonLength  = length $ intersect usRoute santaBoiRoute
    transfers =
        (length usRoute - commonLength) -- Number of hops in our unique portion of the orbit path
                                        + (length santaBoiRoute - commonLength) -- Same, but for Santa
