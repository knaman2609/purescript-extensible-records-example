module Main where

import Prelude
import Data.Array
import Data.Maybe
import Control.Monad.Eff 
import Control.Monad.Eff.Console 

import Partial.Unsafe
import Data.Generic.Rep
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary, genericArbitrary)
import Test.QuickCheck.Gen (Gen, randomSample')

data Props = P {id :: String , p :: Int}
data Q = QS String | QI Int
type M t = {value :: Q , props:: Props  | t}

derive instance genericProps :: Generic (Props) _
instance arbitraryProps :: Arbitrary Props  where arbitrary = genericArbitrary

logProps (P props) = do
  log props.id
  log $ show props.p

fn p@{value: (QS x)}  = do
  log x
  logProps p.props

fn {value: (QI _)}  = do
  log "something"

main = unsafePartial $ do
  x <- randomSample' 1 (arbitrary :: Gen Props)
  fn {value: QS "Naman", id: 1, props: (fromJust $ head x)}
