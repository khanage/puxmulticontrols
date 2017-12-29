module App.State where

import Prelude

import App.Config (config)
import App.Routes (Route, match)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)

newtype Counter = Counter Int

derive instance newtypeCounter :: Newtype Counter _
derive instance genericCounter :: Generic Counter _
derive instance eqCounter      :: Eq      Counter

instance showCounter :: Show Counter where
  show = genericShow

newtype State = State
  { title :: String
  , route :: Route
  , loaded :: Boolean
  , counters :: Array Counter
  }

derive instance newtypeState :: Newtype State _

init :: String -> State
init url = State
  { title:    config.title
  , route:    match url
  , loaded:   false
  , counters: [Counter 1, Counter 2]
  }
