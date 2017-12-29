module App.Events where

import Prelude

import App.Routes (Route)
import App.State (Counter(..), State(..))
import Control.Monad.Aff.Console (CONSOLE, log)
import Data.Array (alterAt, snoc)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (wrap)
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects)

data CounterEvent
  = DeleteCounter
  | IncrementCounter
  | DecrementCounter

data Event
  = PageView Route
  | CreateCounter
  | CounterChildEvent { index :: Int, action :: CounterEvent}

type AppEffects fx = (ajax :: AJAX, console :: CONSOLE | fx)

foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (PageView route) (State st) = noEffects $ State st { route = route, loaded = true }

foldp CreateCounter (State st) =
  { state: wrap $ st { counters = snoc st.counters (Counter 0) }
  , effects: [do log "hell there"
                 pure Nothing]
  }

foldp (CounterChildEvent event) state@(State st) =
  noEffects $ maybe state updateCounters (alterAt event.index (flip eventHandler event.action) st.counters)
  where
    updateCounters :: Array Counter -> State
    updateCounters newCounters = State $ st { counters = newCounters}

    eventHandler :: Counter -> CounterEvent -> Maybe Counter
    eventHandler (Counter i) = case _ of
      DeleteCounter    -> Nothing
      IncrementCounter -> Just $ Counter $ i + 1
      DecrementCounter -> Just $ Counter $ i - 1
