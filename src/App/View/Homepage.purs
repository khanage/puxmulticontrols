module App.View.Homepage where

import Prelude hiding (id,div)

import App.Events (CounterEvent(..), Event(..))
import App.State (Counter, State(..))
import Data.Array (length, zip, (..))
import Data.Foldable (for_)
import Data.Tuple (uncurry)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (button, div, h1)
import Text.Smolder.HTML.Attributes (className, id)
import Text.Smolder.Markup (text, (!), (#!))

mapI
  :: forall a b
   . (Int -> a -> b)
  -> Array a
  -> Array b
mapI f as =
  map (uncurry f) $ zip (0 .. (length as - 1)) as

view
  :: State
  -> HTML Event
view (State state) =
  div do
    h1 $ text "CounterStrike"
    button ! id "hi" ! className "add-counter" #! onClick (pure CreateCounter) $ text "➕"
    div ! className "counters" $ do
      for_ (mapI createCounterForIndex state.counters) \a -> a

createCounterForIndex
  :: Int
  -> Counter
  -> HTML Event
createCounterForIndex index counter = do
  div $ do
    button #! onClick (pure $ CounterChildEvent { index: index, action: DecrementCounter }) $ text "-"
    text $ show counter
    button #! onClick (pure $ CounterChildEvent { index: index, action: IncrementCounter }) $ text "+"
