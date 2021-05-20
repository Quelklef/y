module Y.Client.Util.OnKey
  ( ShouldKeyBePressed(..)
  , onKey
  , onKey'one
  , KeyListener
  , keyListenerToAttribute
  ) where

import Prelude

import Data.Maybe (fromMaybe)

import Attribute (Attribute)
import Attribute as A
import WHATWG.DOM.Event (target, currentTarget) as Wwg
import WHATWG.HTML.KeyboardEvent (KeyboardEvent, toMaybeKeyboardEvent, shiftKey, key) as Wwg

import Y.Client.Util.Opts (Opts)
import Y.Client.Util.Is ((===))

-- Due to https://github.com/ursi/purescript-elmish/issues/6, we have to collect
-- all key listeners for any element into a single event.
-- To do this, we define a new KeyListener type with a Monoid instance and
-- a conversion function keyListenerToAttribute to Attributes.

data KeyListener action = KeyListener (Wwg.KeyboardEvent -> action)

instance semigroupKeyListener :: Semigroup action => Semigroup (KeyListener action) where
  append (KeyListener l1) (KeyListener l2) = KeyListener \keyEvent -> l1 keyEvent <> l2 keyEvent

instance monoidKeyListener :: Monoid action => Monoid (KeyListener action) where
  mempty = KeyListener (const mempty)

type OnKeyOpts = { self :: ShouldKeyBePressed, shift :: ShouldKeyBePressed }
data ShouldKeyBePressed = NoPreference | RequirePressed | RequireNotPressed

keyListenerToAttribute :: forall action. Monoid action => KeyListener action -> Attribute action
keyListenerToAttribute (KeyListener listener) =
  A.on "keydown" \event -> pure $ event # Wwg.toMaybeKeyboardEvent # map listener # fromMaybe mempty

onKey :: forall action. Monoid action => String -> Opts OnKeyOpts -> action -> KeyListener action
onKey key mkOpts action =
  let opts = mkOpts { self: NoPreference, shift: NoPreference }
  in KeyListener \event ->
       event
       # Wwg.toMaybeKeyboardEvent
       # map (\keyEvent ->
                let
                  keyOk = Wwg.key keyEvent == key
                  selfOk = pressedOk opts.self $ Wwg.target keyEvent === Wwg.currentTarget keyEvent
                  shiftOk = pressedOk opts.shift $ Wwg.shiftKey keyEvent
                  ok = keyOk && selfOk && shiftOk
                in
                  if ok then action else mempty)
       # fromMaybe mempty

  where
    pressedOk shouldBePressed isPressed = case shouldBePressed of
      NoPreference -> true
      RequirePressed -> isPressed == true
      RequireNotPressed -> isPressed == false

-- | Convenience function for creating an Attribute directly
onKey'one :: forall action. Monoid action => String -> Opts OnKeyOpts -> action -> Attribute action
onKey'one key mkOpts action = onKey key mkOpts action # keyListenerToAttribute
