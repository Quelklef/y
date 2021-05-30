module Y.Client.Util.Redundant where

import Prelude

newtype Redundant s c = Rt
  { state :: s
  , computed :: c
  , patch :: { oldState :: s, newState :: s, oldComputed :: c } -> c
  }

mk :: forall s c.
  { state :: s
  , computed :: c
  , patch :: { oldState :: s , newState :: s , oldComputed :: c } -> c
  }
  -> Redundant s c
mk { state, computed, patch } = Rt { state, computed, patch }

mk0 :: forall s c.
  { state0 :: s
  , computed0 :: c
  , state :: s
  , patch :: { oldState :: s , newState :: s , oldComputed :: c } -> c
  }
  -> Redundant s c
mk0 { state0, computed0, state, patch } = Rt
  { state: state
  , computed: patch { oldState: state0, newState: state, oldComputed: computed0 }
  , patch: patch
  }

mkEz :: forall s c. s -> (s -> c) -> Redundant s c
mkEz s f = Rt { state: s, computed: f s, patch: _.newState >>> f }

set :: forall s c. s -> Redundant s c -> Redundant s c
set newState (Rt rt) = Rt
  { state: newState
  , computed: rt.patch { oldState: rt.state, newState: newState, oldComputed: rt.computed }
  , patch: rt.patch
  }

get :: forall s c. Redundant s c -> c
get (Rt rt) = rt.computed
