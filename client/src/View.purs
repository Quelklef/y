module Client.View (view) where

import Prelude

import Data.List (toUnfoldable)

import Html (Html)
import Html as H

import Client.Core (Model, Msg)

view :: Model -> { head :: Array (Html Msg), body :: Array (Html Msg) }
view model =
  { head: []
  , body: model.convo.events <#> (\event -> H.p [] [ H.text "an event" ]) # toUnfoldable
  }
