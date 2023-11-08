module Design
  ( following
  , inputBoxBorderWidth
  , inputStyles
  , inputStyles2
  , panel
  , staticStyles
  , vars
  )
  where

import MasonPrelude

import Css (Styles)
import Css as C
import Css.Functions as CF
import Css.Global as CG
import Css.Variables (makeVariables)
import Html (Html)
import Platform (batch)

sv =
  makeVariables
    { borderWidth1: "1px"
    , hue1: "0"
    , gray1: "#e6e6e6"
    , purple1: "#634372"
    , saturation1: "0%"
    }
    \r ->
      let
        makeBackground :: Int -> String
        makeBackground percent =
          CF.hsl r.hue1 r.saturation1
          $ show percent <> "%"
      in
      { accent1: r.purple1
      , background: makeBackground 12
      , color: r.gray1
      , lighterBackground22: makeBackground 22
      , lighterBackground32: makeBackground 32
      , lighterBackground60: makeBackground 60
      }

vars = sv.values

staticStyles :: ∀ a. Html a
staticStyles =
  CG.style
    [ CG.body
        [ sv.styles
        , C.margin "0"
        , C.fontFamily "monospace"
        , C.background vars.background
        , C.color vars.color
        ]
    , CG.button
        [ C.background vars.lighterBackground32
        , C.color vars.color
        , C.border "none"
        , C.variable "padding" "4px"
        , C.paddingTop $ CF.var "padding"
        , C.paddingBottom $ CF.var "padding"
        , C.fontFamily "monospace"
        ]
    , CG.rule "::-webkit-scrollbar"
        [ C.variable "size" "10px"
        , C.width $ CF.var "size"
        , C.height $ CF.var "size"
        ]
    , CG.rule "::-webkit-scrollbar-thumb"
        [ C.background vars.lighterBackground60
        , C.borderRadius "3px"
        ]
    , CG.rule "::-webkit-scrollbar-track"
        [ C.background vars.lighterBackground22 ]
    ]

panel :: Styles
panel =
  batch
    [ C.display "grid"
    , C.gridAutoRows "fit-content(100%)"
    ]

inputBoxBorderWidth :: Number
inputBoxBorderWidth = 1.0

following :: Array Styles -> Styles
following = C.mapSelector $ C.prepend "* + "

inputStyles :: Styles
inputStyles =
  batch
    [ C.outline "none"
    , C.background vars.lighterBackground22
    , C.color vars.color
    , C.fontFamily "monospace"
    ]

inputStyles2 :: Styles
inputStyles2 =
  batch
    [ inputStyles
    , C.border "none"
    , C.borderRadius "5px"
    , C.padding "3px"
    , C.lineHeight "1.25"
    ]
