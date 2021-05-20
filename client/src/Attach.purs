module Y.Client.Attach (el, (&), (!), Builder, build, attach, class IsAttributeOrStyles, getAttributeOrStyles, AttributeOrStyles) where

import Prelude

import Unsafe.Coerce (unsafeCoerce)
import Type.Equality (class TypeEquals)

import Html (Html)
import Html as H
import Attribute (Attribute)
import Css (Styles)

data Builder msg = Builder
  { tag :: String
  , styles :: Array Styles
  , attrs :: Array (Attribute msg)
  }

el :: forall msg. String -> Builder msg
el tag = Builder { tag, styles: [], attrs: [] }

build :: forall msg. Builder msg -> Array (Html msg) -> Html msg
build (Builder builder) children = H.elementS builder.tag builder.styles builder.attrs children

infixl 1 build as !

attach :: forall msg atOrSt. IsAttributeOrStyles msg atOrSt => Builder msg -> atOrSt -> Builder msg
attach (Builder builder) atOrSt =
  case getAttributeOrStyles atOrSt of
    AttributeOrStyles_Attribute at -> Builder $ builder { attrs = builder.attrs <> [at] }
    AttributeOrStyles_Styles st -> Builder $ builder { styles = builder.styles <> [st] }

infixl 2 attach as &

data AttributeOrStyles msg = AttributeOrStyles_Attribute (Attribute msg) | AttributeOrStyles_Styles Styles

class IsAttributeOrStyles msg atOrSt where
  getAttributeOrStyles :: atOrSt -> AttributeOrStyles msg

instance isAttributeOrStylesAttribute :: TypeEquals msg1 msg2 => IsAttributeOrStyles msg1 (Attribute msg2) where
  getAttributeOrStyles = AttributeOrStyles_Attribute <<< unsafeCoerce  -- hehe

instance isAttributeOrStylesStyles :: IsAttributeOrStyles msg Styles where
  getAttributeOrStyles = AttributeOrStyles_Styles
