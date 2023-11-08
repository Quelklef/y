module Input
  ( focusInput
  , hitEnter
  , html
  , infuse
  )
  where

import Attribute (Attribute)
import Attribute as A
import Css as C
import Data.JSValue (toJSValue)
import Design as Ds
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Html (Html, textareaS)
import InputBox (InputBox)
import InputBox as IB
import MasonPrelude
import ModelMsg (Height, InputAction(..), Model, Msg(..))
import Platform (afterRender, Update)
import RefEq (RefEq(..))
import WHATWG.HTML.All as H
import WHATWG.HTML.All (Event)
import WHATWG.HTML.HTMLTextAreaElement as TextArea

ref :: Ref InputBox
ref = unsafePerformEffect $ Ref.new IB.default

infuse :: ∀ model msg.
  (model -> InputBox -> model) ->
  (model -> msg -> Update msg model) ->
  (model -> msg -> Update msg model)
infuse addIB update =
  \model msg -> do
    currentInputBox <-
      liftEffect do
        oldState /\ mnewState <- updateState

        case mnewState of
          Just newState -> pure newState
          Nothing -> pure oldState

    update (addIB model currentInputBox) msg

updateState :: Effect (InputBox /\ Maybe InputBox)
updateState = do
    oldState <- Ref.read ref
    doc <- H.document'
    mtextarea <- (H.getElementById id doc <#> H.toMaybeHTMLTextAreaElement)

    newState <-
      case mtextarea of
        Just ta -> do
          value <- TextArea.value ta
          height <- toNumber <$> H.scrollHeight ta
          selectionStart <- TextArea.selectionStart ta
          selectionEnd <- TextArea.selectionEnd ta

          let
            newState =
              oldState
              # IB.setContent value
              # IB.setHeight (height + Ds.inputBoxBorderWidth)
              # IB.setSelectionRange (selectionStart /\ selectionEnd)

          Ref.write newState ref
          pure $ Just newState

        Nothing -> pure Nothing

    pure $ oldState /\ newState

id :: String
id = "input"

html :: Model -> Html Msg
html model =
  textareaS
     [ C.height $ C.px $ IB.height model.inputBox
     , C.flex "1"
     , C.borderJ [ C.px Ds.inputBoxBorderWidth, "solid", Ds.vars.color ]
     , C.padding ".45em"
     , Ds.inputStyles
     , C.borderTop "none"
     ]
     [ A.id id
     , A.alwaysSet $ A.value $ IB.content model.inputBox
     , A.alwaysSet
       $ A.property "selectionStart"
       $ toJSValue
       $ IB.selectionStart model.inputBox
     , A.alwaysSet
       $ A.property "selectionEnd"
       $ toJSValue
       $ IB.selectionEnd model.inputBox
     , onInput
     , detectSpecial
     ]
     []

onInput :: Attribute Msg
onInput =
  A.on "input"
    \_ -> do
      oldState /\ mnewState <- updateState

      case mnewState of
        Just newState ->
          let
            height = IB.height newState

            helper ::
              Boolean ->
              (Maybe InputAction /\ Height ->
               Maybe InputAction /\ Height
              ) ->
              Maybe (Maybe InputAction /\ Height) ->
              Maybe (Maybe InputAction /\ Height)
            helper b update ma =
              let helper2 a = if b then Just $ update a else ma in
              case ma of
                Just a -> helper2 a
                Nothing -> helper2 $ Nothing /\ height

          in
          pure
          $ Nothing
            # helper (height /= IB.height oldState)
                (\(a /\ _) -> a /\ height)
            # helper
                (IB.content newState == "/edit " && IB.content oldState == "/edit")
                (\(_ /\ h) -> Just Edit /\ h)
            <#> uncurry UpdateInputBox

        Nothing -> pure Nothing

detectSpecial :: Attribute Msg
detectSpecial =
  A.on "keydown"
  $ H.toMaybeKeyboardEvent
    .> case _ of
         Just kbe ->
           if H.key kbe == "Enter" && (H.ctrlKey kbe || H.metaKey kbe) then do
             H.preventDefault kbe
             pure $ Just SendMessage
           else if H.key kbe == "z" && H.ctrlKey kbe then do
             H.preventDefault kbe
             pure $ Just Undo
           else
             pure Nothing

         Nothing -> pure Nothing

focusInput :: ∀ a. Update a Unit
focusInput =
  afterRender
  $ H.document'
    >>= H.getElementById id .> map H.toMaybeHTMLElement
    >>= maybe (pure unit) (H.focus {})

hitEnter :: Event -> Effect (Maybe Msg)
hitEnter =
  H.toMaybeKeyboardEvent
  .> case _ of
      Just kbe ->
        if H.key kbe == "Enter" then do
          document <- H.document'
          body <- H.unsafeBody document
          bind (H.activeElement document)
            case _ of
              Just activeElement ->
                if (RefEq body == RefEq activeElement) then do
                  bind
                    (H.getElementById id document # map H.toMaybeHTMLElement)
                    (maybe (pure unit) (H.focus {}))

                  H.preventDefault kbe
                  pure Nothing
                else
                  pure Nothing

              Nothing -> pure Nothing
        else
          pure Nothing

      Nothing -> pure Nothing
