module Main (main) where

import MasonPrelude

import Attribute (Attribute)
import Attribute as A
import Compat.Event (Event(..), EventPayload(..))
import Compat.Message (Message)
import Compat.Transmission
  (ToClient(..), ToServer(..), fromToClient, toToServer)

import Css (Styles)
import Css as C
import Css.Functions as CF
import Data.Array as Array
import Data.List ((:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Set as Set
import Data.String.Utils (startsWith)
import Data.String.CodePoints as String
import Debug as Debug
import Design as Ds
import Html (Html)
import Html as H
import Input (focusInput, hitEnter)
import Input as Input
import ModelMsg (FoldedEvents, InputAction(..), Leaf, MessageTree, Model, Msg(..))
import InputBox as InputBox
import Platform (Cmd(..), Program, Update, batch, tell)
import Platform as Platform
import Producer as P
import Sub (Sub)
import Sub as Sub
import TreeMap (IVP, Thread, toTreeMap)
import TreeMap as TreeMap
import WebSocketSub (wsToSub)
import WHATWG.HTML.All as HTML
import Y.Client.WebSocket (Client)
import Y.Client.WebSocket as Ws
import Y.Shared.Id (Id)
import Y.Shared.Id as Id
import Y.Shared.Instant (Instant, asMilliseconds)
import Y.Shared.Instant as Instant
import Y.Shared.Transmission as Trans

foreign import initialize_f ::
  ∀ r.
  (String -> String -> r) ->
  String ->
  String ->
  Effect r
foreign import getHostname :: Effect String
foreign import sendNotification :: Boolean -> String -> String -> String -> Effect Unit
foreign import notificationsPermission :: Effect Unit
foreign import hasFocus :: Effect Boolean
foreign import setItem :: String -> String -> Effect Unit

foreign import getItemImpl ::
  (∀ a. Maybe a) ->
  (∀ a. a -> Maybe a) ->
  String ->
  Effect (Maybe String)

getItem :: String -> Effect (Maybe String)
getItem = getItemImpl Nothing Just

main :: Program Unit Model Msg
main = do
  Platform.app
    { init
    , update
    , view
    , subscriptions
    }

init :: Unit -> Update Msg Model
init _ = do
  liftEffect notificationsPermission

  userId /\ convoId <- liftEffect do
    (freshUserId :: Id "User") /\ (freshConvoId :: Id "Convo" ) <-
      liftEffect $ lift2 Tuple Id.new Id.new
    initialize_f
      (\idStr1 idStr2 ->
         case lift2 Tuple (Id.parse idStr1) (Id.parse idStr2) of
           Right t -> t
           Left error -> unsafeThrow error
      )
      (Id.format freshUserId)
      (Id.format freshConvoId)

  wsClient <-
    liftEffect do
      hostname <- getHostname
      Ws.newConnection { url: "ws://" <> hostname <> ":8081" }
      -- Ws.newConnection { url: "wss://y.maynards.site:8081" }
      -- Ws.newConnection { url: "wss://pre.y.maynards.site:8081" }

  tell
    (Cmd
       \msgCallback -> do
         Ws.onOpen
           (msgCallback WebSocketOpened)
           wsClient
    )

  notificationSound <- liftEffect (getItem audioLSKey) <#> fromMaybe ""

  pure
    { convoId
    , userId
    , wsClient
    , events:
        { raw: []
        , folded:
            { names: Map.empty
            , messages: TreeMap.empty
            , read: Set.empty
            }
        }
    , inputBox: InputBox.default
    , thread: Nothing
    , messageParent: Nothing
    , nameInput: ""
    , unread: false
    , notificationSound
    }

update :: Model -> Msg -> Update Msg Model
update =
  Input.infuse (_ { inputBox = _ })
    \model@{ userId, convoId } ->
      let _ = Debug.log model in
      case _ of
        UpdateNotificationSound soundName -> do
          liftEffect $ setItem audioLSKey soundName
          pure $ model { notificationSound = soundName }

        Undo -> pure $ model { inputBox = InputBox.undo model.inputBox }
        Focused -> pure $ model { unread = false }

        SelectSibling mid -> do
          focusInput
          model2 <- pushReadEvent model mid

          pure
            (model2
               { messageParent = Just mid
               , thread = TreeMap.findLeaf mid model.events.folded.messages
               }
            )

        UpdateName -> do
          liftEffect do
            pushEvent model
              \_ ->
                EventPayload_SetName
                  { convoId
                  , userId
                  , name: model.nameInput
                  }

          pure model

        UpdateNameInput str -> pure $ model { nameInput = str }

        SelectMessageParent mid -> do
          focusInput
          pure $ model { messageParent = Just mid }

        NewThread -> do
          focusInput

          pure
            (model
               { thread = Nothing
               , messageParent = Nothing
               }
            )

        SelectThread mid -> do
          focusInput
          model2 <- pushReadEvent model mid

          pure
            (model2
               { thread = Just mid
               , messageParent = Just mid
               }
            )

        SendMessage -> do
          focusInput

          let
            content = InputBox.content model.inputBox

            errorMsg =
              model
                { inputBox =
                    InputBox.setContentAndCursor
                      "You didn't send that message!"
                      model.inputBox
                }

          if content == "" then
            pure model
          else if content == "/delete" then
            (do
               mid <- model.messageParent
               { value: mes } <- TreeMap.lookup mid model.events.folded.messages

               Just
                 (if mes.authorId == userId then do
                    liftEffect
                      (pushEvent model
                         \_ ->
                           EventPayload_MessageDelete
                             { convoId
                             , userId
                             , messageId: mes.id
                             }
                      )

                    pure (model { inputBox = InputBox.default })
                  else
                    pure errorMsg
                 )
            )
            # fromMaybe (pure model)
          else if startsWith "/edit " content then
            (do
               mid <- model.messageParent
               { value: mes } <- TreeMap.lookup mid model.events.folded.messages

               Just
                 (if mes.authorId == userId then do
                    liftEffect
                      (pushEvent model
                         \_ ->
                           EventPayload_MessageEdit
                             { convoId
                             , messageId: mes.id
                             , authorId: userId
                             , content: String.drop 6 content
                             }
                      )

                    pure (model { inputBox = InputBox.reset model.inputBox })
                  else
                    pure errorMsg
                 )
            )
            # fromMaybe (pure model)
          else do
            id :: Id "Message" <- liftEffect Id.new

            liftEffect do
              pushEvent model
                \instant ->
                  EventPayload_MessageSend
                    { convoId
                    , message:
                        { id
                        , timeSent: instant
                        , authorId: model.userId
                        , convoId
                        , deleted: false
                        , depIds:
                            model.messageParent
                            <#> Set.singleton
                            # fromMaybe mempty
                        , content: InputBox.content model.inputBox
                        }
                    }

            pure
              (model
                 { inputBox = InputBox.reset model.inputBox
                 , messageParent = Just id
                 , thread =
                     case model.thread of
                       Nothing -> Just id
                       _ -> model.thread
                 }
              )

        UpdateInputBox mInputAction height ->
          let
            model2 = model { inputBox = InputBox.setHeight height model.inputBox }
          in
          case mInputAction of
            Just a ->
              case a of
                Edit ->
                  pure
                  $ (do
                       mid <- model2.messageParent
                       { value: mes } <- TreeMap.lookup mid model2.events.folded.messages
                       Just
                         if mes.deleted then
                           model2
                         else
                           model2
                             { inputBox =
                                 InputBox.setContentAndCursor
                                   ("/edit " <> mes.content)
                                   model.inputBox
                             }
                    )
                    # fromMaybe model2

            Nothing -> pure model2

        TransmissionReceived mtc ->
          case mtc of
            Just (ToClient_Broadcast events) -> do
              focused <- liftEffect hasFocus

              let
                newEvents :: Array Event
                newEvents = addEvents model.events.raw events

                folded :: FoldedEvents
                folded = foldEvents newEvents

                newLeaf :: Id "Message" -> Maybe (Id "Message")
                newLeaf =
                  TreeMap.findNewLeaf ~$ model.events.folded.messages ~$ folded.messages

                newThread :: Maybe Leaf
                newThread = newLeaf =<< model.thread

                model2 =
                  model
                    { events =
                        { raw: newEvents

                        -- this is erasing read messages that were not added by events
                        -- it's currently not a problem but very tricky to debug
                        -- so I'm documenting it
                        , folded
                        }
                    , nameInput =
                        if model.nameInput == "" then
                          Map.lookup userId folded.names
                          # fromMaybe ""
                        else
                          model.nameInput
                    , messageParent =
                        case model.messageParent of
                          Just mid ->
                            if
                              InputBox.content model.inputBox == ""
                              && TreeMap.isLeaf mid model.events.folded.messages
                            then
                              newThread
                            else
                              case model.messageParent <#> TreeMap.member ~$ folded.messages of
                                Just false -> newLeaf =<< model.messageParent
                                _ -> model.messageParent
                          Nothing -> Nothing
                    , thread = newThread
                    , unread = if focused then false else true
                    }

                firstMessage :: Maybe Message
                firstMessage =
                  splitEvents events
                  # _.messageSend
                  # List.head
                  <#> _.message

              model3 <-
                case newThread of
                  Just messageId -> pushReadEvent model2 messageId
                  Nothing -> pure model2

              case firstMessage of
                Just mes ->
                  if mes.authorId == userId then
                    (do
                       mid <- model.messageParent
                       { parent } <- TreeMap.lookup mid model3.events.folded.messages

                       Just
                         case parent of
                           Just _ -> pure unit
                           Nothing -> focusInput -- new thread has been created
                    )
                    # fromMaybe (pure unit)
                  else
                    liftEffect
                    $ sendNotification
                        (model.notificationSound /= "")
                        (makeAudioUrl model.notificationSound)
                        (getName mes.authorId model3.events.folded.names)
                        mes.content

                Nothing -> pure unit

              pure model3

            Nothing -> pure model

        WebSocketOpened -> do
          liftEffect do
            Ws.transmit (Trans.ToServer_Hello { userId }) model.wsClient
            Ws.transmit
              (toToServer $ ToServer_Subscribe { userId, convoId })
              model.wsClient

            Ws.transmit
              (toToServer $ ToServer_Pull { convoId })
              model.wsClient

          pure model

makeAudioUrl :: String -> String
makeAudioUrl name = "https://www.myinstants.com/media/sounds/" <> name <> ".mp3"

audioLSKey :: String
audioLSKey = "notification-sound"

pushReadEvent :: Model -> Id "Message" -> Update Msg Model
pushReadEvent model@{ convoId, userId, events } mid =
  if Set.member (userId /\ mid) events.folded.read then
    pure model
  else do
    liftEffect
      (pushEvent model
         \_ ->
           EventPayload_SetReadState
             { convoId
             , userId
             , messageId: mid
             , readState: true
             }
      )

    pure
      (model
         { events { folded { read = Set.insert (userId /\ mid) events.folded.read } } }
      )

pushEvent :: ∀ a r.
  { convoId :: Id "Convo"
  , wsClient :: Client Trans.ToServer a
  | r
  } ->
  (Instant -> EventPayload) ->
  Effect Unit
pushEvent { convoId, wsClient } payload = do
  eventId :: Id "Event" <- Id.new
  now <- Instant.getNow
  (Ws.transmit)
    (toToServer
     $ ToServer_Push
       { convoId
       , event:
           Event
             { id: eventId
             , time: now
             , payload: payload now
             }
       }
    )
    wsClient

addEvents :: Array Event -> Array Event -> Array Event
addEvents events newEvents =
  case oldEnd, newHead of
    Just (Event e1), Just (Event e2) ->
      if e1.time <= e2.time then
        potentialFinalArray
      else
        Array.sortWith eventTime potentialFinalArray
    _, _ -> potentialFinalArray

  where
    oldEnd :: Maybe Event
    oldEnd = Array.unsnoc events <#> _.last

    sortedEvents :: Array Event
    sortedEvents = Array.sortWith eventTime newEvents

    newHead :: Maybe Event
    newHead = Array.uncons sortedEvents <#> _.head

    potentialFinalArray :: Array Event
    potentialFinalArray = events <> sortedEvents

eventTime :: Event -> Instant
eventTime (Event e) = e.time

foldEvents :: Array Event -> FoldedEvents
foldEvents =
  splitEvents
  .> \events ->
       { names:
           foldl
             (\acc { userId, name } ->
                Map.insert userId name acc
             )
             Map.empty
             events.setName
       , messages:
           let
             initialTM :: MessageTree
             initialTM =
               events.messageSend
               <#> _.message .> toIVP
               # toTreeMap
           in
           foldl
             (\acc { messageId } ->
                acc
                # TreeMap.edit messageId
                    (\vpc -> vpc { value = vpc.value { deleted = true } })
                # TreeMap.removeLeafRecursive
                    (_.value .> _.deleted)
                    messageId
             )
             initialTM
             events.messageDelete
           # foldl
               (\acc { messageId, content } ->
                  acc
                  # TreeMap.edit messageId
                      \vpc -> vpc { value = vpc.value { content = content } }
               )
             ~$ events.messageEdit
       , read:
           foldl
             (\acc { userId, messageId, readState } ->
                if readState then
                  Set.insert (userId /\ messageId) acc
                else
                  acc
             )
             Set.empty
             events.setReadState
       }

splitEvents ::
  Array Event ->
  { setName ::
       List
         { convoId :: Id "Convo"
         , userId :: Id "User"
         , name :: String
         }
   , messageSend ::
       List
         { convoId :: Id "Convo"
         , message :: Message
         }
   , messageEdit ::
       List
         { convoId :: Id "Convo"
         , messageId :: Id "Message"
         , authorId :: Id "User"
         , content :: String
         }
   , messageDelete ::
       List
         { convoId :: Id "Convo"
         , userId :: Id "User"
         , messageId :: Id "Message"
         }
   , setReadState ::
       List
         { convoId :: Id "Convo"
         , userId :: Id "User"
         , messageId :: Id "Message"
         , readState :: Boolean
         }
   }
splitEvents =
  foldr
    (\(Event event) acc ->
       case event.payload of
         EventPayload_SetName data' ->
           acc { setName = data' : acc.setName }

         EventPayload_MessageSend data' ->
           acc { messageSend = data' : acc.messageSend }

         EventPayload_MessageEdit data' ->
           acc { messageEdit = data' : acc.messageEdit }

         EventPayload_MessageDelete data' ->
           acc { messageDelete = data' : acc.messageDelete }

         EventPayload_SetReadState data' ->
           acc { setReadState = data' : acc.setReadState }
    )
    mempty

toIVP :: Message -> IVP (Id "Message") Message
toIVP value@{ id, depIds } =
  { id
  , value
  , parent: Set.findMax depIds
  }

-- separate function cuz wsToSub uses Refeq
transmissionReceived :: Maybe Trans.ToClient -> Msg
transmissionReceived = case _ of
  Just toClient -> TransmissionReceived $ Just $ fromToClient toClient
  Nothing -> TransmissionReceived Nothing

subscriptions :: Model -> Sub Msg
subscriptions model =
  batch
    [ wsToSub transmissionReceived model.wsClient
    , Sub.on "keydown" hitEnter
    , Sub.on "focus" focusHandler
    ]

focusHandler :: HTML.Event -> Effect (Maybe Msg)
focusHandler _ = pure $ Just Focused

view ::
  Model ->
  { head :: Array (Html Msg)
  , body :: Array (Html Msg)
  }
view model =
  { head: [ H.title $ "⅄" <> if model.unread then " (unread messages)" else "" ]
  , body:
      [ Ds.staticStyles
      , H.divS
          [ C.display "grid"
          , C.grid "100vh / min(30%, 350px) 1fr"
          ]
          []
          [ H.divS [ Ds.panel ] []
              [ nameChanger model
              , H.divS [ C.margin ".3em" ] []
                  [ H.text "Notification Sound "
                  , H.inputS
                      [ Ds.inputStyles2 ]
                      [ A.value model.notificationSound
                      , A.onInput UpdateNotificationSound
                      ]
                  ]
              , threadBar model
              ]
          , threadView model
          ]
      ]
  }

nameChanger :: Model -> Html Msg
nameChanger model =
  H.divS [ C.margin ".3em" ] []
    [ H.inputS
        [ Ds.inputStyles2 ]
        [ A.value model.nameInput
        , A.onInput UpdateNameInput
        ]
    , H.buttonS [ C.marginLeft "5px" ] [ A.onClick UpdateName ] [ H.text "Update Name"]
    ]

threadBar :: Model -> Html Msg
threadBar model =
  let
    leaves :: Array (Id "Message")
    leaves =
      TreeMap.leaves model.events.folded.messages
      # Array.sortBy
          (\a b ->
             compare
               ((snd b).value).timeSent
               ((snd a).value).timeSent
          )
      <#> fst
  in
  batch
    [ H.divS [ C.margin "5px" ] []
        [ H.button [ A.onClick NewThread ] [ H.text "New Thread" ] ]
    , H.divS
        [ C.overflow "auto"
        , C.borderJ [ Ds.vars.borderWidth1, "solid" ]
        ]
        []
      $ leaves
        <#> \mid ->
              let { messages, read } = model.events.folded in
              TreeMap.lookup mid messages
              # case _ of
                  Just { value: { authorId, content, deleted, timeSent } } ->
                    let
                      isChosen :: Boolean
                      isChosen =
                        case TreeMap.siblings mid messages of
                          Right [] -> true
                          Right siblings ->
                            (true /\ timeSent)
                            <= foldl
                                 (\(chosen /\ oldest) m ->
                                    min chosen (TreeMap.isLeaf m.id messages)
                                    /\ min oldest m.timeSent
                                 )
                                 (true /\ timeSent)
                                 siblings
                          Left _ -> false

                      isRead :: Boolean
                      isRead =
                        authorId == model.userId || Set.member (model.userId /\ mid) read
                    in
                    if (not isChosen && isRead && model.thread /= Just mid) || deleted then
                      mempty
                    else
                      H.divS
                        [ if model.thread == Just mid then
                            C.background Ds.vars.accent1
                          else
                            mempty
                        , Ds.following [ C.borderTop "1px solid" ]
                        , C.padding ".3em"
                        , C.whiteSpace "pre-wrap"
                        , C.overflow "auto"
                        ]
                        [ onNotSelectingClick $ SelectThread mid ]
                        [ H.spanS
                            [ if authorId == model.userId || isRead then
                                mempty
                              else
                                C.color "#ff4040"
                            ]
                            []
                            [ H.text content ]
                        ]

                  Nothing -> mempty
    ]

type IsSibling = Boolean

createMessage :: Model -> IsSibling -> Styles -> Message -> Html Msg
createMessage model isSibling styles mes =
  H.divS
    [ Ds.following [ C.borderBottom "1px solid" ]
    , C.position "relative"
    , C.padding "3px .25em 6px .25em"
    , styles
    , if
        model.messageParent == Just mes.id
        && model.messageParent /= model.thread
      then
        C.background "#000066"
      else
        mempty
    ]
    [ onNotSelectingClick
      $ (if isSibling then SelectSibling else SelectMessageParent)
          mes.id
    ]
    [ if mes.deleted then
        mempty
      else
        H.divS
          [ C.font "0.72em sans-serif"
          , C.opacity "0.6"
          , C.marginBottom "0.7em"
          ]
          []
          [ H.text $ getName mes.authorId model.events.folded.names
          , getParent mes model.events.folded.messages
            <#> formatTimeDiff <. _.timeSent ~$ mes.timeSent
            # case _ of
                Just diff ->
                  H.spanS [ C.marginLeft "12px" ]
                  [ A.title
                    $ dateString
                    $ asMilliseconds mes.timeSent
                  ]
                  [ H.text diff ]

                Nothing -> mempty
          ]
    , H.divS
        [ C.whiteSpace "pre-wrap"
        , C.position "relative"
        , C.overflowX "auto"
        , C.marginTop "3px"
        ]
        []
        [ H.text
            if mes.deleted then
              "(deleted)"
            else
              mes.content
        ]
    ]

threadView :: Model -> Html Msg
threadView model =
  H.keyedS "div"
    [ Ds.panel
    , C.transform
      $ CF.translateX
      $ CF.calc
      $ CF.sub "0px" Ds.vars.borderWidth1
    ]
    []
    [ "message list" /\ maybe mempty messageList mthread
    , "reply"
       /\ if model.messageParent == model.thread then
            mempty
          else
            fromMaybe mempty $ do
              id <- model.messageParent
              { value: message } <- TreeMap.lookup id model.events.folded.messages
              pure $ createMessage model false mempty message
    , "input" /\ messageInput
    ]

    where
      mthread :: Maybe (Thread Message)
      mthread =
        model.thread
        >>= TreeMap.getThread ~$ model.events.folded.messages

      messageInput :: Html Msg
      messageInput =
        H.divS
          [ C.display "flex"
          , C.width $ CF.calc $ CF.add "100%" Ds.vars.borderWidth1
          ]
          []
          [ Input.html model
          , H.button [ A.onClick SendMessage ] [ H.text "Send" ]
          ]

      messageList :: Thread Message -> Html Msg
      messageList =
        Array.fromFoldable
        .> map
             (\(message /\ siblings) ->
                batch
                $ Array.snoc
                    (siblings
                     <#> createMessage model true (C.background Ds.vars.lighterBackground22)
                    )
                    (createMessage model false (C.background Ds.vars.background) message)
                  # Array.reverse
             )
        .> \messagesHtml ->
             H.divS
               [ C.borderJ [ Ds.vars.borderWidth1, "solid" ]
               , C.overflow "auto"
               , C.display "flex"
               , C.flexDirection "column-reverse"
               , C.width $ CF.calc $ CF.sub "100%" Ds.vars.borderWidth1
               ]
               []
               messagesHtml

getName :: Id "User" -> Map (Id "User") String -> String
getName id names = Map.lookup id names # fromMaybe "<anonymous>"

foreign import dateString :: Number -> String

getParent :: Message -> MessageTree -> Maybe Message
getParent { id } tm =
  TreeMap.lookup id tm
  >>= _.parent
  >>= TreeMap.lookup ~$ tm
  <#> _.value

formatTimeDiff :: Instant -> Instant -> String
formatTimeDiff iOld iNew =
  let
    seconds = (asMilliseconds iNew - asMilliseconds iOld) / 1000.0

    show' time label = show (round time) <> label
  in
  if round seconds < 120 then
    show' seconds "s"
  else
    let minutes =  seconds / 60.0 in
    if round minutes < 120 then
      show' minutes "m"
    else
      let hours = minutes / 60.0 in
      if hours < 48.0 then
        show' hours "h"
      else
        let days = hours / 24.0 in
        if days < 14.0 then
          show' days "d"
        else
        show' (days / 7.0) "w"

foreign import isSelecting :: Effect Boolean

onNotSelectingClick :: Msg -> Attribute Msg
onNotSelectingClick =
  A.on "click" <. P.producer onNotSelectingClickRE

onNotSelectingClickRE :: Msg -> HTML.Event -> Effect (Maybe Msg)
onNotSelectingClickRE msg =
  \_ -> do
    selecting <- isSelecting

    if selecting then
      pure Nothing
    else
      pure $ Just msg
