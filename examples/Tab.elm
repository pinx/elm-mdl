module Tab exposing (main)

import Html.App as App
import Html exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json exposing ((:=), at)
import Html.Attributes exposing (href, class, style)
import Material
import Material.Layout as Layout
import Material.Options as Style 
import Material.Scheme
import Material.Options exposing (css)

main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd Msg )
init =
    (initialModel ! [])


initialModel : Model
initialModel =
    { mdl = Material.model
    , selectedTab = 1
    , categories = [ "cars", "bikes", "planes" ]
    , downCoord = Coord (Just 0.0) (Just 0.0)
    }


type alias Model =
    { mdl : Material.Model
    , selectedTab : Int
    , categories : List String
    , downCoord : Coord
    }


type alias Coord =
  { x : Maybe Float
  , y : Maybe Float
  }


type Msg
    = Down Coord
    | Up Coord
    | SelectTab Int
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Down coord ->
          let
              co = Debug.log "coord" coord
          in
            { model | downCoord = coord } ! []

        Up coord ->
          let
              dif = (distance model.downCoord (Debug.log "up" coord)) |> Debug.log "dif"
          in
            { model | downCoord = Coord Nothing Nothing} ! []

        SelectTab t ->
            { model | selectedTab = t } ! []

        Mdl msg ->
            Material.update msg model


view : Model -> Html Msg
view model =
    Layout.render Mdl
        model.mdl
        [ Layout.selectedTab model.selectedTab
        , Layout.onSelectTab SelectTab
        , Layout.fixedHeader
        ]
        { header = header model
        , drawer = []
        , tabs = tabs model
        , main = mainView model
        }


header : Model -> List (Html Msg)
header model =
    [ Layout.row []
        [ Layout.title [] [ text "Swipe tabs" ]
        , Layout.spacer
        ]
    ]


tabs : Model -> ( List (Html Msg), List (Style.Property () Msg) )
tabs model =
    ( model.categories
        |> List.map (\cat -> text cat)
    , []
    )


mainView : Model -> List (Html Msg)
mainView model =
    [ div
      [ class "main-view"
      , on "touchstart" ( Json.map Down downDecoder )
      , on "touchend" ( Json.map Up upDecoder )
      ]
      [ div []
        (List.repeat 200 (text "Hello world"))
      ]
    ]


downDecoder : Json.Decoder Coord
downDecoder =
  Json.object2 Coord
    (Json.maybe (at ["touches", "0", "clientX"] Json.float))
    (Json.maybe (at ["touches", "0", "clientY"] Json.float))


upDecoder : Json.Decoder Coord
upDecoder =
  Json.object2 Coord
    (Json.maybe (at ["changedTouches", "0", "clientX"] Json.float))
    (Json.maybe (at ["changedTouches", "0", "clientY"] Json.float))
    -- (Json.maybe ("clientX" := Json.float))
    -- (Json.maybe ("clientY" := Json.float))


distance : Coord -> Coord -> Float
distance down up =
  (Maybe.withDefault 0 down.x) - (Maybe.withDefault 0 up.x)
