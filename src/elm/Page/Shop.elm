module Page.Shop exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Html exposing (Html, div, h2, h4, ul, li, text)
import Html.Attributes exposing (class)
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session
    , pageTitle : String
    , pageBody : Html Msg
    }

init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , pageTitle = "Shop"
      , pageBody = viewBody
      }
    , Cmd.none
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = model.pageTitle
    , content =
        div [ class "container" ]
            [ h2 [] [ text model.pageTitle ]
            , div [] [ model.pageBody ]
            ]
    }

viewBody : Html msg
viewBody =
    div [class "shop-container"]
       [ 
        div [class "shop-filters"] [ h4 [] [ text "Shop Filters"]],    
        div [class "shop-list"] [ h4 [] [ text "Shop Items"], viewShopList ]
        ]

viewShopList : Html msg
viewShopList =
    div [class "shop-list-list"]
       [ ul [class "list"] [li [class "list-item"][text "item 1"]] ]

-- UPDATE


type Msg
    = Todo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Todo ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
