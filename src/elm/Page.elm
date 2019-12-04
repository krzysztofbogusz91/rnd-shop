module Page exposing (Page(..), view)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Route exposing (Route)


{-| Determines which navbar link (if any) will be rendered as active.

Note that we don't enumerate every page here, because the navbar doesn't
have links for every page. Anything that's not part of the navbar falls
under Other.

-}
type Page
    = Other
    | Home
    | Basket
    | Shop


{-| Take a page's Html and frames it with a header and footer.

The caller provides the current user, so we can display in either
"signed in" (rendering username) or "signed out" mode.

isLoading is for determining whether we should show a loading spinner
in the header. (This comes up during slow page transitions.)

-}
view : Page -> { title : String, content : Html msg } -> Document msg
view page { title, content } =
    { title = title ++ " - Elm SPA"
    , body =
       [ div [class "page-container"] [ viewHeader page
        , div [class "content-container" ][ content]
        , viewFooter
        ]]
    }


viewHeader : Page -> Html msg
viewHeader page =
    nav [ class "navbar" ]
        [ div [ class "navbar__container" ]
            [ ul [ class "navbar-menu navbar-menu--left"]
                  [ navbarLink page Route.Home [ text "Home" ],
                   navbarLink page Route.Shop [ text "Shop" ]
                  ]
            , ul [ class "navbar-menu navbar-menu--right" ]
                [ activeLink  [ ],
                  navbarLink page Route.Basket [ text "Checkout" ]
                 ]
            ]
        ]


viewFooter : Html msg
viewFooter =
    footer [class "footer"]
        [ div [] [ text "this is footer"]
        ]


navbarLink : Page -> Route -> List (Html msg) -> Html msg
navbarLink page route linkContent =
    li [ classList [ ( "nav-item", True ), ( "active", isActive page route ) ] ]
        [ a [ class "nav-item--link", Route.href route ] linkContent ]

activeLink : List (Html msg) -> Html msg
activeLink linkContent =
    li [ class "nav-item" ]
        [ input [ class "nav-item--search" ] []]

isActive : Page -> Route -> Bool
isActive page route =
    case ( page, route ) of
        ( Home, Route.Home ) ->
            True

        ( Basket, Route.Basket ) ->
            True

        _ ->
            False


{-| Render dismissable errors. We use this all over the place!
-}
viewErrors : msg -> List String -> Html msg
viewErrors dismissErrors errors =
    if List.isEmpty errors then
        Html.text ""

    else
        div
            [ class "error-messages"
            , style "position" "fixed"
            , style "top" "0"
            , style "background" "rgb(250, 250, 250)"
            , style "padding" "20px"
            , style "border" "1px solid"
            ]
        <|
            List.map (\error -> p [] [ text error ]) errors
                ++ [ button [ onClick dismissErrors ] [ text "Ok" ] ]
