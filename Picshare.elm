module Picshare exposing (main)
import Html exposing (..)
import Html.Attributes exposing (..)

baseUrl : String
baseUrl = 
    "https://programming-elm.com/"


viewDetailedPhoto : String -> String -> Html msg
viewDetailedPhoto url caption =
    div [ class "detailed-photo" ]
        [ img [ src url ] []
        , div [ class "photo-info" ]
            [ h2 [ class "caption", align "center" ] [ text caption ] ]
        ]

main : Html msg -- Html (type) msg (type variable)
main = 
    div []
        [ div [class "header"]
            [ h1 [] [text "Picshare"]]
        , div [ class "content-flow" ]
            [ div [ class "detailed-photo" ]
                [ viewDetailedPhoto (baseUrl ++ "1.jpg") "Surfing"
                , viewDetailedPhoto (baseUrl ++ "2.jpg") "The Fox"
                , viewDetailedPhoto (baseUrl ++ "3.jpg") "Evening"
                ]
            ]
        ]

