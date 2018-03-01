module Picshare exposing (..) 
-- module Picshare exposing (main) 
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

type alias Model = 
    { url : String
    , caption : String
    , liked: Bool 
    }

baseUrl : String 
baseUrl = 
    "https://programming-elm.com/"

initialModel : Model
initialModel =
    Model (baseUrl ++ "3.jpg") "Evening" False

viewLoveButton : Model -> Html Msg
viewLoveButton model = 
    let
        buttonClass = 
            if model.liked then 
                "fa-heart"
            else 
                "fa-heart-o"
    in 
    div [class "like-button"]
        [ i 
            [class "fa fa-2x" 
            , class buttonClass 
            , onClick ToggleLike 
            ]
            []
        ]

viewDetailedPhoto : Model -> Html Msg
viewDetailedPhoto model =
    div [ class "detailed-photo" ]
        [ img [ src model.url ] []
        , div [ class "photo-info" ]
            [ viewLoveButton model
            , h2 [ class "caption", align "center" ] [ text model.caption ] 
            ]
        ]

view : Model -> Html Msg -- Html (type) msg (type variable)
view model = 
    div []
        [ div [class "header"]
            [ h1 [] [text "Picshare"]]
        , div [ class "content-flow" ]
            [ viewDetailedPhoto model
            ]
        ]
type Msg =
    ToggleLike

update : Msg -> Model -> Model 
update msg model = 
    case msg of 
        ToggleLike -> 
            { model | liked = not model.liked }

main : Program Never Model Msg
main = 
    Html.beginnerProgram 
        { model = initialModel 
        , view = view
        , update = update 
        }