module Picshare exposing (..) 
-- module Picshare exposing (main) 
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


baseUrl : String 
baseUrl = 
    "https://programming-elm.com/"

initialModel : { url : String, caption : String, liked: Bool }
initialModel =
    { url = baseUrl ++ "3.jpg"
    , caption = "Evening"
    , liked  = False
    }

viewDetailedPhoto : { url : String, caption : String, liked : Bool } -> Html Msg
viewDetailedPhoto model =
    let 
        buttonClass = 
            if model.liked then 
                "fa-heart"
            else 
                "fa-heart-o"
        msg: Msg
        msg = 
            if model.liked then
                Unlike
            else
                Like 
    in 
    div [ class "detailed-photo" ]
        [ img [ src model.url ] []
        , div [ class "photo-info" ]
            [ div [class "like-button"]
                [ i 
                    [class "fa fa-2x" 
                    , class buttonClass 
                    , onClick msg 
                    ]
                     []
                ]
            , h2 [ class "caption", align "center" ] [ text model.caption ] 
            ]
        ]

view : { url : String, caption : String, liked : Bool } -> Html Msg -- Html (type) msg (type variable)
view model = 
    div []
        [ div [class "header"]
            [ h1 [] [text "Picshare"]]
        , div [ class "content-flow" ]
            [ viewDetailedPhoto model
            ]
        ]
type Msg =
    Like | Unlike

update : Msg 
        -> { url : String, caption : String, liked : Bool} 
        -> { url : String, caption : String, liked : Bool } 
update msg model = 
    case msg of 
        Like -> 
            { model | liked = True }
        Unlike ->
            { model | liked = False }


main : Program Never { url : String, caption : String, liked : Bool } Msg
main = 
    Html.beginnerProgram 
        { model = initialModel 
        , view = view
        , update = update 
        }