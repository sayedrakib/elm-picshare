module Picshare exposing (..) 
-- module Picshare exposing (main) 
import Html exposing (..)
import Html.Attributes exposing ( align, class, disabled, placeholder, src, type_, value )
import Html.Events exposing (onClick, onInput, onSubmit)

type alias Id = Int 

type alias Photo = 
    { id : Id 
    , url : String
    , caption : String
    , liked: Bool 
    , comments : List String
    , newComment : String
    }

type alias Model = Photo 

baseUrl : String 
baseUrl = 
    "https://programming-elm.com/"

initialModel : Model
initialModel =
    { id = 1
    , url = baseUrl ++ "1.jpg"
    , caption = "Surfing"
    , liked = False
    , comments = [ "Superb, dude!" ]
    , newComment = ""
    }

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

viewComment : String -> Html Msg
viewComment comment = 
    li []
        [ strong [] [ text "Comment: " ]
        , text (" " ++ comment )
        ]

viewCommentList : List String -> Html Msg
viewCommentList comments = 
    case comments of 
        [] -> 
            text ""
        _ -> 
            div [ class "comments" ]
                [ ul []
                    (List.map viewComment comments) 
                ]

viewComments : Model -> Html Msg 
viewComments model = 
    div []
        [
            viewCommentList model.comments
            , form [ class "new-comment", onSubmit SaveComment ]
                [ input 
                    [ type_ "text"
                    , placeholder "Add a comment...." 
                    , value model.newComment 
                    , onInput UpdateComment
                    ]
                    []
                , button 
                    [ disabled (String.isEmpty model.newComment) ]
                    [ text "Save" ]
                ]
        ]

viewDetailedPhoto : Model -> Html Msg
viewDetailedPhoto model =
    div [ class "detailed-photo" ]
        [ img [ src model.url ] []
        , div [ class "photo-info" ]
            [ viewLoveButton model
            , h2 [ class "caption", align "center" ] [ text model.caption ]
            , viewComments model  
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
    | UpdateComment String
    | SaveComment 

saveNewComment : Model -> Model
saveNewComment model = 
    case model.newComment of 
        "" -> 
            model
        _ ->
            let
                comment = 
                    String.trim model.newComment
            in
            { model 
                | comments = model.comments ++ [ comment ]
                , newComment = ""
            }

update : Msg -> Model -> Model 
update msg model = 
    case msg of 
        ToggleLike -> 
            { model | liked = not model.liked }
        
        UpdateComment comment -> 
            { model | newComment = comment }
        
        SaveComment -> 
            saveNewComment model 

main : Program Never Model Msg
main = 
    Html.beginnerProgram 
        { model = initialModel 
        , view = view
        , update = update 
        }