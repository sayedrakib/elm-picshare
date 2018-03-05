module Picshare exposing (..) 
-- module Picshare exposing (main) 
import Html exposing (..)
import Html.Attributes exposing ( align, class, disabled, placeholder, src, type_, value )
import Html.Events exposing (onClick, onInput, onSubmit)
import Json.Decode exposing (Decoder, bool, int, list, string)
import Json.Decode.Pipeline exposing (decode, hardcoded, required)
import Http 


type alias Id = Int 

type alias Photo = 
    { id : Id 
    , url : String
    , caption : String
    , liked: Bool 
    , comments : List String
    , newComment : String
    }

type alias Model = 
    { photo : Maybe Photo } 

photoDecoder : Decoder Photo
photoDecoder = decode Photo 
    |> required "id" int 
    |> required "url" string
    |> required "caption" string
    |> required "liked" bool
    |> required "comments" (list string)
    |> hardcoded "" 


baseUrl : String 
baseUrl = 
    "https://programming-elm.com/"

initialModel : Model
initialModel =
    { photo =
        Just
            { id = 1
            , url = baseUrl ++ "1.jpg"
            , caption = "Surfing"
            , liked = False
            , comments = [ "Superb, dude!" ]
            , newComment = ""
            }
    }

init : ( Model, Cmd Msg )
init = ( initialModel, fetchFeed )

fetchFeed : Cmd Msg
fetchFeed = 
    Http.get (baseUrl ++ "feed/1") photoDecoder
       |> Http.send LoadFeed 

viewLoveButton : Photo -> Html Msg
viewLoveButton photo = 
    let
        buttonClass = 
            if photo.liked then 
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
   

{-
pen : String 
pen = "Writer"
listOfPens : List String
listOfPens = [ "econo", "matador" ]
listALL : List String -> List String 
listALL a = a ++ [ "Jelly" ]
-}

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

viewComments : Photo -> Html Msg 
viewComments photo = 
    div []
        [
            viewCommentList photo.comments
            , form [ class "new-comment", onSubmit SaveComment ]
                [ input 
                    [ type_ "text"
                    , placeholder "Add a comment...." 
                    , value photo.newComment 
                    , onInput UpdateComment
                    ]
                    []
                , button 
                    [ disabled (String.isEmpty photo.newComment) ]
                    [ text "Save" ]
                ]
        ]

viewDetailedPhoto : Photo -> Html Msg
viewDetailedPhoto photo =
    div [ class "detailed-photo" ]
        [ img [ src photo.url ] []
        , div [ class "photo-info" ]
            [ viewLoveButton photo
            , h2 [ class "caption", align "center" ] [ text photo.caption ]
            , viewComments photo  
            ]
        ]

viewFeed : Maybe Photo -> Html Msg
viewFeed maybePhoto =
    case maybePhoto of
        Just photo ->
            viewDetailedPhoto photo
        Nothing ->
            text ""

view : Model -> Html Msg -- Html (type) msg (type variable)
view model = 
    div []
        [ div [class "header"]
            [ h1 [] [text "Picshare"]]
        , div [ class "content-flow" ]
            [ viewFeed model.photo
            ]
        ]
type Msg =
    ToggleLike
    | UpdateComment String
    | SaveComment 
    | LoadFeed (Result Http.Error Photo)

saveNewComment : Photo -> Photo
saveNewComment photo = 
    case photo.newComment of 
        "" -> 
            photo
        _ ->
            let
                comment = 
                    String.trim photo.newComment
            in
            { photo 
                | comments = photo.comments ++ [ comment ]
                , newComment = ""
            }

toggleLike : Photo -> Photo
toggleLike photo =
    { photo | liked = not photo.liked }


updateComment : String -> Photo -> Photo
updateComment comment photo =
    { photo | newComment = comment }

update : Msg -> Model -> ( Model, Cmd Msg ) 
update msg model = 
    case msg of 
        ToggleLike -> 
            ( { model | liked = not model.liked }
            , Cmd.none
            )        
        UpdateComment comment -> 
            ( { model | newComment = comment }
            , Cmd.none
            )        
        SaveComment -> 
            ( saveNewComment model 
            , Cmd.none
            )
        LoadFeed _ ->
            ( model, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

main : Program Never Model Msg
main = 
    Html.program 
        { init = init
        , view = view
        , update = update 
        , subscriptions = subscriptions
        }