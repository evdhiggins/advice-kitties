module Main exposing (Model, Msg(..), adviceDecoder, catDecoder, catListDecoder, extractCat, getRandomAdvice, getRandomCat, init, main, subscriptions, update, view)

import Browser
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import Html.Styled.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, field, string)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view >> toUnstyled
        }



-- MODEL


type alias Model =
    { advice : String
    , url : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" "", Cmd.batch [ getRandomAdvice, getRandomCat ] )



-- UPDATE


type Msg
    = MorePlease
    | GotCat (Result Http.Error (List String))
    | GotAdvice (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( model, Cmd.batch [ getRandomAdvice, getRandomCat ] )

        GotAdvice result ->
            case result of
                Ok advice ->
                    ( { model | advice = advice }, Cmd.none )

                Err _ ->
                    ( { model | advice = "" }, Cmd.none )

        GotCat result ->
            case result of
                Ok url ->
                    ( { model | url = extractCat url }, Cmd.none )

                Err _ ->
                    ( { model | url = "" }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Advice Kitties" ]
        , div
            [ css
                [ width (vw 50)
                , height (vh 60)
                , position relative
                ]
            ]
            [ img
                [ src model.url
                , css
                    [ maxWidth (pct 100)
                    , maxHeight (pct 100)
                    , width auto
                    , height auto
                    , margin auto
                    ]
                ]
                []
            , div
                [ css
                    [ marginTop (px -10)
                    , padding (px 10)
                    , backgroundColor (rgba 50 50 50 1)
                    , color (hex "#fff")
                    , zIndex (int 100001)
                    , textAlign center
                    , position absolute
                    ]
                ]
                [ text model.advice ]
            ]
        , button
            [ onClick MorePlease
            , css
                [ display block
                , marginTop (px 50)
                ]
            ]
            [ text "More Please!" ]
        ]


extractCat : List String -> String
extractCat catImages =
    case List.head catImages of
        Just url ->
            url

        Nothing ->
            ""



-- HTTP


getRandomAdvice : Cmd Msg
getRandomAdvice =
    Http.get
        { url = "https://api.adviceslip.com/advice"
        , expect = Http.expectJson GotAdvice adviceDecoder
        }


adviceDecoder : Decoder String
adviceDecoder =
    field "slip" (field "advice" string)


getRandomCat : Cmd Msg
getRandomCat =
    Http.get
        { url = "https://api.thecatapi.com/v1/images/search"
        , expect = Http.expectJson GotCat catListDecoder
        }


catDecoder : Decoder String
catDecoder =
    field "url" string


catListDecoder : Decoder (List String)
catListDecoder =
    Json.Decode.list catDecoder
