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
    article []
        [ node "link" [ href "https://fonts.googleapis.com/css?family=Poppins|Yatra+One&display=swap" ] []
        , div
            [ css
                [ display inlineFlex
                , flexDirection column
                , justifyContent center
                , width (pct 100)
                ]
            ]
            [ div
                [ css
                    [ maxWidth (vw 90)
                    , minWidth (vw 50)
                    , margin auto
                    , display inlineFlex
                    , flexDirection column
                    , fontFamilies [ "Poppins", "sans-serif" ]
                    , border3 (px 3) solid (hex "#9400D3")
                    , borderRadius (px 20)
                    , padding (rem 2)
                    ]
                ]
                [ h1
                    [ css
                        [ textAlign center
                        , width (pct 100)
                        , fontFamilies [ "Yatra One", "cursive" ]
                        , fontWeight bold
                        , fontSize (rem 2)
                        ]
                    ]
                    [ text "Advice Kitties" ]
                , div
                    [ css
                        [ width (pct 100)
                        , displayFlex
                        , alignItems center
                        ]
                    ]
                    [ figure
                        [ css
                            [ display inlineTable
                            , borderSpacing (px 20)
                            , width (pct 1)
                            , textAlign center
                            , margin (px 0)
                            ]
                        ]
                        [ img
                            [ src model.url
                            , css
                                [ maxWidth (vw 70)
                                , maxHeight (vh 60)

                                -- , width auto
                                -- , height auto
                                , margin auto
                                ]
                            ]
                            []
                        , figcaption
                            [ css
                                [ backgroundColor (rgba 50 50 50 1)
                                , color (hex "#fff")
                                , padding4 (px 10) (px 0) (px 10) (px 0)
                                , marginTop (px -10)
                                ]
                            ]
                            [ text model.advice
                            ]
                        ]
                    ]
                , button
                    [ onClick MorePlease
                    , css
                        [ display block
                        , marginTop (px 50)
                        , padding4 (px 20) (px 10) (px 20) (px 10)
                        , backgroundColor (hex "#9400D3")
                        , color (hex "#FFF")
                        , fontWeight bold
                        , fontSize (rem 1.2)
                        ]
                    ]
                    [ text "More Please!" ]
                ]
            ]
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
