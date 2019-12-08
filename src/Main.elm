module Main exposing (Guess(..), Model, Msg(..), generateNumber, init, main, update, view, viewError, viewHint, viewMessage)

import Browser
import Html
    exposing
        ( Html
        , button
        , div
        , form
        , h1
        , img
        , input
        , label
        , text
        )
import Html.Attributes exposing (class, src, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Random



---- MODEL ----


type Guess
    = NotYet
    | Guessed Int


type alias Model =
    { number : Int
    , guess : String
    , guessed : Guess
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel 0
    , generateNumber
    )


generateNumber =
    Random.generate New (Random.int 1 20)



---- UPDATE ----


type Msg
    = ChangeGuess String
    | Guess
    | New Int
    | Again


initialModel number =
    { number = number
    , guess = ""
    , guessed = NotYet
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        New number ->
            ( initialModel number, Cmd.none )

        Again ->
            ( model, generateNumber )

        ChangeGuess g ->
            ( { model | guess = g }, Cmd.none )

        Guess ->
            let
                g =
                    String.toInt model.guess |> Maybe.withDefault -1
            in
            ( { model | guessed = Guessed g }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [ class "title" ] [ text "Zahlen raten!" ]
        , div [ class "text" ] [ text "Ich habe mir eine Zahl zwischen 1 und 20 ausgedacht." ]
        , div [ class "text" ] [ text "Rate mal welche!" ]
        , div [ class "guess" ]
            [ form [ onSubmit Guess ]
                [ label []
                    [ input
                        [ type_ "number"
                        , onInput ChangeGuess
                        , value model.guess
                        ]
                        []
                    ]
                , button [ onClick Guess ] [ text "Raten" ]
                ]
            ]
        , viewError model
        , viewHint model
        ]


viewError : Model -> Html msg
viewError model =
    if String.length model.guess > 0 then
        case String.toInt model.guess of
            Just g ->
                text ""

            Nothing ->
                div [ class "error" ]
                    [ (model.guess ++ " ist keine Zahl!") |> text
                    ]

    else
        text ""


viewHint : Model -> Html Msg
viewHint model =
    case model.guessed of
        NotYet ->
            text ""

        Guessed guessed ->
            viewMessage guessed model.number


viewMessage : Int -> Int -> Html Msg
viewMessage guessed target =
    if guessed < target then
        div [ class "wrong" ] [ text "Die gesuchte Zahl ist grösser!" ]

    else if guessed > target then
        div [ class "wrong" ] [ text "Die gesuchte Zahl ist kleiner!" ]

    else
        div [ class "correct" ]
            [ text ("Gratuliere! Die gesuchte Zahl war " ++ String.fromInt target)
            , div [] [ button [ onClick Again ] [ text "Nochmal!" ] ]
            ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
