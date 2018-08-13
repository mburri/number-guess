module Main exposing (..)

import Html
    exposing
        ( Html
        , text
        , div
        , h1
        , img
        , input
        , label
        , button
        , form
        )
import Html.Attributes exposing (src, value, class, type_)
import Html.Events exposing (onInput, onClick, onSubmit)
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


init : Int -> Model
init number =
    { number = number
    , guess = ""
    , guessed = NotYet
    }


generateNumber =
    Random.generate New (Random.int 1 20)



---- UPDATE ----


type Msg
    = ChangeGuess String
    | Guess
    | New Int
    | Again


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        New number ->
            ( init number, Cmd.none )

        Again ->
            ( model, generateNumber )

        ChangeGuess g ->
            ( { model | guess = g }, Cmd.none )

        Guess ->
            let
                g =
                    Result.withDefault -1 (String.toInt model.guess)
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
            Ok g ->
                text ""

            Result.Err _ ->
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
            [ text ("Gratuliere! Die gesuchte Zahl war " ++ (toString target))
            , div [] [ button [ onClick Again ] [ text "Nochmal!" ] ]
            ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = ( init 0, generateNumber )
        , update = update
        , subscriptions = always Sub.none
        }
