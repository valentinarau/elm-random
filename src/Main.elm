module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Json.Decode
import Set exposing (Set)
import Http
import Html.Attributes exposing (src)
import Array

---- MODEL ----


type alias Model =
    { 
      guesses : Set String
    , gameState : GameState
    }

type GameState
    = Loading
    | Running String
    | Won

init : ( Model, Cmd Msg )
init =
   ( {  
        guesses = Set.empty
      , gameState = Running mistery
      }
    , Cmd.none
    )

mistery: String
mistery = "figaro"

---- UPDATE ----

type Msg
    = Guess String
    | Restart
    | Win


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
   case msg of
        Guess letter ->
            ( { model | guesses = Set.insert letter model.guesses }, Cmd.none )

        Restart ->
            ( { model | guesses = Set.empty, gameState = Running mistery }, Cmd.none )

        Win ->  
            ( { model | guesses = Set.empty, gameState = Won }, Cmd.none )


---- VIEW ----


view : Model -> Html Msg
view model =
    let
        phraseHtml =
            case model.gameState of
                Loading ->
                    span [] [ text "Loading" ]

                Running phrase ->
                    if (phrase |> String.length) == (model.guesses |> Set.toList |> List.filter (\letter ->
                                String.contains letter phrase
                            ) |> List.length) 
                    then div []
                            [ button [ onClick Win ] [ text "Continue" ]
                            ]
                    else
                        phrase
                        |> String.split ""
                        |> List.map
                            (\letter ->
                                if letter == " " then
                                    " "
                                else if Set.member letter model.guesses then
                                    letter
                                else
                                    "_"
                            )
                        |> List.map
                            (\entry ->
                                span [] [ text entry ]
                            )
                        |> div []

                Won ->
                    div [] [ text "Congrats!" ]

        failuresHtml =
            case model.gameState of
                Running phrase ->
                    model.guesses
                        |> Set.toList
                        |> List.filter
                            (\letter ->
                                not <| String.contains letter phrase
                            )
                        |> List.map
                            (\letter ->
                                span [] [ text letter ]
                            )
                        |> div []

                _ ->
                    text ""

        buttonsHtml =
            case model.gameState of
                Running _ -> 
                    "abcdefghijklmnopqrstuvwxyz"
                        |> String.split ""
                        |> List.map
                            (\letter ->
                                button [ onClick <| Guess letter ] [ text letter ]
                            )
                        |> div []
                _ -> text ""
        
        restart = 
            case model.gameState of
                    Running _ -> 
                        div []
                            [ button [ onClick Win ] [ text "Restart" ]
                            ]
                    _ -> text ""

    in
    div []
        [ img [ src "/cat.png" ] [],
          phraseHtml,
          buttonsHtml,
          failuresHtml,
          restart
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
