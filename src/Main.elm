module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { inputText : String
    , todo : List Todo
    }


type alias Todo =
    { text : String
    , status : Bool
    }


init : Model
init =
    { inputText = ""
    , todo =
        []
    }


type Msg
    = Add
    | Toggle String
    | Archive String
    | SetText String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Add ->
            if model.inputText == "" then
                model

            else
                { model
                    | todo =
                        List.append
                            model.todo
                            [ { text = model.inputText
                              , status = False
                              }
                            ]
                    , inputText = ""
                }

        Toggle text ->
            { model
                | todo =
                    List.map
                        (\item ->
                            if item.text == text then
                                { item
                                    | status = not item.status
                                }

                            else
                                item
                        )
                        model.todo
            }

        Archive text ->
            { model
                | todo =
                    List.filter
                        (\item -> text /= item.text)
                        model.todo
            }

        SetText value ->
            { model
                | inputText = value
            }


emptyState : Html Msg
emptyState =
    p []
        [ text "no data"
        ]


view : Model -> Html Msg
view model =
    main_ [ class "container mx-auto p-4 max-w-md" ]
        [ h1 [ class "text-4xl" ]
            [ text "My Todo List" ]
        , if List.length model.todo == 0 then
            p
                [ class "my-2 p-2 border-dotted border-4 border-black" ]
                [ text "Either you've done everything already or there are still things to add to your list. Add your first todo ↓" ]

          else
            ul []
                (List.map
                    (\l ->
                        li [ class "my-2 p-1 flex items-center" ]
                            [ label [ class "flex-1" ]
                                [ input [ class "mr-4", type_ "checkbox", checked l.status, onClick (Toggle l.text) ] []
                                , span
                                    [ class "text-l", classList [ ( "line-through", l.status ) ] ]
                                    [ text l.text ]
                                ]
                            , button
                                [ class " px-2 py-1 flex-2", onClick (Archive l.text) ]
                                [ img [ src "/trash.svg" ] [] ]
                            ]
                    )
                    model.todo
                )
        , Html.form
            [ class "flex mt-2"
            , onSubmit Add
            ]
            [ input [ class "flex-1 mr-2 px-2 py-1 border-solid border-4 border-black", placeholder "E.g adopt an owl", value model.inputText, onInput SetText ] []
            , input [ type_ "submit", value "Add", class " flex-2 px-2 py-1 text-white bg-black " ]
                [ text "Add"
                ]
            ]
        ]
