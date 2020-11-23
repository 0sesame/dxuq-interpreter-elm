import Dxuq4 exposing (topInterp)
import Browser
import Html exposing (Html, Attribute, input, button, div, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


-- MAIN

main = Browser.sandbox { init = init, update = update, view = view }


-- MODEL

type alias Model = {dxuq4 : String, result : String}

init : Model
init = {dxuq4 = "", result = ""}


-- UPDATE
type Msg = Update | Change String

update : Msg -> Model -> Model
update msg model =
    case msg of
        Update ->
            {model | result = topInterp(model.dxuq4)}
        Change newExpr ->
            {model | dxuq4 = newExpr} 
    



-- VIEW
view : Model -> Html Msg
view model = 
    div []
        [ input [ placeholder "Enter a DXUQ4 expression...", value model.dxuq4, onInput Change] []
        , button [ onClick Update ] [ text "Interpret Expression" ]
        , div [] [ text (model.result)]
        ]
