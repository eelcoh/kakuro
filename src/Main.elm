module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h2, h3, input, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onInput)
import List.Extra exposing (subsequences)
import Set exposing (Set)


type alias Model =
    { val : Int
    , num : Int
    , include : Set Int
    , exclude : Set Int
    }


initialModel : Model
initialModel =
    { val = 25
    , num = 4
    , include = Set.empty
    , exclude = Set.empty
    }


type Msg
    = SetNum Int
    | NewVal String
    | Toggle InOrExclude Int Bool


type InOrExclude
    = Include
    | Exclude


digits : List Int
digits =
    [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]


numbers : Model -> List (List Int)
numbers { val, num, include, exclude } =
    let
        usedAsList =
            Set.toList include

        allUsed ns =
            let
                numsAsSet =
                    Set.fromList ns
            in
            List.map (\n -> Set.member n numsAsSet) usedAsList
                |> List.all identity

        excludedAsList =
            Set.toList exclude

        nonExcluded ns =
            let
                numsAsSet =
                    Set.fromList ns
            in
            List.map (\n -> not (Set.member n numsAsSet)) excludedAsList
                |> List.all identity
    in
    subsequences digits
        |> List.filter (\s -> num == List.length s)
        |> List.filter (\s -> val == List.sum s)
        |> List.filter allUsed
        |> List.filter nonExcluded


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetNum num ->
            { model | num = num }

        NewVal s ->
            case String.toInt s of
                Just val ->
                    { model | val = val, include = Set.empty, exclude = Set.empty }

                Nothing ->
                    model

        Toggle Include n True ->
            { model | include = Set.insert n model.include, exclude = Set.remove n model.exclude }

        Toggle Include n False ->
            { model | include = Set.remove n model.include }

        Toggle Exclude n True ->
            { model | exclude = Set.insert n model.exclude, include = Set.remove n model.include }

        Toggle Exclude n False ->
            { model | exclude = Set.remove n model.exclude }


view : Model -> Html Msg
view model =
    let
        numList =
            numbers model

        allNums =
            List.concat numList
                |> Set.fromList
                |> Set.toList

        allNumsOut =
            List.filter (\n -> not (List.member n allNums)) digits
    in
    div []
        [ h1 [] [ text "kakuro helper" ]
        , h2 [] [ text "input" ]
        , h3 [] [ text "value" ]
        , viewVal model.val
        , h3 [] [ text "number of digits" ]
        , viewNum model.num
        , h3 [] [ text "must include" ]
        , viewUsed model.include
        , h3 [] [ text "must exclude" ]
        , viewExcluded model.exclude
        , h2 [] [ text "possible sequences" ]
        , div [] (List.map (viewNums model.include) numList)
        , h2 [] [ text "possible numbers" ]
        , viewNums model.include allNums
        , h2 [] [ text "impossible numbers" ]
        , viewNums model.exclude allNumsOut
        ]


viewVal : Int -> Html Msg
viewVal val =
    div []
        [ div [] [ text (String.fromInt val) ]
        , input [ onInput NewVal ] [ text (String.fromInt val) ]
        ]


viewNum : Int -> Html Msg
viewNum num =
    let
        colour n =
            if n == num then
                style "background-color" "green"

            else
                style "background-color" "white"

        viewToggle n =
            button [ onClick (SetNum n), colour n ]
                [ text (String.fromInt n) ]

        viewToggles =
            List.map viewToggle digits
    in
    div [] viewToggles


viewUsed : Set Int -> Html Msg
viewUsed include =
    let
        isUsed n =
            Set.member n include
                |> viewToggle n

        colour u =
            if u then
                style "background-color" "red"

            else
                style "background-color" "white"

        viewToggle n u =
            button [ onClick (Toggle Include n (not u)), colour u ]
                [ text (String.fromInt n) ]

        viewToggles =
            List.map isUsed digits
    in
    div [] viewToggles


viewExcluded : Set Int -> Html Msg
viewExcluded exclude =
    let
        excludeToggle n =
            Set.member n exclude
                |> viewToggle n

        colour u =
            if u then
                style "background-color" "red"

            else
                style "background-color" "white"

        viewToggle n u =
            button [ onClick (Toggle Exclude n (not u)), colour u ]
                [ text (String.fromInt n) ]

        viewToggles =
            List.map excludeToggle digits
    in
    div [] viewToggles


viewNums : Set Int -> List Int -> Html Msg
viewNums include nums =
    let
        colour n =
            if Set.member n include then
                style "color" "grey"

            else
                style "color" "black"

        viewNumText n =
            span [ colour n ] [ text (String.fromInt n) ]

        viewInBracktes t =
            (openBracket :: t) ++ [ closeBracket ]

        comma =
            span [] [ text ", " ]

        openBracket =
            span [] [ text "[ " ]

        closeBracket =
            span [] [ text " ]" ]

        viewNumss ns =
            List.map viewNumText ns
                |> List.intersperse comma
                |> viewInBracktes
                |> div []
    in
    div []
        [ viewNumss nums ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
