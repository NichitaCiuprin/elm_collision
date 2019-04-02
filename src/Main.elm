port module Main exposing (main)

import Html exposing (canvas, div, p, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onMouseDown)
import Mouse exposing (moves)


port frameUpdated : (String -> msg) -> Sub msg


type alias Point =
    { x : Float
    , y : Float
    }


type alias Payload =
    ( Model, Cmd Msg )


type alias CircleId =
    Int


type alias Model =
    { circles : List Circle
    , boundary : Boundary
    , error : Maybe String
    }


type alias Circle =
    { id : Int
    , x : Float
    , y : Float
    , radius : Float
    , isSelected : Bool
    , collidedWith : List CircleId --if list is empty then circle is not collided
    , movementDirection : Float
    , movementSpeed : Float
    }


type alias Boundary =
    { x : Float
    , y : Float
    , height : Float
    , wight : Float
    , borderThickness : Float
    }


type Msg
    = OnCircleMouseDown Circle
    | OnMouseUp Mouse.Position
    | OnMouseMoved Mouse.Position
    | OnFrameUpdate String


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = viewError
        , update = updateError
        , subscriptions = subscriptions
        }


init : Payload
init =
    { circles =
        [ Circle 1 100 200 50 False [] 270 20
        , Circle 2 200 300 50 False [] 90 20
        , Circle 3 300 400 50 False [] 45 20
        , Circle 4 400 500 50 False [] 45 20
        ]
    , boundary = Boundary 0 0 700 1000 5
    , error = Nothing
    }
        ! []


viewError : Model -> Html.Html Msg
viewError model =
    case model.error of
        Nothing ->
            view model

        Just msg ->
            Html.text msg


view : Model -> Html.Html Msg
view model =
    let
        viewCircles =
            let
                viewCircle : Circle -> Html.Html Msg
                viewCircle circle =
                    let
                        whiteOrRed =
                            if List.length circle.collidedWith > 0 then
                                "#FF0000"

                            else
                                "#737373"
                    in
                    span
                        [ onMouseDown (OnCircleMouseDown circle)
                        , style
                            [ ( "left", toString (circle.x - circle.radius) ++ "px" )
                            , ( "top", toString (circle.y - circle.radius) ++ "px" )
                            , ( "height", toString (circle.radius * 2) ++ "px" )
                            , ( "width", toString (circle.radius * 2) ++ "px" )
                            , ( "background-color", whiteOrRed )
                            , ( "position", "absolute" )
                            , ( "border-radius", "50%" )
                            ]
                        ]
                        []
            in
            div [] (List.map viewCircle model.circles)

        viewBoundary =
            let
                boundary =
                    model.boundary
            in
            div
                [ style
                    [ ( "position", "absolute" )
                    , ( "top", toString boundary.y ++ "px" )
                    , ( "left", toString boundary.x ++ "px" )
                    , ( "width", toString boundary.wight ++ "px" )
                    , ( "height", toString boundary.height ++ "px" )
                    , ( "border", toString boundary.borderThickness ++ "px solid red" )
                    ]
                ]
                []
    in
    div
        []
        [ viewBoundary
        , viewCircles
        ]


updateError : Msg -> Model -> Payload
updateError msg model =
    case model.error of
        Just msg ->
            model ! []

        Nothing ->
            update msg model


update : Msg -> Model -> Payload
update msg model =
    case msg of
        OnCircleMouseDown circle ->
            selectCircle circle.id model ! []

        OnMouseUp _ ->
            diselectCircles model ! []

        OnMouseMoved mousePosition ->
            setSelectedCircleToMousePosition mousePosition model ! []

        OnFrameUpdate _ ->
            model
                |> moveCircles
                |> updateCircles
                |> deflectCircles_ofCircles
                |> deflectCircles_ofBoundary
                |> (\newModel ->
                        newModel ! []
                   )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.moves OnMouseMoved
        , Mouse.ups OnMouseUp
        , frameUpdated OnFrameUpdate
        ]


moveCircles : Model -> Model
moveCircles model =
    model.circles
        |> List.map moveCircle
        |> (\newCircles -> { model | circles = newCircles })


moveCircle : Circle -> Circle
moveCircle circle =
    if circle.isSelected then
        circle

    else
        { circle
            | x = circle.x + (circle.movementDirection |> degrees |> cos) * circle.movementSpeed
            , y = circle.y - (circle.movementDirection |> degrees |> sin) * circle.movementSpeed
        }


selectCircle : CircleId -> Model -> Model
selectCircle circleId model =
    model.circles
        |> List.map
            (\c ->
                if c.id == circleId then
                    { c | isSelected = True }

                else
                    { c | isSelected = False }
            )
        |> (\newCircles -> { model | circles = newCircles })


diselectCircles : Model -> Model
diselectCircles model =
    model.circles
        |> List.map (\c -> { c | isSelected = False })
        |> (\newCircles -> { model | circles = newCircles })


deflectCircles_ofBoundary : Model -> Model
deflectCircles_ofBoundary model =
    let
        deflectCircle_ofBoundary : Boundary -> Circle -> Circle
        deflectCircle_ofBoundary boundary circle =
            let
                isCircle_above_boundary =
                    if (circle.y - circle.radius) < boundary.y then
                        True

                    else
                        False

                isCircle_leftOf_boundary =
                    if (circle.x - circle.radius) < boundary.x then
                        True

                    else
                        False

                isCircle_rightOf_boundary =
                    if (circle.x + circle.radius) > boundary.x + boundary.wight then
                        True

                    else
                        False

                isCircle_below_boundary =
                    if (circle.y + circle.radius) > boundary.y + boundary.height then
                        True

                    else
                        False

                isCircleInTheCorner =
                    (isCircle_above_boundary && isCircle_leftOf_boundary)
                        || (isCircle_above_boundary && isCircle_rightOf_boundary)
                        || (isCircle_below_boundary && isCircle_rightOf_boundary)
                        || (isCircle_below_boundary && isCircle_leftOf_boundary)

                normaliseAll degree =
                    let
                        normalise degree =
                            Basics.toFloat <| Basics.round degree % 360

                        skip360 degree =
                            if degree == 360 then
                                0

                            else
                                degree
                    in
                    degree |> normalise |> skip360

                reflectDegree_backwards degree =
                    degree |> (+) 180 |> normaliseAll

                reflectDegree_horizontali degree =
                    degree |> negate |> normaliseAll

                reflectDegree_verticali degree =
                    degree |> negate |> (+) 180 |> normaliseAll

                reflectCircle_backwards =
                    { circle | movementDirection = reflectDegree_backwards <| circle.movementDirection }

                reflectCircle_horizontali =
                    { circle | movementDirection = reflectDegree_horizontali <| circle.movementDirection }

                reflectCircle_verticali =
                    { circle | movementDirection = reflectDegree_verticali <| circle.movementDirection }
            in
            if isCircleInTheCorner then
                reflectCircle_backwards

            else if isCircle_leftOf_boundary || isCircle_rightOf_boundary then
                reflectCircle_verticali

            else if isCircle_above_boundary || isCircle_below_boundary then
                reflectCircle_horizontali

            else
                circle

        newCircles =
            List.map (deflectCircle_ofBoundary model.boundary) model.circles
    in
    { model | circles = newCircles }


deflectCircles_ofCircles : Model -> Model
deflectCircles_ofCircles model =
    let
        deflectCircle_ofCircle : List Circle -> Circle -> Circle
        deflectCircle_ofCircle list_circles circle =
            case list_circles of
                [] ->
                    circle

                y :: ys ->
                    let
                        direction_fromP1_toP2 : { x : Float, y : Float } -> { x : Float, y : Float } -> Float
                        direction_fromP1_toP2 p1 p2 =
                            let
                                vector =
                                    { x = p2.x - p1.x
                                    , y = p2.y - p1.y
                                    }

                                radiansToDegree : Float -> Float
                                radiansToDegree rad =
                                    rad * 57.2958

                                vectorDegree =
                                    radiansToDegree <| atan2 vector.y vector.x
                            in
                            vectorDegree

                        checkNextCircle =
                            deflectCircle_ofCircle ys circle

                        sameCircle =
                            y.id == circle.id

                        otherColliderFound =
                            List.member y.id circle.collidedWith

                        swapMovementDirections =
                            { circle | movementDirection = 180 + direction_fromP1_toP2 { x = circle.x, y = -circle.y } { x = y.x, y = -y.y } }
                    in
                    if sameCircle then
                        checkNextCircle

                    else if otherColliderFound then
                        swapMovementDirections

                    else
                        checkNextCircle

        newCircles =
            List.map (deflectCircle_ofCircle model.circles) model.circles
    in
    { model | circles = newCircles }


updateCircles : Model -> Model
updateCircles model =
    model
        |> resetCollidedWith
        |> updateCollidedWith


updateCollidedWith : Model -> Model
updateCollidedWith model =
    let
        newCircles =
            List.foldl
                (\c l ->
                    l
                        |> List.map
                            (\a ->
                                if a.id == c.id then
                                    a

                                else if isCirclesCollided a c then
                                    { a | collidedWith = c.id :: a.collidedWith }

                                else
                                    a
                            )
                )
                model.circles
                model.circles
    in
    { model | circles = newCircles }


resetCollidedWith : Model -> Model
resetCollidedWith model =
    let
        resetedCircles =
            List.map (\c -> { c | collidedWith = [] }) model.circles

        newModel =
            { model | circles = resetedCircles }
    in
    newModel


setSelectedCircleToMousePosition : Mouse.Position -> Model -> Model
setSelectedCircleToMousePosition mousePosition model =
    model.circles
        |> List.filter (\c -> c.isSelected)
        |> (\list ->
                if List.length list > 1 then
                    setError "setSelectedCircleToMousePosition" "Found more then one selected circle" model

                else
                    let
                        newCircles =
                            model.circles
                                |> List.map
                                    (\c ->
                                        if c.isSelected then
                                            { c | x = toFloat mousePosition.x, y = toFloat mousePosition.y }

                                        else
                                            c
                                    )
                    in
                    { model | circles = newCircles }
           )


isCirclesCollided : Circle -> Circle -> Bool
isCirclesCollided c1 c2 =
    let
        radiusSum =
            c1.radius + c2.radius
    in
    radiusSum >= distanceOverCircles c1 c2


distanceOverCircles : Circle -> Circle -> Float
distanceOverCircles c1 c2 =
    distance (Point c1.x c1.y) (Point c2.x c2.y)


distance : Point -> Point -> Float
distance p1 p2 =
    ((p1.x - p2.x) ^ 2) + ((p1.y - p2.y) ^ 2) |> sqrt


setError : String -> String -> Model -> Model
setError location info model =
    let
        msg =
            "Error in \"" ++ location ++ "\". " ++ info
    in
    { model | error = Just msg }
