port module Main exposing (main)

import Html exposing (canvas, div, p, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onMouseDown)
import Mouse exposing (moves)
import Time
import Set


type alias Direction = Float

type alias Payload = ( Model, Cmd Msg )

type alias CircleId = Int

type alias Point =
    { x : Float
    , y : Float
    }

type alias Model =
    { circles : List Circle
    , boundary : Boundary
    , error : Maybe String
    }

type alias Circle =
    { id : Int
    , point : Point
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
    | OnEveryMilisecconds Time.Time



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
    let
        setError_test testName expectedResult actualResult model =
            setError testName ("Test failed. " ++ "Expected result: " ++ toString expectedResult ++ ". Actual result: " ++ toString actualResult) model

        test1 model =
            let
                expectedResult = [ "a111", "b111", "c111", "d111" ]
                actualResult = mapCombination (\a b -> ( a ++ "1", b ++ "1" )) [ "a", "b", "c", "d" ]
            in
            if expectedResult == actualResult then model
            else setError_test "test1" expectedResult actualResult model
    in
    ({ circles =
        [ Circle 1 (Point 100 200) 50 False [] 240 circlesSpeed
        , Circle 2 (Point 200 300) 50 False [] 90  circlesSpeed
        , Circle 3 (Point 300 400) 50 False [] 45  circlesSpeed
        , Circle 4 (Point 400 500) 50 False [] 45  circlesSpeed
        ]
     , boundary = Boundary 0 0 700 1000 5
     , error = Nothing
     }
        |> test1
    )
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
            div [] (List.map viewCircle model.circles)

        viewBoundary =
            let boundary = model.boundary in
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

viewCircle : Circle -> Html.Html Msg
viewCircle circle =
    span
        [ onMouseDown (OnCircleMouseDown circle)
        , style
            [ ( "left", toString (circle.point.x - circle.radius) ++ "px" )
            , ( "top", toString (circle.point.y - circle.radius) ++ "px" )
            , ( "height", toString (circle.radius * 2) ++ "px" )
            , ( "width", toString (circle.radius * 2) ++ "px" )
            , ( "background-color", if List.length circle.collidedWith > 0 then "#FF0000" else "#737373" )
            , ( "position", "absolute" )
            , ( "border-radius", "50%" )
            ]
        ]
        []


updateError : Msg -> Model -> Payload
updateError msg model =
    case model.error of
        Just msg ->
            model ! []

        Nothing ->
            update msg model


update : Msg -> Model -> Payload
update msg model =
    let end = \a -> a ! [] in
    case msg of
        OnCircleMouseDown circle ->
            model
                |> selectCircle circle.id
                |> end

        OnMouseUp _ ->
            model
                |> diselectCircles
                |> end

        OnMouseMoved mousePosition ->
            model
                |> moveCircleToMousePosition mousePosition
                |> end

        OnEveryMilisecconds _ ->
            model
                |> moveCircles
                |> updateCircles
                |> deflectCircles_ofCircles
                |> deflectCircles_ofBoundary
                |> end


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.moves OnMouseMoved
        , Mouse.ups OnMouseUp
        , Time.every 10 OnEveryMilisecconds
        ]


moveCircles : Model -> Model
moveCircles model =
    model.circles
        |> List.map updateCirclePosition
        |> (\newCircles -> { model | circles = newCircles })


updateCirclePosition : Circle -> Circle
updateCirclePosition circle =
    if circle.isSelected then circle
    else { circle | point = movePoint circle.movementDirection circle.movementSpeed circle.point }


diselectCircles : Model -> Model
diselectCircles model =
    model.circles
        |> List.map (\c -> { c | isSelected = False })
        |> (\c -> { model | circles = c })


deflectCircles_ofBoundary : Model -> Model
deflectCircles_ofBoundary model =
    let
        deflectCircle_ofBoundary : Boundary -> Circle -> Circle
        deflectCircle_ofBoundary boundary circle =
            let
                isCircle_above_boundary =
                    if (circle.point.y - circle.radius) < boundary.y then
                        True

                    else
                        False

                isCircle_leftOf_boundary =
                    if (circle.point.x - circle.radius) < boundary.x then
                        True

                    else
                        False

                isCircle_rightOf_boundary =
                    if (circle.point.x + circle.radius) > boundary.x + boundary.wight then
                        True

                    else
                        False

                isCircle_below_boundary =
                    if (circle.point.y + circle.radius) > boundary.y + boundary.height then
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



--TODO handle multiple collisions in same time


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
                        checkNextCircle =
                            deflectCircle_ofCircle ys circle

                        sameCircle =
                            y.id == circle.id

                        otherColliderFound =
                            List.member y.id circle.collidedWith

                        swapMovementDirections =
                            { circle | movementDirection = 180 + direction circle.point y.point }
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
    newCircles
        |> (\a -> { model | circles = a })


updateCircles : Model -> Model
updateCircles model =
    model
        |> resetCollidedWith
        |> updateCollidedWith


swapDirection : Circle -> Circle -> ( Circle, Circle )
swapDirection c1 c2 =
    ( { c1 | movementDirection = c2.movementDirection }
    , { c2 | movementDirection = c1.movementDirection }
    )



updateCollidedWith : Model -> Model
updateCollidedWith model =
    model.circles
        |> mapCombination
            (\a b ->
                if isCirclesCollided a b then
                    ( { a | collidedWith = b.id :: a.collidedWith }
                    , { b | collidedWith = a.id :: b.collidedWith }
                    )
                else
                    ( a, b )
            )
        |> (\a -> { model | circles = a })



resetCollidedWith : Model -> Model
resetCollidedWith model =
    model.circles
        |> List.map (\a -> { a | collidedWith = [] })
        |> (\a -> { model | circles = a })


moveCircleToMousePosition : Mouse.Position -> Model -> Model
moveCircleToMousePosition mousePosition model =
    model.circles
        |> List.map
            (\a -> 
                if a.isSelected then { a | point = mousePositionToPoint mousePosition }
                else a)
        |> (\a -> { model | circles = a })


isCirclesCollided : Circle -> Circle -> Bool
isCirclesCollided c1 c2 =
    (c1.radius + c2.radius) >= circlesDistance c1 c2


circlesDistance : Circle -> Circle -> Float
circlesDistance c1 c2 = distance c1.point c2.point

direction : Point -> Point -> Float
direction p1 p2 =
    let
        vector =
            { x = p2.x - p1.x
            , y = p2.y - p1.y
            }

        vectorDegree =
            radiansToDegree <| atan2 vector.y vector.x
    in
    -vectorDegree


distance : Point -> Point -> Float
distance p1 p2 =
    ((p1.x - p2.x) ^ 2) + ((p1.y - p2.y) ^ 2) |> sqrt


setError : String -> String -> Model -> Model
setError location info model =
    ("Error in \"" ++ location ++ "\". " ++ info)
        |> Just
        |> (\a -> { model | error = a })


mousePositionToPoint : Mouse.Position -> Point
mousePositionToPoint mousePosition =
    Point
        (Basics.toFloat mousePosition.x)
        (Basics.toFloat mousePosition.y)



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


movePoint : Direction -> Float -> Point -> Point
movePoint dir float point =
    { point
        | x = point.x + (dir |> degrees |> cos) * float
        , y = point.y - (dir |> degrees |> sin) * float
    }


mapCombination : (a -> a -> ( a, a )) -> List a -> List a
mapCombination f items =
    let
        createDto head tail =
            { head = head
            , tail_old = tail
            , tail_new = []
            }

        updateDto f2 dto =
            case dto.tail_old of
                [] -> dto
                x::xs ->
                    let tempTuple = f2 dto.head x in
                    { dto 
                        | head = Tuple.first tempTuple
                        , tail_old = xs
                        , tail_new = dto.tail_new ++ [Tuple.second tempTuple]
                    }
                    |> updateDto f2

        dtoToList dto = dto.head :: dto.tail_new

        nextCombination items2 = 
            case items2 of
                [] -> items2
                x::xs -> x :: (mapCombination f xs)
    in
    case items of
        [] -> items
        x :: xs -> 
            createDto x xs 
            |> updateDto f 
            |> dtoToList 
            |> nextCombination



f1 : (a -> { head : a, tail : List a } -> { head : a, tail : List a }) -> List a -> List a
f1 f items =
    case items of
        [] -> items
        x :: xs -> 
            List.foldl f {head = x, tail = []} xs 
            |> (\a -> a.head :: a.tail)


radiansToDegree : Float -> Float
radiansToDegree rad = rad * 57.2958

circlesSpeed : Float
circlesSpeed = 3