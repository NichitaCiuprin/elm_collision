port module Main exposing (main)

import ErrorMsg
import Html
import Html.Attributes
import Html.Events
import Mouse exposing (moves)
import Set
import Time


type alias Model =
    { circles : Circles
    , border : Border
    , maybeErrorMsg : Maybe ErrorMsg.ErrorMsg
    }


type Msg
    = OnCircleMouseDown Circle
    | OnMouseUp Mouse.Position
    | OnMouseMoved Mouse.Position
    | OnEveryTick Time.Time


type alias Circles =
    List Circle


type alias Circle =
    { id : Int --Must be unique
    , isSelected : Bool
    , point : Point
    , radius : Float
    , overlapersId : List Id --Contains ids of overlapers. If list is empty then circle is not overlaps with any circle
    , movementDirection : Direction
    , color : Color
    }


type alias Border =
    { point : Point
    , height : Float
    , wight : Float
    , thickness : Float
    }


type alias Point =
    { x : Float, y : Float }


type alias Payload =
    ( Model, Cmd Msg )


type alias Direction =
    Float


type alias Distance =
    Float


type alias Radians =
    Float


type alias Degree =
    Float


type alias Id =
    Int


type alias Speed =
    Float


type alias Color =
    String


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Payload
init =
    let
        model =
            { circles = initCircles
            , border = initBorder
            , maybeErrorMsg = Nothing
            }

        cmd =
            []
    in
    model ! cmd


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.moves OnMouseMoved
        , Mouse.ups OnMouseUp
        , Time.every 10 OnEveryTick
        ]



----------------------------------------------------------------------------------------------------


update : Msg -> Model -> Payload
update msg model =
    let
        end =
            \a -> a ! []
    in
    case msg of
        OnCircleMouseDown circle ->
            model
                --|> selectCircle circle
                |> end

        OnMouseUp _ ->
            model
                --|> diselectCircles
                |> end

        OnMouseMoved mousePosition ->
            model
                --|> moveCircleToMousePosition mousePosition
                |> end

        OnEveryTick _ ->
            model
                --|> deflectCirclesOf_circles
                |> m_findCirclesOverlapers
                |> m_changeCirclesColor
                |> m_deflectCirclesOf_border
                |> m_moveCircles
                |> end


m_moveCircles : Model -> Model
m_moveCircles model =
    { model | circles = moveCircles model.circles }


m_changeCirclesColor : Model -> Model
m_changeCirclesColor model =
    { model | circles = changeCirclesColor model.circles }


m_deflectCirclesOf_border : Model -> Model
m_deflectCirclesOf_border model =
    { model | circles = deflectCirclesOf_border model.border model.circles }


m_findCirclesOverlapers : Model -> Model
m_findCirclesOverlapers model =
    { model | circles = findCirclesOverlapers model.circles }



--m_selectCircle : Model -> Model
--m_selectCircle model =
--    { model | }
----------------------------------------------------------------------------------------------------


view : Model -> Html.Html Msg
view model =
    case model.maybeErrorMsg of
        Just errorMsg ->
            ErrorMsg.view errorMsg

        Nothing ->
            groupHtml
                [ viewBorder model.border
                , viewCircles model.circles
                ]


viewCircles : Circles -> Html.Html Msg
viewCircles circles =
    groupHtml (List.map viewCircle circles)


viewCircle : Circle -> Html.Html Msg
viewCircle circle =
    Html.span
        [ Html.Events.onMouseDown (OnCircleMouseDown circle)
        , Html.Attributes.style
            [ ( "left", toString (circle.point.x - circle.radius) ++ "px" )
            , ( "top", toString (circle.point.y - circle.radius) ++ "px" )
            , ( "height", toString (circle.radius * 2) ++ "px" )
            , ( "width", toString (circle.radius * 2) ++ "px" )
            , ( "background-color", circle.color )
            , ( "position", "absolute" )
            , ( "border-radius", "50%" )
            ]
        ]
        []


viewBorder : Border -> Html.Html a
viewBorder border =
    Html.div
        [ Html.Attributes.style
            [ ( "position", "absolute" )
            , ( "top", toString border.point.y ++ "px" )
            , ( "left", toString border.point.x ++ "px" )
            , ( "width", toString border.wight ++ "px" )
            , ( "height", toString border.height ++ "px" )
            , ( "border", toString border.thickness ++ "px solid red" )
            ]
        ]
        []



----------------------------------------------------------------------------------------------------


initBorder : Border
initBorder =
    Border initPoint 700 1000 5


isCirclesOverlap : Circle -> Circle -> Bool
isCirclesOverlap c1 c2 =
    (c1.radius + c2.radius) >= distanceBetweenCircles c1 c2


distanceBetweenCircles : Circle -> Circle -> Float
distanceBetweenCircles c1 c2 =
    distanceBetwinPoints c1.point c2.point


swapCirlcesDirection : Circle -> Circle -> ( Circle, Circle )
swapCirlcesDirection c1 c2 =
    ( { c1 | movementDirection = c2.movementDirection }
    , { c2 | movementDirection = c1.movementDirection }
    )


deflectCircles_verticali : Circle -> Circle
deflectCircles_verticali circle =
    { circle | movementDirection = reflectDegree_vertiacali circle.movementDirection }


deflectCircles_horizontali : Circle -> Circle
deflectCircles_horizontali circle =
    { circle | movementDirection = reflectDegree_horizontali circle.movementDirection }


circle_y_lowest : Circle -> Float
circle_y_lowest circle =
    circle.point.y - circle.radius


circle_y_bigest : Circle -> Float
circle_y_bigest circle =
    circle.point.y + circle.radius


circle_x_lowest : Circle -> Float
circle_x_lowest circle =
    circle.point.x - circle.radius


circle_x_bigest : Circle -> Float
circle_x_bigest circle =
    circle.point.x + circle.radius


moveCircle : Circle -> Circle
moveCircle circle =
    { circle | point = movePoint circle.movementDirection circleMovementSpeed circle.point }


diselectCircles : Circles -> Circles
diselectCircles circles =
    List.map diselectCircle circles


diselectCircle : Circle -> Circle
diselectCircle circle =
    { circle | isSelected = False }


selectCircle : Circle -> Circle
selectCircle circle =
    { circle | isSelected = True }



--swapCircleById : Circle -> Circles -> Cirlces
--swapCircleById circle circles =


changeCirclesColor : Circles -> Circles
changeCirclesColor circles =
    List.map changeCircleColor circles


changeCircleColor : Circle -> Circle
changeCircleColor circle =
    let
        newColor =
            if isCircleOverlaps circle then
                red

            else
                grey
    in
    { circle | color = newColor }


isCircleOverlaps : Circle -> Bool
isCircleOverlaps circle =
    List.length circle.overlapersId > 0


initPoint : Point
initPoint =
    Point 0 0


movePoint : Direction -> Distance -> Point -> Point
movePoint direction distance point =
    { point
        | x = point.x + (direction |> degrees |> cos) * distance
        , y = point.y - (direction |> degrees |> sin) * distance
    }


distanceBetwinPoints : Point -> Point -> Distance
distanceBetwinPoints p1 p2 =
    ((p1.x - p2.x) ^ 2) + ((p1.y - p2.y) ^ 2) |> sqrt


pointsToDirection : Point -> Point -> Direction
pointsToDirection p1 p2 =
    let
        vector =
            { x = p2.x - p1.x
            , y = p2.y - p1.y
            }

        vectorDegree =
            radiansToDegree <| atan2 vector.y vector.x
    in
    -vectorDegree


pointToMousePosition : Mouse.Position -> Point
pointToMousePosition mousePosition =
    { x = Basics.toFloat mousePosition.x
    , y = Basics.toFloat mousePosition.y
    }


findCirclesOverlapers : Circles -> Circles
findCirclesOverlapers circles =
    circles
        |> List.map (\a -> { a | overlapersId = [] })
        |> mapCombination
            (\a b ->
                if isCirclesOverlap a b then
                    ( { a | overlapersId = b.id :: a.overlapersId }
                    , { b | overlapersId = a.id :: b.overlapersId }
                    )

                else
                    ( a, b )
            )


moveCircles : Circles -> Circles
moveCircles circles =
    List.map moveCircle circles


initCircles : Circles
initCircles =
    [ Circle 1 False (Point 100 100) 50 [] 20 grey
    , Circle 2 False (Point 200 200) 50 [] 0 grey
    , Circle 3 False (Point 300 300) 50 [] 0 grey
    , Circle 4 False (Point 400 400) 50 [] 0 grey
    ]


deflectCirclesOf_border : Border -> Circles -> Circles
deflectCirclesOf_border border circles =
    List.map (deflectCircleOf_border border) circles


deflectCircleOf_border : Border -> Circle -> Circle
deflectCircleOf_border border circle =
    if isCircle_aboveBorder border circle || isCircle_belowBorder border circle then
        deflectCircles_horizontali circle

    else if isCircle_leftOfBorder border circle || isCircle_rightOfBorder border circle then
        deflectCircles_verticali circle

    else
        circle


isCircle_aboveBorder : Border -> Circle -> Bool
isCircle_aboveBorder border circle =
    if circle_y_lowest circle < border.point.y then
        True

    else
        False


isCircle_leftOfBorder : Border -> Circle -> Bool
isCircle_leftOfBorder border circle =
    if circle_x_lowest circle < border.point.x then
        True

    else
        False


isCircle_belowBorder : Border -> Circle -> Bool
isCircle_belowBorder border circle =
    if circle_y_bigest circle > border.height then
        True

    else
        False


isCircle_rightOfBorder : Border -> Circle -> Bool
isCircle_rightOfBorder border circle =
    if circle_x_bigest circle > border.wight then
        True

    else
        False


mapCombination : (a -> a -> ( a, a )) -> List a -> List a
mapCombination f items =
    case items of
        [] ->
            items

        x :: xs ->
            createMapCombinationDto x xs
                |> updateMapCombinationDto f
                |> mapCombinationDtoToList
                |> nextCombination f


createMapCombinationDto : a -> b -> { head : a, tail_new : List c, tail_old : b }
createMapCombinationDto head tail =
    { head = head
    , tail_old = tail
    , tail_new = []
    }


mapCombinationDtoToList : { b | head : a, tail_new : List a } -> List a
mapCombinationDtoToList dto =
    dto.head :: dto.tail_new


nextCombination : (a -> a -> ( a, a )) -> List a -> List a
nextCombination f items =
    case items of
        [] ->
            items

        x :: xs ->
            x :: mapCombination f xs


updateMapCombinationDto :
    (a -> b -> ( a, c ))
    -> { d | head : a, tail_new : List c, tail_old : List b }
    -> { d | head : a, tail_new : List c, tail_old : List b }
updateMapCombinationDto f2 dto =
    case dto.tail_old of
        [] ->
            dto

        x :: xs ->
            let
                tempTuple =
                    f2 dto.head x
            in
            { dto
                | head = Tuple.first tempTuple
                , tail_old = xs
                , tail_new = dto.tail_new ++ [ Tuple.second tempTuple ]
            }
                |> updateMapCombinationDto f2


findAvailableId : Set.Set Id -> Id
findAvailableId idsInUse =
    let
        f1 int idsInUse =
            if Set.member int idsInUse then
                f1 (int + 1) idsInUse

            else
                int
    in
    f1 0 idsInUse


normaliseDegree : Degree -> Degree
normaliseDegree degree =
    degree
        |> Basics.round
        |> (%) 360
        |> Basics.toFloat


reflectDegree : Degree -> Degree -> Degree
reflectDegree measure degree =
    -degree + (measure * 2)


reflectDegree_backwards : Degree -> Degree
reflectDegree_backwards degree =
    reflectDegree degree degree


reflectDegree_vertiacali : Degree -> Degree
reflectDegree_vertiacali degree =
    reflectDegree 90 degree


reflectDegree_horizontali : Degree -> Degree
reflectDegree_horizontali degree =
    reflectDegree 0 degree


radiansToDegree : Radians -> Float
radiansToDegree rad =
    rad * 57.2958


groupHtml : List (Html.Html a) -> Html.Html a
groupHtml items =
    Html.div [] items


circleMovementSpeed : Speed
circleMovementSpeed =
    10


red : Color
red =
    "#FF0000"


grey : Color
grey =
    "#737373"



{-
   mapCombination_test1 : Model -> Model
   mapCombination_test1 model =
       let
           expectedResult = [ "a111", "b111", "c111", "d111" ]
           actualResult = mapCombination (\a b -> ( a ++ "1", b ++ "1" )) [ "a", "b", "c", "d" ]
       in
       if expectedResult == actualResult then model else setError_test "test1" expectedResult actualResult model
-}
