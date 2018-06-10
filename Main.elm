port module Main exposing (..) 

import Html exposing (text, div, span, p, canvas)
import Html.Attributes exposing (..)
import Html.Events exposing (onMouseDown)
import Mouse exposing (moves)

port frameUpdated : (String -> msg) -> Sub msg 

type alias Model =
    { circles : List Circle
    , boundary : Boundary
    }
type alias Circle =
    { id : Int
    , x : Float
    , y : Float 
    , radius : Float
    , isSelected : Bool
    , otherCollidersId : List Int --if value 0 then circle is not collided
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
        { init = (initModel , Cmd.none)
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

initModel : Model  
initModel = 
    { circles = 
        [ Circle 1 1000 200 50 False [] 270 20
        , Circle 2 1000 400 50 False [] 90 20
        , Circle 3 1400 400 50 False [] 45 20
        , Circle 4 1600 600 50 False [] 45 20
        --, Circle 5 1600 600 50 False [] 45 20
        --, Circle 6 1600 600 50 False [] 45 20
        --, Circle 7 1600 600 50 False [] 45 20
        --, Circle 8 1600 600 50 False [] 45 20
        --, Circle 9 1600 600 50 False [] 45 20
        ]
    , boundary = Boundary 700 100 700 1000 5
    }

view : Model -> Html.Html Msg 
view model =
    div 
        [] 
        [ model |> print 
        , model |> viewBoundary 
        , model |> viewCircles 
        ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of 
        OnCircleMouseDown circle ->
            ( model 
                |> selectCircle circle.id 
            , Cmd.none 
            )

        OnMouseUp _ ->
            ( model 
                |> diselectCircles 
            , Cmd.none 
            )

        OnMouseMoved mousePosition ->
            ( model 
                |> setSelectedCircleToMousePosition mousePosition  
            , Cmd.none 
            )

        OnFrameUpdate _ ->
            ( model 
                |> moveCircles 
                |> manage_otherColliderId 
                |> deflectCircles_ofCircles 
                |> deflectCircles_ofBoundary
            , Cmd.none 
            )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.moves OnMouseMoved
        , Mouse.ups OnMouseUp
        , frameUpdated OnFrameUpdate
        ]

------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

viewCircles : Model -> Html.Html Msg
viewCircles model =
    let
        whiteOrRed : Circle -> String
        whiteOrRed circle =
        if (List.length circle.otherCollidersId) > 0 then "#FF0000" else "#737373"

        viewCircle : Circle -> Html.Html Msg
        viewCircle circle =
            span 
                [ onMouseDown (OnCircleMouseDown circle)
                , style 
                    [ ("left",   toString (circle.x - circle.radius) ++ "px")
                    , ("top",    toString (circle.y - circle.radius) ++ "px")
                    , ("height", toString (circle.radius * 2)        ++ "px")
                    , ("width",  toString (circle.radius * 2)        ++ "px")
                    , ("background-color" , whiteOrRed circle)
                    , ("position" , "absolute")
                    , ("border-radius" , "50%")
                    ]
                ]
                [ --span [style 
                  --  [ ("left",   toString (circle.x - circle.radius) ++ "px")] ] [text <| toString <| circle.id]
                ]       
    in
        div [] (List.map viewCircle model.circles)

viewBoundary : Model -> Html.Html Msg
viewBoundary model = 
    let
        boundary = model.boundary
        correctedPosition_left  = (boundary.x - boundary.borderThickness)
        correctedPosition_right = (boundary.y - boundary.borderThickness)
    in
        div 
        [ style 
            [ ("border", toString boundary.borderThickness ++ "px solid red") 
            , ("position","absolute") 
            , ("height", toString boundary.height        ++ "px") 
            , ("width",  toString boundary.wight         ++ "px")
            , ("left",   toString correctedPosition_left  ++ "px")
            , ("top",    toString correctedPosition_right ++ "px")
            ]
        ] 
        []

print : Model -> Html.Html Msg
print model =
    let 
        direction_fromP1_toP2 : {x:Float,y:Float} -> {x:Float,y:Float} -> Float
        direction_fromP1_toP2 p1 p2 = 
            let
                vector = 
                    { x = p2.x - p1.x 
                    , y = p2.y - p1.y
                    }

                radiansToDegree : Float -> Float
                radiansToDegree rad = rad * 57.2958

                vectorDegree = radiansToDegree <| (atan2 vector.y vector.x)
            in
                vectorDegree

        justCircleToCircle justCircle = 
            case justCircle of
                Just circle ->
                    circle
                Nothing ->
                    Circle 0 0 0 0 False [] 0 0 
        
        printCircle circle = 
            let
                s = style [ ("margin", "0px" ) ] 
            in
                div
                    [] 
                    [ p [s] [ text <| toString <| circle.id ]
                    , p [s] [ text <| toString <| circle.x ]
                    , p [s] [ text <| toString <| circle.y ]
                    , p [s] [ text <| toString <| circle.radius ]
                    , p [s] [ text <| toString <| circle.isSelected ]
                    , p [s] [ text <| toString <| circle.otherCollidersId ]
                    , p [s] [ text <| toString <| circle.movementDirection ]
                    , p [s] [ text <| toString <| circle.movementSpeed ]
                    , p [s] [ text <| toString <| "---------------------------" ]
                    ]
    in
        div 
            [] 
            [ --printCircle <| justCircleToCircle <| List.head <| List.take 1 model.circles 
            --, printCircle <| justCircleToCircle <| List.head <| List.take 1 <| List.drop 1 <| model.circles 
            ]

deflectCircles_ofBoundary : Model -> Model
deflectCircles_ofBoundary model =
  let
    deflectCircle_ofBoundary : Boundary -> Circle -> Circle
    deflectCircle_ofBoundary boundary circle =
    let 
        isCircle_above_boundary   = if (circle.y - circle.radius) < boundary.y                   then True else False
        isCircle_leftOf_boundary  = if (circle.x - circle.radius) < boundary.x                   then True else False
        isCircle_rightOf_boundary = if (circle.x + circle.radius) > boundary.x + boundary.wight  then True else False
        isCircle_below_boundary   = if (circle.y + circle.radius) > boundary.y + boundary.height then True else False

        isCircleIn_leftUp_corner    = isCircle_above_boundary && isCircle_leftOf_boundary  
        isCircleIn_upRight_corner   = isCircle_above_boundary && isCircle_rightOf_boundary  
        isCircleIn_rightDown_corner = isCircle_below_boundary && isCircle_rightOf_boundary  
        isCircleIn_downLeft_corner  = isCircle_below_boundary && isCircle_leftOf_boundary  

        isCircleInTheCorner = ( isCircle_above_boundary && isCircle_leftOf_boundary  ) ||  
                              ( isCircle_above_boundary && isCircle_rightOf_boundary ) ||
                              ( isCircle_below_boundary && isCircle_rightOf_boundary ) ||
                              ( isCircle_below_boundary && isCircle_leftOf_boundary  )

        normalise    degree = Basics.toFloat <| (Basics.round degree) % 360
        skip360      degree = if degree == 360 then 0 else degree
        normaliseAll degree = degree |> normalise |> skip360

        reflectDegree_backwards   degree = degree |> (+) 180           |> normaliseAll
        reflectDegree_horizontali degree = degree |> negate            |> normaliseAll
        reflectDegree_verticali   degree = degree |> negate |> (+) 180 |> normaliseAll

        reflectCircle_backwards   = { circle | movementDirection = reflectDegree_backwards   <| circle.movementDirection }
        reflectCircle_horizontali = { circle | movementDirection = reflectDegree_horizontali <| circle.movementDirection }
        reflectCircle_verticali   = { circle | movementDirection = reflectDegree_verticali   <| circle.movementDirection }
    in
        if      isCircleInTheCorner                                   then reflectCircle_backwards
        else if isCircle_leftOf_boundary || isCircle_rightOf_boundary then reflectCircle_verticali
        else if isCircle_above_boundary  || isCircle_below_boundary   then reflectCircle_horizontali
        else    circle

    newCircles = List.map (deflectCircle_ofBoundary model.boundary) model.circles
    newModel = { model | circles = newCircles }
  in
    newModel

deflectCircles_ofCircles : Model -> Model
deflectCircles_ofCircles model =
  let
    deflectCircle_ofCircle : List Circle -> Circle -> Circle
    deflectCircle_ofCircle list_circles circle = 
        case list_circles of
            [] -> circle
            (y::ys) -> 
            let
                direction_fromP1_toP2 : {x:Float,y:Float} -> {x:Float,y:Float} -> Float
                direction_fromP1_toP2 p1 p2 = 
                    let
                        vector = 
                            { x = p2.x - p1.x 
                            , y = p2.y - p1.y
                            }

                        radiansToDegree : Float -> Float
                        radiansToDegree rad = rad * 57.2958

                        vectorDegree = radiansToDegree <| (atan2 vector.y vector.x)
                    in
                        vectorDegree
                        
                checkNextCircle = deflectCircle_ofCircle ys circle
                sameCircle = y.id == circle.id
                otherColliderFound = List.member y.id circle.otherCollidersId 
                swapMovementDirections = 
                    { circle | movementDirection = 180 + ( direction_fromP1_toP2 {x = circle.x, y =  -circle.y} {x = y.x, y = -y.y} ) }
            in
                if      sameCircle         then checkNextCircle
                else if otherColliderFound then swapMovementDirections
                else    checkNextCircle
            
    newCircles = List.map (deflectCircle_ofCircle model.circles) model.circles 
    newModel = { model | circles = newCircles }
  in
    newModel

moveCircles : Model -> Model
moveCircles model = 
    let
        map item =  
            { item 
                | x = item.x + (item.movementDirection |> degrees |> cos) * item.movementSpeed
                , y = item.y - (item.movementDirection |> degrees |> sin) * item.movementSpeed
            } 

        newCircles = List.map map model.circles
        newModel = { model | circles = newCircles }
    in
        newModel

selectCircle : Int -> Model -> Model
selectCircle circleIdToSelect model =
    let
        map : Circle -> Circle
        map circle =
            if circle.id == circleIdToSelect then
                { circle | isSelected = True }
            else
                circle
        
        newCircles = List.map map model.circles 
        newModel = { model | circles = newCircles }
    in
        newModel

diselectCircles : Model -> Model
diselectCircles model =
    let
        map : Circle -> Circle
        map circle = { circle | isSelected = False }

        newCircles = List.map map model.circles 
        newModel = { model | circles = newCircles }
    in
        newModel
 
setSelectedCircleToMousePosition : Mouse.Position -> Model -> Model
setSelectedCircleToMousePosition mousePosition model = 
    let
        setSelectedCircleToMousePosition2 : Mouse.Position -> List Circle -> List Circle
        setSelectedCircleToMousePosition2 mousePosition circles = 
            case circles of
                [] -> circles
                (x::xs) -> 
                    if x.isSelected then 
                        { x | x = toFloat mousePosition.x , y = toFloat mousePosition.y } :: xs 
                    else 
                        x :: (setSelectedCircleToMousePosition2 mousePosition xs)
                        
        newCircles = setSelectedCircleToMousePosition2 mousePosition model.circles
        newModel = { model | circles = newCircles }
    in
        newModel

manage_otherColliderId : Model -> Model
manage_otherColliderId model = 
    let
        f2 : Circle -> List Circle -> List Circle -> List Circle 
        f2 c1 rest addTo =
            case rest of 
                [] -> c1 :: addTo
                (y::ys) -> 
                    let
                        add_c1    = { c1 | otherCollidersId = (y.id  :: c1.otherCollidersId) }
                        add_y     = { y  | otherCollidersId = (c1.id :: y.otherCollidersId)  }
                        remove_c1 = { c1 | otherCollidersId = List.filter (\id -> id /= y.id) c1.otherCollidersId }
                        remove_y  = { y  | otherCollidersId = List.filter (\id -> id /= c1.id) y.otherCollidersId }

                        is_c1_contains_y = List.member y.id c1.otherCollidersId
                        is_y_contains_c1 = List.member c1.id y.otherCollidersId

                        collided = isTwoCirclesCollided c1 y 
                        collidedInPreviousFrame = (is_c1_contains_y && is_y_contains_c1)

                        removeIds = f2 remove_y ys (remove_c1 :: addTo)
                        doNothing = f2 y        ys (c1        :: addTo)
                        addIds =    f2 add_y    ys (add_c1    :: addTo)
                    in
                        if collided then
                            if collidedInPreviousFrame then doNothing
                            else addIds
                        else removeIds

        f1 : List Circle -> List Circle
        f1 circles = 
            case circles of 
                [] -> []
                [x] -> [x]
                (x::xs) ->
                    let
                        newList = (f2 x xs [])
                    in
                        case newList of
                            [] -> []
                            (x::xs) ->
                                x :: (f1 xs)
    in
        { model | circles = f1 model.circles }


isTwoCirclesCollided : Circle -> Circle -> Bool
isTwoCirclesCollided c1 c2 = 
  let
    distance = sqrt <| (+) ((c1.x - c2.x) ^ 2) ((c1.y - c2.y) ^ 2)
    radiusSum = c1.radius + c2.radius
  in
    radiusSum >= distance 

