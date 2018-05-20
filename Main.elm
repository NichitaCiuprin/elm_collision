{-

view
├───print
├───circlesToHTML
└───boundaryToHTML

update
├───OnMilisecond
├───OnCircleMouseDown
│   └───selectCircle
│
├───OnMouseUp
│   └───diselectCircles
│
├───OnMouseMoves
│   └───setSelectedCirclesToMousePosition
│   
└───OnFrameUpdate
    ├───markCollidedCircles 
    │   ├───setCircleIsCollided
    │   │   └───isCircleCollided
    │   │       └───isTwoCirclesCollided
    │   │ 
    │   └───setCircleOtherColliderId
    │ 
    ├───moveCircles 
    └───deflectCircles
        └───deflectCircle
            ├───reflectCircle_right
            ├───reflectCircle_left
            ├───isCircle_leftOf_boundary 
            ├───isCircle_rightOf_boundary
            ├───isCircle_above_boundary  
            └───isCircle_below_boundary              

justToCircle
reflectDegree
-}
port module Main exposing (..) 

import Html exposing (text, div, span, p, canvas)
import Html.Attributes exposing (..)
import Html.Events exposing (onMouseDown)
import Time exposing (Time, second)
import Mouse exposing (moves)

port frameUpdated : (String -> msg) -> Sub msg 

type alias Model =
  { circlesList : List Circle
  , boundary : Boundary
  }

type alias Circle =
  { id : Int
  , x : Float
  , y : Float 
  , radius : Float
  , isSelected : Bool
  , otherColliderId : Int --if value 0 then circle is not collided
  , movementDirection : Float
  , movementSpeed : Float
  }

type alias Boundary =
  { x : Float
  , y : Float
  , height : Float
  , wight : Float
  }

type Msg 
  = OnMilisecond Time
  | OnCircleMouseDown Circle
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
  { circlesList = 
    [ Circle 1 800 200 50 False 0 135 20
    , Circle 2 1000 250 50 False 0 -135 20 
    , Circle 3 1400 400 50 False 0 45 20 
    ]
  , boundary = Boundary 700 100 700 1000
  }

view : Model -> Html.Html Msg 
view model =
  let 
    circle = justToCircle <| List.head model.circlesList
  in
    div 
      [] 
      [ print <| toString <| model.circlesList
      , boundaryToHTML model.boundary 
      , circlesToHTML model.circlesList
      ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of 
    OnMilisecond _ -> 
      (model, Cmd.none)

    OnCircleMouseDown circle ->
      ( selectCircle model circle , Cmd.none )

    OnMouseUp _ ->
      ( diselectCircles model , Cmd.none )

    OnMouseMoved mousePosition ->
      ( setSelectedCirclesToMousePosition model mousePosition , Cmd.none )

    OnFrameUpdate _ ->
      ( model 
        |> markCollidedCircles 
        |> deflectCircles_ofBoundary
        |> deflectCircles_ofCircles
        |> moveCircles 
      , Cmd.none 
      )

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Time.every Time.millisecond OnMilisecond 
    , Mouse.moves OnMouseMoved
    , Mouse.ups OnMouseUp
    , frameUpdated OnFrameUpdate
    ]

--////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

circlesToHTML : List Circle -> Html.Html Msg
circlesToHTML listOfCircles =
  let
    whiteOrRed : Circle -> String
    whiteOrRed circle =
      if circle.otherColliderId > 0 then "#FF0000" else "#737373"

    circleToHTML : Circle -> Html.Html Msg
    circleToHTML circle =
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
        []       
  in
   div [] ( List.map circleToHTML listOfCircles )

boundaryToHTML : Boundary -> Html.Html Msg
boundaryToHTML boundary = 
  let 
    borderThickness = 5
  in
  div 
    [ style 
      [ ("border", toString borderThickness ++ "px solid red") 
      , ("position","absolute") 
      , ("height", toString boundary.height  ++ "px") 
      , ("width",  toString boundary.wight   ++ "px")
      , ("left",   toString (boundary.x - borderThickness) ++ "px")
      , ("top",    toString (boundary.y - borderThickness) ++ "px")
      ]
    ] 
    []

print : String -> Html.Html Msg
print log =
  p [] [ text log ]

deflectCircles_ofBoundary : Model -> Model
deflectCircles_ofBoundary model =
  let
    map = deflectCircle_ofBoundary model.boundary
  in
    { model | circlesList = List.map map model.circlesList } 

deflectCircles_ofCircles : Model -> Model
deflectCircles_ofCircles model =
  let
    map = deflectCircle_ofCircle model.circlesList
  in
    { model | circlesList = List.map map model.circlesList } 


deflectCircle_ofBoundary : Boundary -> Circle -> Circle
deflectCircle_ofBoundary boundary circle =
  if      isCircleIn_upRight_corner    circle boundary then reflectCircleBackward     circle
  else if isCircleIn_rightDown_corner  circle boundary then reflectCircleBackward     circle
  else if isCircleIn_downLeft_corner   circle boundary then reflectCircleBackward     circle
  else if isCircleIn_leftUp_corner     circle boundary then reflectCircleBackward     circle

  else if isCircle_leftOf_boundary  circle boundary then reflectCircle_verticali   circle
  else if isCircle_rightOf_boundary circle boundary then reflectCircle_verticali   circle
  else if isCircle_above_boundary   circle boundary then reflectCircle_horizontali circle
  else if isCircle_below_boundary   circle boundary then reflectCircle_horizontali circle
  else    circle

reflectCircleBackward : Circle -> Circle
reflectCircleBackward circle =
  { circle | movementDirection = circle.movementDirection + 180}

deflectCircle_ofCircle : List Circle -> Circle -> Circle
deflectCircle_ofCircle list_circles circle = 
  case list_circles of
    [] -> circle
    (x::xs) -> 
      let
        checkNextCircle = deflectCircle_ofCircle xs circle
        sameCircle = x.id == circle.id
        otherColliderFound = circle.otherColliderId == x.id
        swapMovementDirections = { circle | movementDirection = x.movementDirection }
      in
        if      sameCircle         then checkNextCircle
        else if otherColliderFound then swapMovementDirections
        else    checkNextCircle





reflectCircle_verticali : Circle -> Circle
reflectCircle_verticali circle =
  { circle | movementDirection = reflectDegree circle.movementDirection True }

reflectCircle_horizontali : Circle -> Circle
reflectCircle_horizontali circle =
  { circle | movementDirection = reflectDegree circle.movementDirection False }

reflectDegree : Float -> Bool -> Float
reflectDegree degree isVertically =
  let 
    piToAdd = if isVertically then 180 else 0
    reflected = negate degree
    normalised = Basics.toFloat <| (Basics.round reflected) % 360
    result = normalised + piToAdd
  in
    if result == 360 then 0
    else result

isCircleIn_leftUp_corner : Circle -> Boundary -> Bool 
isCircleIn_leftUp_corner circle boundary = 
  isCircle_above_boundary circle boundary &&
  isCircle_leftOf_boundary circle boundary 

isCircleIn_upRight_corner : Circle -> Boundary -> Bool 
isCircleIn_upRight_corner circle boundary = 
  isCircle_above_boundary circle boundary &&
  isCircle_rightOf_boundary circle boundary 

isCircleIn_rightDown_corner : Circle -> Boundary -> Bool 
isCircleIn_rightDown_corner circle boundary = 
  isCircle_below_boundary circle boundary &&
  isCircle_rightOf_boundary circle boundary 

isCircleIn_downLeft_corner : Circle -> Boundary -> Bool 
isCircleIn_downLeft_corner circle boundary = 
  isCircle_below_boundary circle boundary &&
  isCircle_leftOf_boundary circle boundary   

isCircle_below_boundary : Circle -> Boundary -> Bool
isCircle_below_boundary circle boundary = 
  if (circle.y + circle.radius) > boundary.y + boundary.height then True else False

isCircle_above_boundary : Circle -> Boundary -> Bool
isCircle_above_boundary circle boundary = 
  if (circle.y - circle.radius) < boundary.y then True else False

isCircle_leftOf_boundary : Circle -> Boundary -> Bool
isCircle_leftOf_boundary circle boundary = 
  if (circle.x - circle.radius) < boundary.x then True else False

isCircle_rightOf_boundary : Circle -> Boundary -> Bool
isCircle_rightOf_boundary circle boundary = 
  if (circle.x + circle.radius) > boundary.x + boundary.wight then True else False

justToCircle : Maybe Circle -> Circle
justToCircle justCircle = 
  case justCircle of
    Just justCircle -> justCircle
    Nothing -> Circle 0 0 0 0 False 0 0 0

moveCircles : Model -> Model
moveCircles model = 
  let
    map : Circle -> Circle
    map item =  
      { item 
        | x = item.x + (item.movementDirection |> degrees |> cos) * item.movementSpeed
        , y = item.y - (item.movementDirection |> degrees |> sin) * item.movementSpeed
      } 
  in
    { model | circlesList = List.map map model.circlesList }

markCollidedCircles : Model -> Model
markCollidedCircles model = 
  { model | circlesList = List.map (setOtherColliderId model.circlesList) model.circlesList }

setOtherColliderId : List Circle -> Circle -> Circle
setOtherColliderId list_circles circle = 
  { circle | otherColliderId = getCircleOtherColliderId list_circles circle } 


selectCircle : Model -> Circle -> Model
selectCircle model circle =
  let
    map : Circle -> Circle
    map x =
      if x.id == circle.id then
          { x | isSelected = True }
      else
          x
  in
    { model | circlesList = List.map map model.circlesList }

diselectCircles : Model -> Model
diselectCircles model =
  let
    map : Circle -> Circle
    map x =
      { x | isSelected = False }
  in
    { model | circlesList = List.map map model.circlesList }

setSelectedCirclesToMousePosition : Model -> Mouse.Position -> Model
setSelectedCirclesToMousePosition model mousePosition = 
  let
    map : Circle -> Circle
    map x =
    if x.isSelected then
      { x | x = toFloat mousePosition.x , y = toFloat mousePosition.y }
    else
      x
  in
    { model | circlesList = List.map map model.circlesList }

--isCircleCollided : List Circle -> Circle -> Int
--isCircleCollided list_circle circle = 
--  case list_circle of
--    [] -> False
--    (x::xs) -> 
--      if x.id == circle.id then 
--        isCircleCollided xs circle
--      else
--        if isTwoCirclesCollided x circle then
--          True
--        else
--          isCircleCollided xs circle

getCircleOtherColliderId : List Circle -> Circle -> Int
getCircleOtherColliderId list_circle circle =
  case list_circle of 
    [] -> 0
    (x::xs) ->
      if x.id == circle.id then 
        getCircleOtherColliderId xs circle
      else
        if isTwoCirclesCollided x circle then
          x.id
        else
          getCircleOtherColliderId xs circle


isTwoCirclesCollided : Circle -> Circle -> Bool
isTwoCirclesCollided c1 c2 = 
  let
    distance = sqrt <| (+) ((c1.x - c2.x) ^ 2) ((c1.y - c2.y) ^ 2)
    radiusSum = c1.radius + c2.radius
  in
    radiusSum >= distance 