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
  { x : Float
  , y : Float 
  , radius : Float
  , isSelected : Bool
  , id : Int
  , isCollided : Bool
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
  = Tick Time
  | SelectCircle Circle
  | DiselectCircles Mouse.Position
  | SetSelectedCirclesToMousePosition Mouse.Position
  | UpdateFrame String

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
    [ Circle 500 200 50 False 1 False 45 10
    , Circle 700 200 50 False 2 False 60 10 
    --, Circle 900 200 50 False 3 False 45 1
    --, Circle 1100 200 50 False 4 False 45 1
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
      [ print <| toString <| circle.movementDirection
      , boundaryToHTML model.boundary 
      , circlesToHTML model.circlesList
      ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of 
    Tick _ -> 
      (model, Cmd.none)

    SelectCircle circle ->
      ( selectCircle model circle , Cmd.none )

    DiselectCircles _ ->
      ( diselectCircles model , Cmd.none )

    SetSelectedCirclesToMousePosition mousePosition ->
      ( setSelectedCirclesToMousePosition model mousePosition , Cmd.none )

    UpdateFrame _ ->
      ( model |> markCollidedCircles |> moveCicles |> changeCirclesDirection, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Time.every Time.millisecond Tick 
    , Mouse.moves SetSelectedCirclesToMousePosition
    , Mouse.ups DiselectCircles
    , frameUpdated UpdateFrame
    ]

--////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
--////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
--////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
--////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
--////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
--////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
--////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
--////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
--////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

circlesToHTML : List Circle -> Html.Html Msg
circlesToHTML listOfCircles =
  let
    whiteOrRed : Circle -> String
    whiteOrRed circle =
      if circle.isCollided then "#FF0000" else "#737373"

    circleToHTML : Circle -> Html.Html Msg
    circleToHTML circle =
      span 
        [ onMouseDown (SelectCircle circle)
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
  div 
    [ style 
      [ ("border","10px solid red") 
      , ("position","absolute") 
      , ("height", toString (boundary.height) ++ "px") 
      , ("width", toString (boundary.wight) ++ "px")
      , ("left", toString (boundary.x) ++ "px")
      , ("top", toString (boundary.y) ++ "px")
      ]
    ] 
    []

print : String -> Html.Html Msg
print log =
  p [] [ text log ]

changeCirclesDirection : Model -> Model
changeCirclesDirection model =
  let
    func4 = func3 model.boundary
  in
    { model | circlesList = List.map func4 model.circlesList } 


func3 : Boundary -> Circle -> Circle
func3 boundary circle =
    if      isCircle_leftOf_boundary  circle boundary then { circle | movementDirection = reflectDegree circle.movementDirection True }
    else if isCircle_rightOf_boundary circle boundary then { circle | movementDirection = reflectDegree circle.movementDirection True }
    else if isCircle_above_boundary   circle boundary then { circle | movementDirection = reflectDegree circle.movementDirection False }
    else if isCircle_below_boundary   circle boundary then { circle | movementDirection = reflectDegree circle.movementDirection False }
    else    circle


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


isCircle_below_boundary : Circle -> Boundary -> Bool
isCircle_below_boundary circle boundary = 
  if circle.x >= boundary.x &&
     circle.x <= boundary.x + boundary.wight &&
     circle.y > boundary.y + boundary.height then
    True
  else
    False

isCircle_above_boundary : Circle -> Boundary -> Bool
isCircle_above_boundary circle boundary = 
  if circle.x >= boundary.x &&
     circle.x <= boundary.x + boundary.wight &&
     circle.y < boundary.y then
    True
  else
    False

isCircle_leftOf_boundary : Circle -> Boundary -> Bool
isCircle_leftOf_boundary circle boundary = 
  if circle.y >= boundary.y &&
     circle.y <= boundary.y + boundary.height &&
     circle.x < boundary.x then
    True
  else
    False

isCircle_rightOf_boundary : Circle -> Boundary -> Bool
isCircle_rightOf_boundary circle boundary = 
  if circle.y >= boundary.y &&
     circle.y <= boundary.y + boundary.height &&
     circle.x > boundary.x + boundary.wight then
    True
  else
    False

justToCircle : Maybe Circle -> Circle
justToCircle justCircle = 
  case justCircle of
    Just justCircle -> justCircle
    Nothing -> Circle 0 0 0 False 0 False 0 0 

moveCicles : Model -> Model
moveCicles model = 
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
  let
    map : Circle -> Circle
    map x =
      { x | isCollided = isCircleCollided model x } 
  in
    { model | circlesList = List.map map model.circlesList }

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

isCircleCollided : Model -> Circle -> Bool
isCircleCollided model circle = 
  let 
    isCircleCollided2 : List Circle -> Circle -> Bool
    isCircleCollided2 listOfCircles circle2 =
      case listOfCircles of
        [] -> False
        (x::xs) -> 
          if x.id == circle2.id then 
            isCircleCollided2 xs circle2
          else
            if isTwoCirclesCollided x circle2 then
              True
            else
              isCircleCollided2 xs circle2
  in
    isCircleCollided2 model.circlesList circle

isTwoCirclesCollided : Circle -> Circle -> Bool
isTwoCirclesCollided c1 c2 = 
  let
    distance = sqrt <| (+) ((c1.x - c2.x) ^ 2) ((c1.y - c2.y) ^ 2)
    radiusSum = c1.radius + c2.radius
  in
    radiusSum >= distance 