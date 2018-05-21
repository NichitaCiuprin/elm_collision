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
  , borderThickness : Float
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
  , boundary = Boundary 700 100 700 1000 5
  }

view : Model -> Html.Html Msg 
view model =
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
    corectedPosition_left  = (boundary.x - boundary.borderThickness)
    corectedPosition_right = (boundary.y - boundary.borderThickness)
  in
  div 
    [ style 
      [ ("border", toString boundary.borderThickness ++ "px solid red") 
      , ("position","absolute") 
      , ("height", toString boundary.height        ++ "px") 
      , ("width",  toString boundary.wight         ++ "px")
      , ("left",   toString corectedPosition_left  ++ "px")
      , ("top",    toString corectedPosition_right ++ "px")
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

    reflectDegree degree direction = 
      let
        normalise    degree = Basics.toFloat <| (Basics.round degree) % 360
        skip360      degree = if degree == 360 then 0 else degree
        normaliseAll degree = degree |> normalise |> skip360
      in
        case direction of "backwards"   -> degree |> (+) 180           |> normaliseAll
                          "horizontali" -> degree |> negate            |> normaliseAll
                          "verticali"   -> degree |> negate |> (+) 180 |> normaliseAll
                          _             -> degree                      |> normaliseAll


    reflectCircle direction = { circle | movementDirection = reflectDegree circle.movementDirection direction }
    
  in
    if      isCircleInTheCorner                                   then reflectCircle "backwards"
    else if isCircle_leftOf_boundary || isCircle_rightOf_boundary then reflectCircle "horizontali"
    else if isCircle_above_boundary  || isCircle_below_boundary   then reflectCircle "verticali"
    else    circle

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