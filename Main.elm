port module Main exposing (..) 

import Html exposing (text, div, span, p)
import Html.Attributes exposing (..)
import Html.Events exposing (onMouseDown)
import Time exposing (Time, second)
import Mouse exposing (moves)

port frameUpdated : (String -> msg) -> Sub msg 

type alias Model =
  List Circle

type alias Circle =
  { x : Float
  , y : Float 
  , radius : Float
  , isSelected : Bool
  , id : Int
  , isCollided : Bool
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
  [ Circle 200 200 50 False 1 False
  , Circle 400 200 50 False 2 False
  , Circle 600 200 50 False 3 False
  , Circle 800 200 50 False 4 False
  ]

view : Model -> Html.Html Msg 
view model =
  div 
   [] 
   [ div [] (List.map circleToText model)  
   , div [] (List.map circleToSpan model)  
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
      ( model |> markCollidedCircles |> mooveCicles , Cmd.none )

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

mooveCicles : Model -> Model
mooveCicles model = 
  let
    map : Circle -> Circle
    map item =
        {item | x = item.x + 1} 
  in
    List.map map model

markCollidedCircles : Model -> Model
markCollidedCircles model = 
  let
      map : Circle -> Circle
      map x =
          {x | isCollided = isCircleCollided x model} 
  in
      List.map map model

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
      List.map map model

diselectCircles : Model -> Model
diselectCircles model =
  let
      map : Circle -> Circle
      map x =
          { x | isSelected = False }
  in
      List.map map model 

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
      List.map map model

circleToSpan : Circle -> Html.Html Msg
circleToSpan circle =
  span 
    [ onMouseDown (SelectCircle circle)
    , style 
        [ ("left",   toString (circle.x - circle.radius) ++ "px")
        , ("top",    toString (circle.y - circle.radius) ++ "px")
        , ("height", toString (circle.radius * 2)        ++ "px")
        , ("width",  toString (circle.radius * 2)        ++ "px")
        , ("background-color" , whiteOrRed circle)
        ]
    ] 
    [] 

whiteOrRed : Circle -> String
whiteOrRed circle =
  if circle.isCollided then "#FF0000" else "#737373"

isCircleCollided : Circle -> Model -> Bool
isCircleCollided circle model = 
  case model of
    [] -> False
    (x::xs) -> 
      if x.id == circle.id then 
        isCircleCollided circle xs
      else
        if isTwoCirclesCollided x circle then
          True
        else
          isCircleCollided circle xs

isTwoCirclesCollided : Circle -> Circle -> Bool
isTwoCirclesCollided c1 c2 = 
  let
    distance = sqrt <| (+) ((c1.x - c2.x) ^ 2) ((c1.y - c2.y) ^ 2)
    radiusSum = c1.radius + c2.radius
  in
    radiusSum >= distance 

circleToText : Circle -> Html.Html Msg
circleToText circle = 
    let
        s = style [ ("line-height","0.5") ]
    in
        div 
            []
            [ p [s] [text <| toString <| circle.id ]
            , p [s] [text <| toString <| circle.x ]
            , p [s] [text <| toString <| circle.y ]
            , p [s] [text <| toString <| circle.isCollided ]
            , p [s] [text <| toString <| circle.isSelected ]
            , p [s] [text <| toString <| circle.radius ]
            , p [s] [text <| "-------" ]
            ]


