import Html exposing (Html, div, button)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second, millisecond)
import Array exposing (Array, get, toList)
import Mouse exposing (Position)



type alias Point3D = (Float, Float, Float)
type alias Point2D = (Float, Float)
type alias Edge = (Int, Int)
type alias Vertices = List Point3D

type alias AngularVelocity = {xRate: Float, yRate: Float, zRate: Float}

type alias Model = {v: Vertices, a: AngularVelocity}

type Msg = Tick Time
         | IncXAng
         | IncYAng
         | IncZAng


------------------ INIT ----------------------------------
initVertices : Vertices
initVertices = [( 100, 100, 100)
               ,(-100, 100, 100)
               ,( 100,-100, 100)
               ,(-100,-100, 100)
               ,( 100, 100,-100)
               ,(-100, 100,-100)
               ,( 100,-100,-100)
               ,(-100,-100,-100)]

frameRate = 200
oneDeginRad = 0.01745329255
tenDeginRad = oneDeginRad * 10

drpf = 0.0174532925*10/frameRate -- 10 deg per second -- angle dampen rate per frame


initAnglarVel : AngularVelocity
initAnglarVel = {xRate=tenDeginRad, yRate=tenDeginRad, zRate=tenDeginRad}

edges : List Edge
edges = [ (0,1), (0,2), (0,4),(1,5), (1,3), (2,3)
        , (2,6), (4,5), (4,6),(3,7), (6,7), (5,7)
        ]

--edges = [ (0,7), (3,4), (0,4),(1,5), (1,3), (2,3)
--        , (2,6), (4,5), (4,6),(3,7), (6,7), (5,7)]



------------------- UPDATE / REDUCERS ---------------------------

rotateInPlane axis1 axis2 ang =
  ( axis1 * cos(ang) - axis2 * sin(ang),
    axis2 * cos(ang) + axis1 * sin(ang) )

rotateX ang (x,y,z) = let (ny,nz) = rotateInPlane y z ang in ( x, ny, nz)
rotateY ang (x,y,z) = let (nx,nz) = rotateInPlane x z ang in (nx,  y, nz)
rotateZ ang (x,y,z) = let (nx,ny) = rotateInPlane x y ang in (nx, ny,  z)
rotate xang yang zang p = p |> rotateX xang |> rotateY yang |> rotateZ zang

dampenPercent = (1-(0.9 / frameRate)) -- 10% per second
dampen : Float -> Float
dampen ang = ang * dampenPercent -- Basics.max 0 (ang-drpf)g

accelerateBy :  Float
accelerateBy = oneDeginRad * 50


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick _ ->  ( {v= (List.map (rotate (model.a.xRate/frameRate)
                                           (model.a.yRate/frameRate)
                                           (model.a.zRate/frameRate) ) model.v),
                      a= {xRate= dampen model.a.xRate, yRate= dampen model.a.yRate, zRate= dampen model.a.zRate}
                 }
                    , Cmd.none)
    IncXAng -> let a = model.a
                   newa = {a | xRate = a.xRate + accelerateBy}
               in ({model | a = newa} , Cmd.none)
    IncYAng -> let a = model.a
                   newa = {a | yRate = a.yRate + accelerateBy}
               in ({model | a = newa} , Cmd.none)
    IncZAng -> let a = model.a
                   newa = {a | zRate = a.zRate + accelerateBy}
               in ({model | a = newa} , Cmd.none)


-------------------- VIEW --------------------------------

viewCenterX = 300
viewCenterY = 300

-- parallel projection
project : List Point3D -> Array Point2D
project = Array.fromList << List.map (\(x,y,z) -> (x + viewCenterX, y + viewCenterY ))

str = toString

drawEdges : Array Point2D -> List Edge -> List (Svg Msg)
drawEdges vert2Ds edges =
  let
    edgeLine (p1x,p1y) (p2x,p2y) =
                    line [ x1 (str p1x), y1 (str p1y)
                         , x2 (str p2x), y2 (str p2y), stroke "#023963"  ] []

    drawEdge (v1,v2) = Maybe.map2 edgeLine (get v1 vert2Ds) (get v2 vert2Ds)
                          |> Maybe.withDefault (text "empty edge")

  in
   edges |> List.map drawEdge


drawVertices : Array Point2D -> List (Svg Msg)
drawVertices vert2Ds =
  let
    drawVertex idx (px, py) =
      [ text_ [ x (str (px + 5)), y (str (py - 5)) , fill "blue"]
              [ text (str idx) ]
      , circle [cx (str px), cy (str py), r "4", stroke "blue"] [] ]
   in
     vert2Ds |> Array.indexedMap drawVertex |> toList |> List.concat

drawCube : Array Point2D -> List (Svg Msg)
drawCube vert2Ds = drawEdges vert2Ds edges ++ drawVertices vert2Ds

divStyle : Attribute msg
divStyle =
  Html.Attributes.style
    [ ("backgroundColor", "red")
    , ("float", "left")
    ]

buttonStyle : Attribute msg
buttonStyle =
  Html.Attributes.style
    [ ("backgroundColor", "blue")
    , ("float", "left")
    ]

view : Model -> Html Msg
view model =
  let
     vert2Ds = project model.v
  in
    div [divStyle] [

           svg [ viewBox "0 0 600 600", width "350px" ] (drawCube vert2Ds)

           , button [ onClick IncXAng, buttonStyle ] [ text "x-ang ++" ]
           , button [ onClick IncYAng, buttonStyle ] [ text "y-ang ++" ]
           , button [ onClick IncZAng, buttonStyle ] [ text "z-ang ++" ]
           ]


----------------- SETUP ------------------

subscriptions : Model -> Sub Msg
subscriptions model = Time.every ((1000/frameRate) * millisecond) Tick

main =
  Html.program
    { init = ({v= initVertices, a= initAnglarVel}, Cmd.none)
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
