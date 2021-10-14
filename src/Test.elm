module Test exposing (..)

import Browser
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Html
import Html.Attributes
import Html.Events
import Random
import Time

main = Browser.element {init=init, update=update, view=view, subscriptions=subscriptions}

type alias Dew = {x:Int, y:Int, size:Int}
type alias Splash = {x:Int, y:Int, vx:Int, vy:Int}
type Stage = Setup | Playing
type alias Model = {dews: List Dew
                   ,splashes: List Splash
                   ,rest: Int
                   ,mapSize: Int
                 ,stage: Stage}

type Msg = InitGenerated (List Int)
    | Dropped Int Int
    | Move Time.Posix
    | Quit
    | Start
    |MapSizeIncrease
    |MapSizeDecrease
    |Solve

breakSize=5
dewSizeUnit=floor(20/(breakSize-1))

init: () -> (Model, Cmd Msg)
init _ = ({dews=[]
          ,splashes=[]
          ,rest = 18
          ,stage = Setup
          ,mapSize = 5
          }
         ,Random.generate InitGenerated (Random.list (5*5) (Random.int 0 (breakSize-1))))

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  if model.stage == Playing then
      case msg of
          InitGenerated rlist ->
              ({model|dews =
                    List.filter (\p -> p.size >0) (randomize 1 1 rlist model.mapSize)}
              , Cmd.none)
          Dropped x y ->
              (drop x y model |> phi |> (Debug.log ""), Cmd.none)
          Move _ ->
              (phi model, Cmd.none)
          Quit ->
              ({model | stage = Setup},Cmd.none)
          _ ->
            (model,Cmd.none)
    else
      case msg of
        Start ->
          ({model|stage=Playing},Random.generate InitGenerated (Random.list (model.mapSize*model.mapSize) (Random.int 0 (breakSize-1))))
        MapSizeIncrease ->
          ({model|mapSize=(model.mapSize+1)},Cmd.none)
        MapSizeDecrease ->
          ({model|mapSize=(model.mapSize-1)},Cmd.none)

        _->
          (model,Cmd.none)

phi: Model -> Model
phi model =
    let
        support dlist =
            List.map (\d -> (d.x,d.y)) dlist
        numsplash d = List.length (List.filter (\s -> s.x==d.x && s.y==d.y) model.splashes)
        newDews = List.map (\d -> {d | size = (numsplash d)+d.size}) model.dews |>
                  List.filter (\d -> d.size < breakSize)
        diff = List.filter (\p -> not(List.member p (support newDews)))
               (support model.dews)
        burstAt p =
            let
                x=Tuple.first p
                y=Tuple.second p
            in
            [{x=x+1,  y=y, vx=1, vy=0}
            ,{x=x-1,  y=y, vx=-1, vy=0}
            ,{x=x,  y=y+1, vx=0, vy=1}
            ,{x=x,  y=y-1, vx=0, vy=-1}]
        burstSplashes =
            List.foldr  (\p list -> List.concat [burstAt p, list]) [] diff
        remainingSplashes =
            List.map (\s -> {s|x=(s.x+s.vx), y=(s.y+s.vy)}) <|
            List.filter (\s -> not <| List.member (s.x,s.y) (support model.dews))
                model.splashes
    in
        {model | dews=newDews
        , splashes=List.filter (\s -> s.x > 0 && s.x <= model.mapSize && s.y > 0 && s.y <= model.mapSize) <|
            List.concat [burstSplashes, remainingSplashes]}


drop: Int -> Int -> Model -> Model
drop x y model =
    {model|splashes = List.concat [[{x=x,y=y,vx=0,vy=0}],model.splashes]}

randomize x y rlist mapSize=
    if y > mapSize then
        []
    else if x>mapSize then
             randomize 1 (y+1) rlist mapSize
         else
             {x=x
             , y=y
             , size = (Maybe.withDefault 0 (List.head rlist))
             }
         ::
             (randomize (x+1) y (List.drop 1 rlist) mapSize)

suiteki dew =
  g[][
    circle [cx (String.fromInt (50*dew.x))
      ,cy (String.fromInt (50*dew.y))
      ,r (String.fromInt (dewSizeUnit*dew.size))
      ,fill "#adf"
      ,stroke "#4ad"
      ,strokeWidth (String.fromInt (dew.size))
      ,onClick (Dropped dew.x dew.y)][]
    ,circle[
      cx (String.fromInt (50*dew.x+2*dew.size))
      ,cy (String.fromInt (50*dew.y-2*dew.size))
      ,r (String.fromInt (dewSizeUnit*dew.size//4))
      ,fill "#fff"
      ,onClick (Dropped dew.x dew.y)
    ][]

  ]

himatsu splash =
    g [transform (String.concat ["translate("
                                ,(String.fromInt (50*splash.x))
                                ,","
                                ,(String.fromInt (50*splash.y))
                                ,")"])]
        [
         circle [cx (String.fromInt (5*splash.vx))
                ,cy (String.fromInt (5*splash.vy))
                ,r (String.fromInt (6))
                ,fill "#4ad"
                ][]
        ,circle [cx (String.fromInt (-1*splash.vx))
                ,cy (String.fromInt (-1*splash.vy))
                ,r (String.fromInt (3))
                ,fill "#4ad"
                ][]
        ,circle [cx (String.fromInt (-8*splash.vx))
                ,cy (String.fromInt (-8*splash.vy))
                ,r (String.fromInt (2))
                ,fill "#4ad"
                ][]
        ]

borderR ry mapSize=
    rect [x "25"
         ,y (String.fromInt (50*ry-25))
         ,width (String.fromInt(50*mapSize))
         ,height "50"
         ,fill "none"
         ,stroke "black"]
    []

borderC cx mapSize=
    rect [x (String.fromInt (50*cx-25))
         ,y "25"
         ,width "50"
         ,height (String.fromInt(50*mapSize))
         ,fill "none"
         ,stroke "black"]
    []

view model =
  if model.stage ==Playing then
    Html.div[][
      Html.button[Html.Events.onClick Solve][Html.text "solve-dummy"]
    ,Html.br[][]
    ,svg [width "500",height "500"]
    (List.concat [
    (List.map suiteki model.dews)
    ,(List.map himatsu model.splashes)
    ,(List.map
    (\i-> borderR i model.mapSize)(List.range 1 model.mapSize) )
    ,(List.map
    (\i-> borderC i model.mapSize) (List.range 1 model.mapSize) )
    ]
    )
    ]
    else
      Html.div[][
        Html.h1 [][text "DewDropPop Puzzle"
        ,Html.div[][
          Html.text ("size of map: "++(String.fromInt model.mapSize))
          ,Html.button[onClick MapSizeIncrease][text "+"]
          ,Html.button[onClick MapSizeDecrease][text "-"]
          ]

        ,Html.div[][
          Html.button[onClick Start][text "Start"]]
        ]
      ]

subscriptions model =
    Time.every 100 Move
