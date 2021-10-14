module CutOffTest exposing (..)

import Browser
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Random
import Time
import Html
import Html.Attributes
import Html.Events


main = Browser.element {init=init, update=update, view=view, subscriptions=subscriptions}

type alias Dew = {x:Int, y:Int, size:Int}
type alias Splash = {x:Int, y:Int, vx:Int, vy:Int}

type alias Model = {dews: List Dew
                   ,splashes: List Splash
                   ,rest: Int
                 ,mapSize: Int}

type Msg = InitGenerated (List Int)
    | Dropped Int Int
    | Move Time.Posix
    | Solve

init: () -> (Model, Cmd Msg)
init _ = ({dews=[]
          ,splashes=[]
          ,rest = 18
          ,mapSize=3
          }
         ,Random.generate InitGenerated (Random.list 25 (Random.int 0 3)))

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        InitGenerated rlist ->
            ({model|dews =
                  List.filter (\p -> p.size >0) (randomize 1 1 rlist model.mapSize)}
            , Cmd.none)
        Dropped x y ->
            (drop x y model |> phi |> (Debug.log ""), Cmd.none)
        Solve ->
          let
            dummy=List.map (\d-> solve 1 d model (List.singleton (model.mapSize*100))) model.dews
            --dummy = Debug.log "test" (generateB "")
          in
            (model,Cmd.none)

        Move _ ->
            (phi model, Cmd.none)

phi: Model -> Model
phi model =
    let
        support dlist =
            List.map (\d -> (d.x,d.y)) dlist
        numsplash d = List.length (List.filter (\s -> s.x==d.x && s.y==d.y) model.splashes)
        newDews = List.map (\d -> {d | size = (numsplash d)+d.size}) model.dews |>
                  List.filter (\d -> d.size < 4)
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
        , splashes=List.filter (\s -> s.x > 0 && s.x <= 5 && s.y > 0 && s.y <= 5) <|
            List.concat [burstSplashes, remainingSplashes]}


drop: Int -> Int -> Model -> Model
drop x y model =
  let
      dummy=Debug.log "drop" (x,y)
  in
    {model|splashes = List.concat [[{x=x,y=y,vx=0,vy=0}],model.splashes]}

mPhi: Model->Model
mPhi model =
  if List.isEmpty model.splashes then
    model
  else
    phi model

solve:Int->Dew-> Model -> List Int-> List Int
solve number position model answers=
  let
    afterDrop=drop position.x position.y model
                  |>mPhi
    dummy=Debug.log "number" number
  in
  case (List.minimum answers) of
    Just limit->
      if number<limit then

        if List.isEmpty afterDrop.dews then
          number::answers|>(\a-> Debug.log "answers" a)
        else
          List.concat <|List.map (\d -> solve (number+1) d afterDrop answers) afterDrop.dews
          --cannot refer to the minimum in another branch
      else
        (model.mapSize*100)::answers
    Nothing ->
      (model.mapSize*100)::answers

generateB: String->List String
generateB str=
  if (String.length str)==10 then
    [str]
  else
    List.concat [generateB (str++"0"),generateB (str++"1")]

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
    circle [cx (String.fromInt (50*dew.x))
           ,cy (String.fromInt (50*dew.y))
           ,r (String.fromInt (8*dew.size))
           ,fill "#aaf"
           ,onClick (Dropped dew.x dew.y)][]

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
                ,fill "#33f"
                ][]
        ,circle [cx (String.fromInt (-1*splash.vx))
                ,cy (String.fromInt (-1*splash.vy))
                ,r (String.fromInt (3))
                ,fill "#33f"
                ][]
        ,circle [cx (String.fromInt (-8*splash.vx))
                ,cy (String.fromInt (-8*splash.vy))
                ,r (String.fromInt (2))
                ,fill "#33f"
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
  Html.div[][
    Html.button[Html.Events.onClick Solve][Html.text "solve"]
    ,Html.br[][]
    ,svg [width "500"
      ,height "500"]
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


subscriptions model =
    Time.every 500 Move
