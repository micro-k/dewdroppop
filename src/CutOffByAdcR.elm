module CutOffByAdcR exposing (..)
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
          ,mapSize=2
          }
         ,Random.generate InitGenerated (Random.list (5*5) (Random.int 0 3)))

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
            limit=certain 0 model
            paths=List.map (\d->
               solveMinimum [(0,0)] d model limit) model.dews
               |>List.filter (\s ->not (
                  case List.head s of
                    Just a->
                      (Tuple.first a)==100+limit
                    _->
                      True))

            m=case List.minimum (List.map List.length paths) of
                  Just number->number
                  _->0|>Debug.log "error: no answers"
            ans=List.filter (\p->
              (List.length p)==m) paths
                |>List.map (\p->
                  List.tail (List.reverse p))
                |>Debug.log "answers"
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
        , splashes=List.filter (\s -> List.member s.x (List.range 1 model.mapSize)&&List.member s.y (List.range 1 model.mapSize)) <|
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
    mPhi (phi model)

solveMinimum:List (Int,Int)->Dew-> Model -> Int->List (Int,Int)
solveMinimum moves position model limit=
  let
    afterDrop=mPhi (drop position.x position.y model)
  in
    if List.isEmpty afterDrop.dews then
      (position.x,position.y)::moves|>Debug.log "one of answers"
    else if (List.length moves)>limit then
      (100+limit,100+limit)::moves|>Debug.log "cutoff"
    else
      let
        ret=List.map (\d -> solveMinimum ((position.x,position.y)::moves) d afterDrop limit) afterDrop.dews
        m=case List.minimum (List.map (\r-> List.length r) ret)of
          Just number->number
          _->0|>Debug.log "error"
      in
        List.filter (\r->(List.length r)==m) ret|>List.concat

certain:Int-> Model -> Int
certain number model=
  if List.isEmpty model.splashes then
    let
        target=List.head (pickup model.dews)
    in
      case target of
        Nothing->number|>Debug.log "cutoff by"
        Just dew->
          drop dew.x dew.y model|>(certain (number+1) )
  else
    phi model |>(certain number)

pickup dews =
  let
    vec=[{x=1,y=0}, {x=-1,y=0}, {x=0,y=1}, {x=0,y=-1}]
    adjacent position =List.sum(List.map (\v-> proceed position v dews) vec)
    --the function "adjacent" returns sum(number) of adjacent dews' size in a certain 'position'
    onlySizeMax cs=List.filter (\s->(Just s.size)== (List.maximum (List.map (\t->t.size) cs))) cs

    calSheet = List.map (\d->
       {x=d.x,y=d.y,size=d.size,
        score=d.size+(adjacent (Tuple.pair d.x d.y))}
        ) dews
    mm=List.maximum (List.map (\c->c.score) calSheet)
  in
    onlySizeMax (List.filter (\c ->(Just c.score)==mm) calSheet)
    --pickup dews which have the biggest size in
    --  dews which have the biggest score in dews(List)

    --dews --(dummy)

proceed position v dews=
  let
    existDewList= List.map (\d-> Tuple.pair d.x d.y) dews
    x=(Tuple.first position)+v.x
    y=(Tuple.second position)+v.y
    pp=(Tuple.pair x y)
  in
    if (List.member x (List.range 1 5)) &&(List.member y (List.range 1 5)) then
    --if (x,y) is on the board
      if (List.member pp existDewList) then
      --if a dew exist at (x,y)
        case (List.head (List.filter (\d -> d.x==x&&d.y==y) dews)) of
          Just dew->
            dew.size
          Nothing->
            proceed pp v dews
      else
        proceed pp v dews
        --if there is no dew at (x,y) then proceed more
    else
      0
      --if (x,y) is over the board then returns 0


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
