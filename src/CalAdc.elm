module CalAdc exposing (..)
--copied from Main.elm 2021.10.04
--edited: solve(line 95-)
--added: pickup,proceed(line 110 - 150)
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
                   ,rest: Int}

type Msg = InitGenerated (List Int)
    | Dropped Int Int
    | Move Time.Posix
    | Solve

init: () -> (Model, Cmd Msg)
init _ = ({dews=[]
          ,splashes=[]
          ,rest = 18
          }
         ,Random.generate InitGenerated (Random.list 25 (Random.int 0 3)))

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        InitGenerated rlist ->
            ({model|dews =
                  List.filter (\p -> p.size >0) (randomize 1 1 rlist)}
            , Cmd.none)
        Dropped x y ->
            (drop x y model |> phi |> (Debug.log ""), Cmd.none)
        Solve ->
          let
            dummy=solve 0 model
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


solve:Int-> Model -> Model
solve number model=
  let
      dummy=Debug.log "number" number
  in
  if List.isEmpty model.splashes then
    let
        target=List.head (pickup model.dews)
    in
      case target of
        Nothing->model
        Just dew->
          drop dew.x dew.y model|>(solve (number+1) )
  else
    phi model |>(solve number)

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


randomize x y rlist =
    if y > 5 then
        []
    else if x>5 then
             randomize 1 (y+1) rlist
         else
             {x=x
             , y=y
             , size = (Maybe.withDefault 0 (List.head rlist))
             }
         ::
             (randomize (x+1) y (List.drop 1 rlist))

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

borderR ry =
    rect [x "25"
         ,y (String.fromInt (50*ry-25))
         ,width "250"
         ,height "50"
         ,fill "none"
         ,stroke "black"]
    []

borderC cx =
    rect [x (String.fromInt (50*cx-25))
         ,y "25"
         ,width "50"
         ,height "250"
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
  ,(List.map borderR (List.range 1 5))
  ,(List.map borderC (List.range 1 5))
  ]
  )
  ]


subscriptions model =
    Time.every 500 Move
