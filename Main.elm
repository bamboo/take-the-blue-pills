import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import List (map, filter, partition, any, length, (::), isEmpty)
import Mouse
import Random
import Signal (Signal, (<~), (~), foldp, sampleOn, merge)
import Text
import Time (Time, fps, inSeconds, every, second)
import Window

main : Signal Element
main =
  let game = foldp stepGame defaultGame events
  in render <~ Window.dimensions ~ game

stepGame : Event -> Game -> Game
stepGame e g =
  case g.state of
    Play -> stepGamePlay e g
    Over -> g

defaultGame : Game
defaultGame = {player = defaultPlayer
              ,pills = []
              ,score = 0
              ,state = Play}

events : Signal Event
events = merge
  (Input <~ input)
  (Spawn << randomPill <~ every (0.25 * second))

input : Signal GameInput
input =
  let time = inSeconds <~ fps 30
      windowCenter = center <~ Window.dimensions
      mouse = relativeMouse <~ windowCenter ~ Mouse.position
  in (,) <~ time ~ sampleOn time mouse

render : (Int, Int) -> Game -> Element
render (sw, sh) g =
  let forms = renderScore g.score :: map renderPill (g.player :: g.pills)
  in collage width height forms
    |> color white
    |> container sw sh middle
    |> color lightGray

type Event = Input GameInput | Spawn Pill

type alias GameInput = (Time, (Int, Int))

type alias Game = {player: Pill, pills: List Pill, score: Int, state: State}

type alias Pill = {pos: Vec, vel: Vec, rad: Float, col: Color}

type State = Play | Over

type alias Vec = (Float, Float)

stepGamePlay : Event -> Game -> Game
stepGamePlay e ({player, pills} as g) =
  case e of
    Input (t, mp) ->
      let unculled = filter isVisible pills
          (touched, untouched) = partition (overlaps player) unculled
          (reds, blues) = partition (\{col} -> col == defaultPill.col) touched
          g' = {g | player <- stepPlayer mp player
                  , pills  <- map (stepPill t) untouched
                  , score <- g.score + length blues}
      in if isEmpty reds then g' else {g' | state <- Over}
    Spawn p ->
      {g | pills <- p :: pills}

defaultPlayer : Pill
defaultPlayer = {defaultPill | col <- black
                             , pos <- (0, -hHeight)}

defaultPill : Pill
defaultPill = {pos = (0, hHeight)
              ,vel = (0, -300)  -- pixels per second
              ,rad = 15
              ,col = lightRed}

randomPill : Time -> Pill
randomPill t =
  let seed = Random.initialSeed (round t)
      xRange = (Random.float -hWidth hWidth)
      probability = (Random.float 0 1)
      ((x, p), seed') = Random.generate (Random.pair xRange probability) seed
  in newPill x (if p < 0.1 then lightBlue else defaultPill.col)

newPill : Float -> Color -> Pill
newPill x col =
  {defaultPill | pos <- (x, hHeight)
               , col <- col}

width = 400
height = 400
hWidth = width / 2
hHeight = height / 2

stepPill : Time -> Pill -> Pill
stepPill t p =
  {p | pos <- vecAdd p.pos <| vecMultS p.vel t}

stepPlayer : (Int, Int) -> Pill -> Pill
stepPlayer (x, y) p =  {p | pos <- (toFloat x, toFloat y)}

overlaps : Pill -> Pill -> Bool
overlaps player pill =
  let distance = vecLen <| vecSub player.pos pill.pos
  in distance < player.rad + pill.rad

isVisible : Pill -> Bool
isVisible {pos, rad} = snd pos + rad > -hHeight

renderPill {pos,rad,col} =
  circle rad |> filled col
             |> move pos

renderScore : Int -> Form
renderScore s =
  renderText 0 4 (toString s)

renderText : Float -> Float -> String -> Form
renderText y scl str =
  Text.fromString str |> Text.color gray
                      |> Text.centered
                      |> toForm
                      |> scale scl
                      |> move (0, y)

relativeMouse : (Int, Int) -> (Int, Int) -> (Int, Int)
relativeMouse (ox, oy) (x, y) = (x - ox, -(y - oy))

center : (Int, Int) -> (Int, Int)
center (w, h) = (w // 2, h // 2)

vecAdd : Vec -> Vec -> Vec
vecAdd (x, y) (sx, sy) = (x + sx, y + sy)

vecSub : Vec -> Vec -> Vec
vecSub (x, y) (sx, sy) = (x - sx, y - sy)

vecLen : Vec -> Float
vecLen (x, y) = sqrt (x * x + y * y)

vecMultS : Vec -> Time -> Vec
vecMultS (x, y) t = (x * t, y * t)
