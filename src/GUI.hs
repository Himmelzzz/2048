module GUI where

import Funktionen
 (Game(..), Direction(..), Grid, printCell, initGame, insertRandomGrid, blockCheck, leftGrid, fullCheck, scoreGrid, countNothing, createRandom, createRandomCell)
import Data.Maybe
import Data.List
import Prelude

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor, attrName
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Util as U
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Linear.V2 (V2(..))

data Tick = Tick

type Name = ()

gameOverAttr, blueBg, brblBg, cyanBg, bcyanBg, yellowBg, byellowBg, greenBg, bgreenBg,  whiteBg  :: AttrName
gameOverAttr =attrName "gameOver"
blueBg = attrName "blueBg"
brblBg = attrName "brblBg"
cyanBg = attrName "cyanBg"
bcyanBg = attrName "bcyanBg"
magBg = attrName "magBg"
bmagBg = attrName "bmagBg"
yellowBg = attrName "yellowBg"
byellowBg = attrName "byellowBg"
greenBg = attrName "greenBg"
bgreenBg = attrName "bgreenBg"
whiteBg = attrName "whiteBg"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [
  (gameOverAttr, fg V.red `V.withStyle` V.bold),
  (blueBg, U.fg V.blue),
  (brblBg, U.fg V.brightBlue),
  (cyanBg, U.fg V.cyan),
  (bcyanBg, U.fg V.brightCyan),
  (yellowBg, U.fg V.yellow),
  (byellowBg, U.fg V.brightYellow),
  (magBg, U.fg V.magenta),
  (bmagBg, U.fg V.brightMagenta),
  (greenBg, U.fg V.green),
  (bgreenBg, U.fg V.brightGreen),
  (whiteBg, U.bg V.white)
  ]

app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

humanPlayer :: IO ()
humanPlayer = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100000 -- decides how fast your game moves
  g <- initGame
  void $ customMain (V.mkVty V.defaultConfig) (Just chan) app g

isGameOver :: Game -> Bool
isGameOver g = (fullCheck (_grid g)) && (blockCheck (_grid g))

step :: Game -> Game
step g =
  if isGameOver g then Game {_grid = _grid g, _score = _score g, _done = True}
    else g

handle :: Direction -> Grid -> Grid
handle d grid = case d of
  Funktionen.Up     -> transpose (leftGrid (transpose grid))
  Funktionen.Down   -> transpose (map reverse (leftGrid (map reverse (transpose grid))))
  Funktionen.Left   -> leftGrid grid
  Funktionen.Right  -> map reverse (leftGrid (map reverse grid))

move :: Direction -> Game -> Game
move dir g =
  if (_grid g == handle dir (_grid g))
  then
  Game {  _grid = _grid g
        , _score = _score g
        , _done = _done g
        }
  else
  Game {  _grid = newGrid
        , _score = (scoreGrid newGrid)
        , _done = (fullCheck newGrid && blockCheck newGrid)
        }
  where newGrid =  insertRandomGrid (handle dir (_grid g)) (createRandom (countNothing (handle dir (_grid g)))) (createRandomCell (countNothing (handle dir (_grid g))))

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                       = continue $ step g
handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ move Funktionen.Up g
handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue $ move Funktionen.Down g
handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue $ move Funktionen.Right g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue $ move Funktionen.Left g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO (initGame) >>= continue
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g _                                     = continue g

drawUI :: Game -> [Widget Name]
drawUI g =
  [ C.center $ padRight (Pad 4) (drawStats g) <+> drawGrid g <+> padLeft (Pad 4) drawInfo]

drawInfo :: Widget Name
drawInfo = withBorderStyle BS.unicodeBold
  $ hLimit 20
  $ B.borderWithLabel (str "Commands")
  $ vBox $ map (uncurry drawKey)
  $ [ ("Left", "←")
    , ("Right", "→")
    , ("Down", "↓")
    , ("Restart", "r")
    , ("Quit", "q or esc")
    ]
  where
    drawKey act key = (padRight Max $ padLeft (Pad 1) $ str act)
                      <+> (padLeft Max $ padRight (Pad 1) $ str key)

drawStats :: Game -> Widget Name
drawStats g = hLimit 11
  $ vBox [ drawScore (_score g) , padTop (Pad 2) $ drawGameOver (_done g)]

drawScore :: Int -> Widget Name
drawScore n = withBorderStyle BS.unicodeBold
  $ withAttr gameOverAttr
  $ B.borderWithLabel (str "Score")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

drawGameOver :: Bool -> Widget Name
drawGameOver done =
  if done
    then withAttr gameOverAttr $ C.hCenter $ str "GAME OVER"
    else emptyWidget

colorCell val = case val of
  "2" -> withAttr blueBg $ str val
  "4" -> withAttr brblBg $ str val
  "8" -> withAttr cyanBg $ str val
  "16" -> withAttr bcyanBg $ str val
  "32" -> withAttr magBg $ str val
  "64" -> withAttr bmagBg $ str val
  "128" -> withAttr yellowBg $ str val
  "256" -> withAttr byellowBg $ str val
  "512" -> withAttr greenBg $ str val
  "1024" -> withAttr bgreenBg $ str val
  "2048" -> withAttr whiteBg $ str val
  _ -> str val

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (withAttr magBg $ str "2048")
  $ vBox rows
  where
    rows = [hBox $ cellsInRow r | r <- (_grid g)]
    cellsInRow row = [hLimit 9 $ withBorderStyle BS.unicodeBold $ B.border $ C.hCenter $ padAll 1 $ colorCell $ printCell cell | cell <- row]
