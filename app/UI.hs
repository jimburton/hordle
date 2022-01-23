module UI where

import Brick.Main
import Brick.AttrMap (attrMap, AttrMap)
import Brick.Types ( Widget, BrickEvent(..), Next, EventM)
import Hordle (Game(..))

-- Types

-- | Named resources
--
data Name = Cell Int Int

-- App definition

app :: App Game e Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

main :: IO ()
main = undefined

-- Handling events

handleEvent :: Game -> BrickEvent Name e -> EventM Name (Next Game)
handleEvent = undefined

-- Drawing

drawUI :: Game -> [Widget Name]
drawUI = undefined

theMap :: AttrMap
theMap = undefined
