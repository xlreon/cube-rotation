module Main where

import FFI.Util
import Graphics.Canvas
import Math
import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Either.Nested (in1)
import Data.List (difference)
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafePartial)

-- Imports for the JAVASCRIPT funcitons
foreign import getFaceVert :: Array Vect3 -> Array Int -> Int -> Vect3
foreign import getFace :: Array (Array Int) -> Int -> Array Int

foreign import animate :: forall e. Context2D
                      -> (Context2D -> Eff   (canvas :: CANVAS | e) Unit)
                      -> Eff (canvas :: CANVAS | e) Unit

foreign import data Event :: Type

foreign import addEventListener :: forall e. CanvasElement -> String 
                                  -> (Event -> Eff (canvas :: CANVAS | e) Unit)
                                  -> Eff (canvas :: CANVAS | e) Unit                      


foreign import addBtnListener :: String -> String -> (String -> Unit) -> Unit
foreign import resetDefault :: String -> (String -> Unit) -> (String -> Unit) -> (Unit) -> (Unit) -> (Unit) -> Unit

newtype Vect2 = Vect2 { x :: Number, y :: Number}

newtype Vect3 = Vect3 { x :: Number, y :: Number, z :: Number }

newtype Angle = Angle { qx :: Number, qy :: Number, qz :: Number }


-- Cube vertices
vertices :: Array Vect3
vertices = [
    Vect3 { x: -(1.0), y: 1.0, z: 1.0 },
    Vect3 { x: 1.0, y: 1.0, z: 1.0 },
    Vect3 { x: 1.0, y: -(1.0), z: 1.0 },
    Vect3 { x: -(1.0), y: -(1.0), z: 1.0 },
    Vect3 { x: -(1.0), y: 1.0, z: -(1.0) },
    Vect3 { x: 1.0, y: 1.0, z: -(1.0) },
    Vect3 { x: 1.0, y: -(1.0), z: -(1.0) },
    Vect3 { x: -(1.0), y: -(1.0), z: -(1.0) }
  ]


-- Cube faces
faces :: Array (Array Int)
faces = [
    [4, 5, 6, 7],
    [0, 1, 2, 3],
    [0, 4, 7, 3],
    [5, 1, 2, 6],
    [0, 1, 5, 4],
    [3, 2, 6, 7]
  ]

-- Transforming 3D points to 2D points
perspectiveDivide :: Vect3 -> Vect2
perspectiveDivide (Vect3 v) = Vect2 {
  x : v.x/v.z,
  y : -(v.y)/v.z
}

project :: Vect3 -> Vect2
project (Vect3 v) = perspectiveDivide (Vect3 {
  x: v.x,
  y: v.y,
  z: v.z + 3.0
})


-- Stretching the unit cube
translate :: Vect2 -> Vect2
translate (Vect2 v) = Vect2 {
  x: v.x + (1000.0/2.0),
  y: v.y + (1000.0/2.0)
}

stretch :: Vect2 -> Vect2
stretch (Vect2 v) = translate (Vect2 {
  x: v.x * (1000.0/2.0),
  y: v.y * (1000.0/2.0)
})

-- Showing on the screen
toScreen :: Vect3 -> Vect2
toScreen = stretch <<< project

-- Color set of the cube
color :: Array String
color = ["#000000","#FFFFFF"] 

setColor :: String -> Unit
setColor s = setProperty  color "0" s 

setBackColor :: String -> Unit
setBackColor s = setProperty color "1" s

-- Function to draw the faces of the cube
drawFace :: forall e. Context2D -> Array Vect3 -> Array Int -> Number -> Number -> Number
              -> Eff (canvas :: CANVAS | e) Unit
drawFace ctx vertices faces rx ry rz = void $ do
  (Vect2 v0) <- pure $ toScreen $ rotate (getFaceVert vertices faces 0) rx ry rz
  (Vect2 v1) <- pure $ toScreen $ rotate (getFaceVert vertices faces 1) rx ry rz
  (Vect2 v2) <- pure $ toScreen $ rotate (getFaceVert vertices faces 2) rx ry rz
  (Vect2 v3) <- pure $ toScreen $ rotate (getFaceVert vertices faces 3) rx ry rz
  _ <-  setStrokeStyle (property color "0") ctx
  _ <- setLineWidth 4.5 ctx
  _ <- beginPath ctx
  _ <- moveTo ctx v0.x v0.y
  _ <- lineTo ctx v1.x v1.y
  _ <- lineTo ctx v2.x v2.y
  _ <- lineTo ctx v3.x v3.y
  _ <- lineTo ctx v0.x v0.y
  _ <- stroke ctx
  pure unit

-- Rotating 
rotateX :: Vect3 -> Number -> Vect3 
rotateX (Vect3 v) qx = do
  let sinA = sin(qx)
  let cosA = cos(qx)
  let qy = v.y * cosA - v.z * sinA
  let qz = v.y * sinA + v.z * cosA
  Vect3 {
    x: v.x, y: qy, z: qz
  }
rotateY :: Vect3 -> Number -> Vect3
rotateY (Vect3 v) qy = do
  let sinA = sin(qy)
  let cosA = cos(qy)
  let qx = v.x * cosA + v.z * sinA
  let qz = -v.x * sinA + v.z * cosA
  Vect3 {
    x: qx, y: v.y, z: qz
  }

rotateZ :: Vect3 -> Number -> Vect3
rotateZ (Vect3 v) qz = do
  let sinA = sin(qz)
  let cosA = cos(qz)
  let qx = v.x * cosA - v.y * sinA
  let qy = v.x * sinA + v.y * cosA
  Vect3 {
    x: qx, y: qy, z: v.z
  }

rotate :: Vect3 -> Number -> Number -> Number -> Vect3
rotate (Vect3 vec) qx qy qz = rotateZ ( rotateY ( rotateX (Vect3 vec) (toRadians qx)) (toRadians qy)) (toRadians qz)  

toRadians :: Number -> Number
toRadians angle = angle * pi / 180.0


diff :: Vect2
diff = Vect2 { x: 0.0,y: 0.0 }

previousMouse :: Vect2
previousMouse = Vect2 { x: 0.0,y: 0.0 }

rotation' :: Vect3
rotation' = Vect3 { x: 0.0,y: 0.0,z: 0.0 }

rotationAngles :: Array Number
rotationAngles = [0.0,0.0,0.0]

-- resetRotateX :: Unit
-- resetRotateX = setProperty rotation' "x" 0.0

-- resetRotateY :: Unit
-- resetRotateY = setProperty rotation' "y" 0.0

resetRotationX :: Unit
resetRotationX =  setProperty rotationAngles "0" 0.0

resetRotationY :: Unit
resetRotationY =  setProperty rotationAngles "1" 0.0

resetRotationZ :: Unit
resetRotationZ =  setProperty rotationAngles "2" 0.0



isDragged  :: Array Boolean
isDragged = [ false ]


onMouseDown :: forall e. Event -> Eff (canvas :: CANVAS | e) Unit
onMouseDown evt = void $ do
   _ <- pure $ setProperty isDragged "0" true
   pure unit  

onMouseUp :: forall e. Event -> Eff (canvas :: CANVAS | e) Unit
onMouseUp evt = void $ do
   _ <- pure $ setProperty isDragged "0" false
   pure unit     
    

onMouseMove :: forall e. Event -> Eff (canvas :: CANVAS | e) Unit
onMouseMove evt = void $ do
    let (offSetX :: Number) = property evt "offsetX"
    let (offSetY :: Number) = property evt "offsetY"

    _ <- if (property isDragged "0") then do
            let (prevMouseX :: Number) = property previousMouse "x"
            let (prevMouseY :: Number) = property previousMouse "y"
            _ <- pure $ setProperty diff "x" (offSetX - prevMouseX)
            _ <- pure $ setProperty diff "y" (offSetY - prevMouseY)
            pure unit

            else do
                pure unit

    _ <- pure $ setProperty previousMouse "x" offSetX
    _ <- pure $ setProperty previousMouse "y" offSetY

    pure unit 


drawCube :: forall e. Context2D -> Number -> Number -> Number -> Eff (canvas :: CANVAS | e) Unit
drawCube ctx qx qy qz = void $ do
  drawFace ctx vertices (getFace faces 0) qx qy qz
  drawFace ctx vertices (getFace faces 1) qx qy qz
  drawFace ctx vertices (getFace faces 2) qx qy qz
  drawFace ctx vertices (getFace faces 3) qx qy qz
  drawFace ctx vertices (getFace faces 4) qx qy qz
  drawFace ctx vertices (getFace faces 5) qx qy qz
  pure unit 


-- main :: forall e. Eff (console :: CONSOLE | e) Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  addEventListener canvas "mouseup" onMouseUp
  addEventListener canvas "mousedown" onMouseDown
  addEventListener canvas "mousemove" onMouseMove
  pure $ addBtnListener "red" "#FF0000" setColor
  pure $ addBtnListener "blue" "#0000ff" setColor
  pure $ addBtnListener "green" "#00ff00" setColor 
  pure $ addBtnListener "brown" "#8B4513" setColor
  pure $ addBtnListener "orange" "#ffa500" setColor
  pure $ addBtnListener "black" "#000000" setColor 
  pure $ addBtnListener "red-back" "#FF0000" setBackColor
  pure $ addBtnListener "blue-back" "#0000ff" setBackColor
  pure $ addBtnListener "green-back" "#00ff00" setBackColor 
  pure $ addBtnListener "brown-back" "#8B4513" setBackColor
  pure $ addBtnListener "orange-back" "#ffa500" setBackColor
  pure $ addBtnListener "black-back" "#000000" setBackColor 
  pure $ resetDefault "reset" setColor setBackColor resetRotationX resetRotationY resetRotationZ
  
  animate ctx updateCube
  pure unit

updateCube :: forall e. Context2D  -> Eff (canvas :: CANVAS | e) Unit
updateCube ctx = void $ do

  let (diffX :: Number) = property diff  "x"
  let (diffY :: Number) = property diff "y"
  let (oldQx :: Number) = property rotation' "x"
  let (oldQy :: Number) = property rotation' "y"
  _ <- pure $ setProperty rotation' "x" (oldQx - diffY * 0.35)
  _ <- pure $ setProperty rotation' "y" (oldQy - diffX * 0.35)

  -- let (qx:: Number) = property rotation' "x"
  -- let (qy:: Number) = property rotation' "y"
  -- let (qz:: Number) = property rotation' "z"
    
  _ <- pure $ setProperty rotationAngles "0" (property rotation' "x")
  _ <- pure $ setProperty rotationAngles "1" (property rotation' "y")
  _ <- pure $ setProperty rotationAngles "2" (property rotation' "z")
  pure unit
  
  _ <- setFillStyle (property color "1") ctx
  _ <- fillRect ctx {x: 0.0, y: 0.0, w: 1000.0, h: 1000.0}
  _ <- drawCube ctx (property rotationAngles "0") (property rotationAngles "1") (property rotationAngles "2")
  pure unit
                  

  -- let rot = timeStamp / 10.0 * 0.15
 
  -- _ <- drawCube ctx rot 0.0 0.0
  -- pure unit