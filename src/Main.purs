module Main where
import Graphics.Canvas
import Math ( pi, sin, cos)
import Control.Monad.Eff.Console
import Control.Monad.Eff (Eff)
--import Control.Monad.Eff.Ref
import Partial.Unsafe(unsafePartial)
import Data.Maybe (Maybe(..))
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef, Ref)
import FFI.Util (property, setProperty)
import Prelude

foreign import anim :: forall e. Context2D
                -> (Context2D -> Number -> Eff (canvas :: CANVAS | e) Unit)
                -> Eff (canvas :: CANVAS | e) Unit

foreign import data Event :: Type
--
foreign import addEventListener :: forall a. CanvasElement -> String
                                    -> (Event -> Eff (canvas :: CANVAS | a) Unit)
                                    -> Eff (canvas :: CANVAS | a) Unit


--
newtype Vector2 =Vector2 { x:: Number,   y:: Number }
newtype Vector3= Vector3 { x:: Number,   y:: Number,   z:: Number }
newtype Angle = Angle    { ax:: Number,  ay:: Number,  az::Number }
newtype Cube = Cube      { size::Number,   color::String }

-- scale the cube to the canvas
scale :: Vector3 -> Vector2
scale (Vector3 v) = Vector2 {
            x : (v.x / v.z) * halfHeight + halfWidth ,
            y : -v.y / v.z * halfHeight + halfHeight
            -- x: v.x * halfHeight + halfWidth,
            -- y: v.y * halfHeight + halfHeight
            }
    where
        halfWidth = 250.0
        halfHeight = 250.0

projT :: Vector3 -> Vector2
projT (Vector3 v) = scale (Vector3 {
        x: v.x, y: v.y, z: v.z +3.0
    })

--projecting the slope
rotaX :: Vector3 -> Number -> Vector3
rotaX (Vector3 vec) angle = do
    let sinA = sin(angle)
    let cosA = cos(angle)
    let rty = vec.y * cosA - vec.z * sinA
    let rtz = vec.y * sinA +vec.z * cosA
    Vector3  { x: vec.x, y: rty, z:rtz
    }
rotaY :: Vector3 -> Number -> Vector3
rotaY (Vector3 vec) angle = do
    let sinA = sin(angle)
    let cosA = cos(angle)
    let rtx = vec.x * cosA + vec.z * sinA
    let rtz = -vec.x * sinA + vec.z * cosA
    Vector3  { x: rtx, y: vec.y, z:rtz
    }
-- rotaZ :: Vector3 -> Number -> Vector3
-- rotaZ (Vector3 vec) angle = do
--     let sinA = sin(angle)
--     let cosA = cos(angle)
--     let rtx = vec.x * cosA - vec.y * sinA
--     let rty = vec.x * sinA + vec.y * cosA
--     Vector3  { x: rtx , y: rty, z: vec.z
--     }

rotate :: Vector3 -> Number -> Number  -> Vector3
-- rotate (Vector3 vec) rx ry rz = rotaZ (rotaY (rotaX (Vector3 vec) (rad rx)) (rad ry)) (rad rz)
rotate (Vector3 vec) rx ry = rotaY (rotaX (Vector3 vec) (rad rx)) (rad ry)

rad :: Number -> Number
rad ang = ang * pi /180.0

-- proj :: Vector3 -> Angle -> Vector2
-- proj ( Vector3 v ) ( Angle a ) =
--
--     Vector2 {x:}
--
--     -- let x_rot_az = (v.x * (cos a.az) ) + (v.y * (sin a.az) )
--         -- y_rot_az = v.y * (cos a.az) - v.x * (sin a.az)
--         -- y_rot_az_ax = y_rot_az * (cos a.ax) + v.z * (sin a.ax)
--         -- z_rot_az_ax = v.z * (cos a.az) - y_rot_az * (sin a.ax)
--         -- x_rot_az_ax_ay = x_rot_az * (cos a.ay) + z_rot_az_ax * (sin a.ay)
--     -- in
--     --     Vector2 {x: x_rot_az_ax_ay / (v.z + 2.5), y : y_rot_az_ax / (v.z + 2.5) }
--


withStroke :: forall e. Context2D -> String -> (Context2D -> Eff (canvas :: CANVAS | e) Context2D) ->  Eff (canvas :: CANVAS | e) Context2D
withStroke ctx color draw = withContext ctx $ do
  ctx <- setStrokeStyle color ctx
  ctx <- beginPath ctx
  ctx <- draw ctx
  stroke ctx

-- --Drawing a
dLine :: forall eff. Context2D -> Vector2 -> Vector2 -> Eff(canvas::CANVAS |eff) Context2D
dLine ctx (Vector2 f) (Vector2 t) = do
    ctx <- moveTo ctx f.x f.y
    lineTo ctx t.x t.y


dcube :: forall eff. Context2D -> Cube -> Angle -> Eff(canvas :: CANVAS | eff) Context2D
dcube ctx (Cube { size, color }) (Angle a) = do
    let vertex1 = projT $ rotate (Vector3 { x:  -size, y:  -size, z:  -size }) a.ax a.ay-- a.az
    let vertex2 = projT $ rotate (Vector3 { x:  -size, y:  size, z:  -size }) a.ax a.ay-- a.az
    let vertex3 = projT $ rotate (Vector3 { x:  -size, y:  -size, z:  size }) a.ax a.ay-- a.az
    let vertex4 = projT $ rotate (Vector3 { x:  -size, y:  size, z:  size }) a.ax a.ay-- a.az
    let vertex5 = projT $ rotate (Vector3 { x:  size, y:  -size, z:  -size }) a.ax a.ay-- a.az
    let vertex6 = projT $ rotate (Vector3 { x:  size, y:  size, z:  -size }) a.ax a.ay-- a.az
    let vertex7 = projT $ rotate (Vector3 { x:  size, y:  -size, z:  size }) a.ax a.ay-- a.az
    let vertex8 = projT $ rotate (Vector3 { x:  size, y:  size, z:  size }) a.ax a.ay-- a.az
    -- let vertex1 = scale $ proj (Vector3 { x:  -size, y:  -size, z:  -size }) (Angle { ax: a.ax, ay: a.ay, az: a.az })
    -- let vertex2 = scale $ proj (Vector3 { x:  -size, y:  size, z:  -size }) (Angle { ax: a.ax, ay: a.ay, az: a.az })
    -- let vertex3 = scale $ proj (Vector3 { x:  -size, y:  -size, z:  size }) (Angle { ax: a.ax, ay: a.ay, az: a.az })
    -- let vertex4 = scale $ proj (Vector3 { x:  -size, y:  size, z:  size }) (Angle { ax: a.ax, ay: a.ay, az: a.az })
    -- let vertex5 = scale $ proj (Vector3 { x:  size, y:  -size, z:  -size }) (Angle { ax: a.ax, ay: a.ay, az: a.az })
    -- let vertex6 = scale $ proj (Vector3 { x:  size, y:  size, z:  -size }) (Angle { ax: a.ax, ay: a.ay, az: a.az })
    -- let vertex7 = scale $ proj (Vector3 { x:  size, y:  -size, z:  size }) (Angle { ax: a.ax, ay: a.ay, az: a.az })
    -- let vertex8 = scale $ proj (Vector3 { x:  size, y:  size, z:  size }) (Angle { ax: a.ax, ay: a.ay, az: a.az })

    withStroke ctx color \ctx -> do --top face
     ctx <- dLine ctx vertex1 vertex5
     ctx <- dLine ctx vertex5 vertex6
     ctx <- dLine ctx vertex6 vertex2
     ctx <- dLine ctx vertex2 vertex1
        --bottom face
     ctx <- dLine ctx vertex3 vertex7
     ctx <- dLine ctx vertex7 vertex8
     ctx <- dLine ctx vertex8 vertex4
     ctx <- dLine ctx vertex4 vertex3
        --side face
     ctx <- dLine ctx vertex1 vertex3
     ctx <- dLine ctx vertex5 vertex7
     ctx <- dLine ctx vertex6 vertex8
     dLine ctx vertex2 vertex4

deltaMove :: Vector2
deltaMove = Vector2 { x: 0.0, y: 0.0}

previous :: Vector2
previous = Vector2 { x: 0.0, y: 0.0}

rotation :: Vector3
rotation = Vector3 { x: 0.0, y: 0.0, z: 0.0}

dragging :: Array Boolean
dragging = [ false ]

keying :: Array Boolean
keying = [ false ]

acceleration :: Array Number
acceleration = [ 0.0 ]

onKeyDown :: forall e. Event ->  Eff (canvas :: CANVAS | e) Unit
onKeyDown evt = void $ do
    let (keyCode :: Int) = property evt "keyCode"

    let delX = case keyCode of
          37 -> -4.0
          39 -> 4.0
          _ ->  0.0

    let delY = case keyCode of
          38 -> -4.0
          40 ->  4.0
          _  ->  0.0

    _ <- pure $ setProperty deltaMove "x" delX
    _ <- pure $ setProperty deltaMove "y" delY

    let accel = (property acceleration "0") + 3.0
    pure $ setProperty acceleration "0" accel

    pure unit

onMouseDown :: forall e. Event -> Eff (canvas :: CANVAS| e) Unit
onMouseDown evt = void $ do
    _ <- pure $ setProperty dragging "0" true
    pure unit
onMouseUp :: forall e. Event -> Eff (canvas :: CANVAS| e) Unit
onMouseUp evt = void $ do
    _<- pure $ setProperty dragging "0" false
    pure unit

onMouseMove :: forall e. Event -> Eff (canvas :: CANVAS| e) Unit
onMouseMove evt = void $ do
    let (offSetX :: Number) = property evt "offsetX"
    let (offSetY :: Number) = property evt "offsetY"
    _ <- if(property dragging "0" ) then do
        let (prevMouseX :: Number) = property previous "x"
        let (prevMouseY :: Number) = property previous "y"
        _ <- pure $ setProperty deltaMove "x" (offSetX - prevMouseX)
        _ <- pure $ setProperty deltaMove "y" (offSetY - prevMouseY)
        let accel = (property acceleration "0") + 3.0
        pure $ setProperty acceleration "0" accel
        pure unit
        else do
                pure unit
    _ <- pure $ setProperty previous "x" offSetX
    _ <- pure $ setProperty previous "y" offSetY
    pure unit

drawBackground :: forall e. Context2D -> Eff (canvas :: CANVAS | e) Context2D
drawBackground ctx = do
  _ <- setFillStyle "rgb(122,230,232)" ctx
  fillRect ctx { x: 0.0, y: 0.0, w: 500.0, h: 500.0 }

max:: Number
max = pi/4.0
may::Number
may=pi/3.0
maz::Number
maz=pi/4.0
acc::Number
acc=0.998
drag::Boolean
drag=false

main = void $ unsafePartial do
    Just canvas <- getCanvasElementById "canvas"
    ctx <- getContext2D canvas
    addEventListener canvas "mouseup" onMouseUp
    addEventListener canvas "mousedown" onMouseDown
    addEventListener canvas "mousemove" onMouseMove
    addEventListener canvas "keydown" onKeyDown
    anim ctx updateCube
    pure unit

updateCube :: forall a. Context2D -> Number -> Eff (canvas :: CANVAS | a) Unit
updateCube ctx timeStamp = void $ do
    _ <- drawBackground ctx
    let cube = Cube { size:  0.5, color: "#000" }
    --let angle = Angle { ax: max, ay: may, az: maz }
    let (accel :: Number) = property acceleration "0"
    _ <- if (accel > 0.0) then do
            _ <- pure $ setProperty acceleration "0" (accel - 1.0)
            let (delX :: Number) = property deltaMove "x"
            let (delY :: Number) = property deltaMove "y"
            _ <- pure $ setProperty rotation "x" ((property rotation "x") - delY)
            _ <- pure $ setProperty rotation "y" ((property rotation "y") - delX)
            pure unit

            else do
                pure unit

    let change1 = Angle { ax: property rotation  "x", ay: property rotation  "y", az:0.0 }
    _ <- drawBackground ctx
    _ <- dcube ctx cube change1

    pure unit
