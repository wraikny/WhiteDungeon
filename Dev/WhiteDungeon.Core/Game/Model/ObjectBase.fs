namespace WhiteDungeon.Core.Game.Model

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry

open WhiteDungeon.Core.Model

open FSharpPlus

[<Struct>]
type MoveDirection =
    | Front
    | Back
    | Right
    | Left
    | FrontRight
    | FrontLeft
    | BackRight
    | BackLeft


module MoveDirection =
    let fromVector v =
        let pi2 = 2.0f * Angle.pi
        let angle = (pi2 + Vec2.angle v) % pi2
        let a = angle * 8.0f / Angle.pi
        let bw s t = s <= a && a < t
        let result =
            if bw 1.0f 3.0f then
                FrontRight
            elif bw 3.0f 5.0f then
                Front
            elif bw 5.0f 7.0f then
                FrontLeft
            elif bw 7.0f 9.0f then
                Left
            elif bw 9.0f 11.0f then
                BackLeft
            elif bw 11.0f 13.0f then
                Back
            elif bw 13.0f 15.0f then
                BackRight
            else
                Right
        
        // printfn "%A %A %A" v angle result

        result


    let toVector dir =
        dir |> function
        | Front -> (0, 1)
        | Back -> (0, -1)
        | Right -> (1, 0)
        | Left -> (-1, 0)
        | FrontRight -> (1, 1)
        | FrontLeft -> (-1, 1)
        | BackRight -> (1, -1)
        | BackLeft -> (-1, -1)
        |> uncurry Vec2.init
        |>> float32
        |> Vector.normalize


type ObjectBase = {
    size : float32 Vec2

    /// center
    position : float32 Vec2

    lastPosition : float32 Vec2

    velocity : float32 Vec2

    direction : MoveDirection

    isMoved : bool
}

module ObjectBase =
    let inline size o = o.size
    
    let inline position o = o.position

    let inline lastPosition o = o.lastPosition

    let inline velocity o = o.velocity

    let inline direction o = o.direction

    let inline area (o : ObjectBase) : float32 Rect2 =
        {
            position = o.position - (o.size .* 0.5f)
            size = o.size
        }

    let inline init size position = {
        size = size
        position = position
        lastPosition = position
        velocity = zero
        direction = Front
        isMoved = false
    }

    let inline get (x : ^a) =
        (^a : (static member objectBase : ^a -> ObjectBase) x)

    let inline map f (x : ^a) =
        (^a : (static member MapObjectBase : ^a * (ObjectBase -> ObjectBase) -> ^b) (x, f))