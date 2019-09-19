namespace WhiteDungeon.Core.Model

open wraikny.Tart.Math


open WhiteDungeon.Core.Model

open FSharpPlus


type ObjectBase = {
    size : float32 Vec2

    /// center
    position : float32 Vec2

    lastPosition : float32 Vec2

    velocity : float32 Vec2

    direction : MoveDirection

    isMoved : bool

    radius : float32
} with
    member inline x.objectBase = x
    static member inline SetObjectBase (_ : ObjectBase, y : ObjectBase) = y

    member inline x.Area = {
        position = x.position - (x.size ./ 2.0f)
        size = x.size
    }


module ObjectBase =
    let inline init size position = {
        size = size
        position = position
        lastPosition = position
        velocity = zero
        direction = Front
        isMoved = false

        radius = (Vector.length size) * 0.5f
    }

    let inline get (x : ^a) : ObjectBase =
        (^a : (member objectBase : _) x)

    let inline set (o : ObjectBase) (x : ^a) : ^a =
        (^a : (static member SetObjectBase : _*_->_) (x, o))

    let inline map f (x : ^a) : ^a = set (f (get x)) x

    let inline size x = (get x).size
    let inline position x = (get x).position
    let inline lastPosition x = (get x).lastPosition
    let inline velocity x = (get x).velocity
    let inline direction x = (get x).direction
    let inline area x : float32 Rect2 = (get x).Area

    let inline movingDirection x =
        (position x - lastPosition x)
        |> Vec2.angle

    let inline mapSize f x = map (fun o -> {o with size = f o.size}) x
    let inline mapPosition f x =
        x |> map (fun o ->
            { o with
                position = f o.position
                lastPosition = o.position
            })

    let inline mapVelocity f x =
        x |> map (fun o -> { o with velocity = f o.velocity })

    let inline calcAngle oFrom oTo =
        Vec2.angle(position oTo - position oFrom)

