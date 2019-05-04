namespace WhiteDungeon.Core.Game.Model

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry


type ObjectBase = {
    size : float32 Vec2

    /// center down
    position : float32 Vec2

    lastPosition : float32 Vec2

    velocity : float32 Vec2
}

module ObjectBase =
    let size o = o.size
    
    let position o = o.position

    let lastPosition o = o.lastPosition

    let velocity o = o.velocity

    let area (o : ObjectBase) : float32 Rect =
        let leftUp = o.position - { o.size with x = o.size.x * 0.5f }
        {
            position = leftUp
            size = o.size
        }

    let init size position =
        {
            size = size
            position = position
            lastPosition = position
            velocity = Vec2.zero()
        }