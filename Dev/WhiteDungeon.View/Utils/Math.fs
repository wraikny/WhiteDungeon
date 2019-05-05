namespace WhiteDungeon.View.Utils.Math

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry


module Vec2 =
    let toVector2DF (v : float32 Vec2) =
        new asd.Vector2DF(v.x, v.y)

    let toVector2DI (v : int Vec2) =
        new asd.Vector2DI(v.x, v.y)