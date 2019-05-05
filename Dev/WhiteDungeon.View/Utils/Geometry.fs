namespace WhiteDungeon.View.Utils.Geometry

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry

open WhiteDungeon.View.Utils.Math

module Rect =
    let toRectF (r : float32 Rect) =
        new asd.RectF(
            r.position |> Vec2.toVector2DF
            , r.size |> Vec2.toVector2DF
        )

    let toRectI (r : int Rect) =
        new asd.RectI(
            r.position |> Vec2.toVector2DI
            , r.size |> Vec2.toVector2DI
        )