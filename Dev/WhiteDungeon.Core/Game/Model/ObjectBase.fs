namespace WhiteDungeon.Core.Game.Model

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry


type ObjectBase = {
    rect : float32 Rect
}

module ObjectBase =
    let rect o = o.rect