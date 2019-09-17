namespace WhiteDungeon.Core.Model

open wraikny.Tart.Helper.Extension
open wraikny.Tart.Helper.Math
open WhiteDungeon.Core.Model

open FSharpPlus

type ItemKind =
    | Heal of float32


type Item = {
    objectBase : ObjectBase
    id : uint32
    kind : ItemKind
} with
    static member inline SetObjectBase (x, y) : Item =
        { x with objectBase = y }


module Item =
    let inline init size position id kind = {
        objectBase = ObjectBase.init size position
        id = id
        kind = kind
    }

    let inline kind x = x.kind
