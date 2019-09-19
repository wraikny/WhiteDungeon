namespace WhiteDungeon.Core.Model

open wraikny.Tart.Helper.Extension
open wraikny.Tart.Math
open WhiteDungeon.Core.Model

open FSharpPlus

type BuildingKind =
    | Gate
    | Spring

type Building = {
    objectBase : ObjectBase
    id : uint32
    kind : BuildingKind
} with
    static member inline SetObjectBase (x, y) : Building =
        { x with objectBase = y }


module Building =
    let inline init size position id kind = {
        id = id
        kind = kind
        objectBase = ObjectBase.init size position
    }

    let inline kind x = x.kind