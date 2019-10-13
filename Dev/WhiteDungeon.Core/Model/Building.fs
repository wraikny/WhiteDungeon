namespace WhiteDungeon.Core.Model

open wraikny.Tart.Helper.Extension
open wraikny.Tart.Math
open WhiteDungeon.Core.Model
open wraikny.Tart.Advanced

open FSharpPlus

type BuildingKind =
    | Gate
    //| Spring

type Building = {
    id : uint32
    kind : BuildingKind
    luCell : int Vec2
    cellCount : int Vec2
    cells : Set<int Vec2>
    size: float32 Vec2
}


module Building =
    let inline init cellSize cellCount luCell id kind = {
        id = id
        kind = kind
        cellCount = cellCount
        luCell = luCell

        cells =
            seq {
                for x in 0 .. cellCount.x-1 do
                for y in 0 .. cellCount.y-1 do
                    yield Vec2.init (x + luCell.x) (y + luCell.y)
            }
            |> Set.ofSeq

        size = (map float32 cellCount) * cellSize
    }

    let inline kind x = x.kind