namespace WhiteDungeon.Core.Model

type Level = int

type HP = float32
type Atk = float32
type Def = float32
type Speed = float32

[<Struct>]
type PlayerID = PlayerID of id : uint32 with
    member this.Value = this |> function | PlayerID x -> x