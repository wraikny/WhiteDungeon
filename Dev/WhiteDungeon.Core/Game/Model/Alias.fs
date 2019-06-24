namespace WhiteDungeon.Core.Game.Model

[<Struct>]
type PlayerID = PlayerID of id : uint32 with
    member inline this.Value = this |> function | PlayerID x -> x

[<Struct>]
type EnemyID = EnemyID of id : uint32 with
    member inline this.Value = this |> function | EnemyID x -> x