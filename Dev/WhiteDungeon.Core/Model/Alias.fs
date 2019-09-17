namespace WhiteDungeon.Core.Model

type Occupation = string
type EnemyKind = string
type SkillID = uint32

[<Struct>]
type CharacterID = CharacterID of int


[<Struct>]
type PlayerID = PlayerID of id : uint32 with
    member inline this.Value = this |> function | PlayerID x -> x

[<Struct>]
type EnemyID = EnemyID of id : uint32 with
    member inline this.Value = this |> function | EnemyID x -> x