namespace WhiteDungeon.Core.Game.Msg

open wraikny.Tart.Helper.Math

open WhiteDungeon.Core.Game
open WhiteDungeon.Core.Game.Model

open FSharpPlus


[<Struct>]
type InputDirection =
    | Right
    | Left
    | Up
    | Down



type PlayerInput =
    | RightKey
    | LeftKey
    | UpKey
    | DownKey
    | DashKey
    //| Skill1Key
    //| Skill2Key


module PlayerInput =
    let inputs =
        [
            RightKey
            LeftKey
            UpKey
            DownKey
            DashKey
        ]

    let getPlayerMoveFromInputs (inputSet : PlayerInput Set) : Actor.ActorMove * float32 Vec2 =
        let actorMove =
            if inputSet |> Set.contains PlayerInput.DashKey then Actor.Dash else Actor.Walk

        let moveDirs = [|
            UpKey, Vec2.init 0.0f -1.0f
            DownKey, Vec2.init 0.0f 1.0f
            RightKey, Vec2.init 1.0f 0.0f
            LeftKey, Vec2.init -1.0f 0.0f
        |]

        let direction =
            moveDirs
            |> filter (fun (key, _) ->
                inputSet |> Set.contains key
            )
            |>> snd
            |> sum
            |> Vector.normalize
    
        actorMove, direction


open wraikny.Tart.Advanced.Dungeon


type Msg =
    | SetGameMode of Model.GameSceneMode
    | TimePasses
    | PlayerInputs of PlayerID * PlayerInput Set
    | PlayerSkill of PlayerID * Actor.SkillKind
    | GenerateNewDungeon
    | GeneratedDungeonModel of DungeonBuilder * DungeonModel
    | GeneratedDungeonParams of Dungeon.GeneratedDungeonParams

    | UpdateEnemyOf of EnemyID * Actor.EnemyMsg
    //#if DEBUG
    ///// for Debug
    //| AppendSkillEmits
    //#endif
