﻿namespace WhiteDungeon.Core

open wraikny.Tart.Helper.Math

open WhiteDungeon.Core
open WhiteDungeon.Core.Model

open FSharpPlus


[<Struct>]
type InputDirection =
    | Right
    | Left
    | Up
    | Down



type PlayerInput =
    | Select
    | Cancel
    | Dash
    | Direction of InputDirection
    | Skill of SkillKind


module PlayerInput =
    let inputs = [
        Direction Right
        Direction Left
        Direction Up
        Direction Down
        Direction Down
    ]

    let getPlayerMoveFromInputs (isDash) (inputSet : InputDirection Set) : ActorMove * float32 Vec2 =
        let actorMove = if isDash then ActorMove.Dash else ActorMove.Walk

        let moveDirs = [|
            Up, Vec2.init 0.0f -1.0f
            Down, Vec2.init 0.0f 1.0f
            Right, Vec2.init 1.0f 0.0f
            Left, Vec2.init -1.0f 0.0f
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
    //| PlayerSkill of PlayerID * SkillKind
    | GenerateNewDungeon
    | GeneratedDungeonModel of DungeonBuilder * DungeonModel
    | GeneratedDungeonParams of Dungeon.GeneratedDungeonParams

    | UpdateEnemyOf of EnemyID * EnemyMsg
    //#if DEBUG
    ///// for Debug
    //| AppendSkillEmits
    //#endif
