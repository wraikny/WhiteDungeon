﻿module WhiteDungeon.App.GameSetting

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry

open wraikny.Tart.Helper.Collections
open wraikny.MilleFeuille.Fs.Math
open wraikny.MilleFeuille.Fs.UI
open WhiteDungeon
open WhiteDungeon.Core
open WhiteDungeon.Core.Game
open WhiteDungeon.Core.Game.Model
open WhiteDungeon.Core.Game.Model

open WhiteDungeon.Core.Game.Model.Actor
open WhiteDungeon.Core.Game.Model.Actor.Skill

open FSharpPlus.Math.Applicative
open FSharpPlus

let enemies = [
    "Slime", {
        EnemySetting.actorStatus = {
            //Model.ActorStatus.level = 1
            hp = 50.0f
            walkSpeed = 2.0f
            dashSpeed = 4.0f
        }

        skillCoolTime = 120us
        skill = fun model (enemy : Enemy) ->
            let actor = enemy.actor
            let dir = 
                enemy.lookingRadian
                |> Vec2.fromAngle

            let pos = actor.objectBase.position + (100.0f *. dir)
            [
                AreaBuilder {
                    skillBase = {
                        delay = 0u
                        effects = [| Skill.Damage 15.0f |]
                    }
                    objectBase = ObjectBase.init (one .* 100.0f) pos

                    target = Skill.AreaTarget.Players

                    removeWhenHitWall = true
                    removeWhenHitActor = true

                    move = [
                        for _ in 1..60 -> Skill.Move (dir .* 5.0f)
                        for _ in 1..30 -> Skill.Scale(one .* 5.0f)
                    ]
                }
            ]

        visionAngleRate = 0.25f
        visionDistance = 1000.0f

        //chaseKind = ChaseKind.Losable AfterLoseSight.ChaseLosePoint
        freeMove = FreeMove.WithRotate 300us

        attackRange = 50.0f
        attackDistance = 300.0f

        hateDecrease = 0.01f
        exPoint = 5us

        popFrequency = 10us
    }
]

let enemiesCount = enemies.Length

let gameSetting : Model.GameSetting = {
    Model.GameSetting.dungeonCellSize = one .* 256.0f
    minPlayerCount = 1
    maxPlayerCount = 1
    binarySearchCountMovingOnWall = 4

    visionWallCheckCount = 8u

    enemyUpdateDistance = 10000.0f

    damageCalculation = fun v invoker target ->
        v * float32 invoker.level / float32 target.level

    occupationSettings = HashMap.ofList [
        Character.Bushi.viewSetting.name, Character.Bushi.setting
    ]

    lvUpExp = fun level -> 50us * level * level

    maxLevel = 100us
    playerGrowthRateOverMax = 0.8f
    levelSD = 2.0f

    createDungeonBuilder = fun (dungeonFloor : uint16) (initSize : uint16) ->
        let iSizef = 1.0f + float32 initSize
        let df = float32 dungeonFloor
        let inline f a x = (float32 x) * iSizef + df * a
        let inline fi a x = f a x |> floor |> int
        //let inline f0 x = f 0.0f x

        {
            seed = 0

            roomCount = fi 5.0f 100
            minRoomSize = (fi 0.05f 3, fi 0.05f 2)
            maxRoomSize = (fi 0.1f 6, fi 0.1f 4)
            roomGeneratedRange = (f 2.0f 60.0f, f 2.0f 30.0f)
            corridorWidth = fi 0.02f 1

            roomMoveRate = 0.3f
            roomMeanThreshold = 1.25f
            restoreEdgeRate = 0.2f
        }

    gateCount = fun (dungeonFloor : uint16) (initSize : uint16) ->
        (float32 initSize) + 0.01f * (float32 dungeonFloor)
        |> floor
        |> int

    enemySettings = HashMap.ofList enemies
    enemyFrequencySum = enemies |> Seq.sumBy(fun x -> (snd x).popFrequency)
    enemyFrequencyRanges = [|
        let mutable fsum = 0us
        for (k, x) in enemies do
            let ls = fsum
            fsum <- fsum + x.popFrequency
            yield ( k, (ls, fsum) )
    |]
}