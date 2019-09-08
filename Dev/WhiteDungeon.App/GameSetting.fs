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
            Model.ActorStatus.level = 1
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
        freeMove = FreeMove.Forward

        attackRange = 50.0f
        attackDistance = 300.0f
    }
]

let enemiesCount = enemies.Length

let gameSetting : Model.GameSetting = {
    Model.GameSetting.dungeonCellSize = Vec2.init 250.0f 250.0f
    minPlayerCount = 1
    maxPlayerCount = 1
    binarySearchCountMovingOnWall = 4

    visionWallCheckCount = 8u

    enemyUpdateDistance = 20000.0f

    characterSize = Vec2.init 100.0f 100.0f

    damageCalculation = fun v invoker target ->
        v * float32 invoker.statusCurrent.level / float32 target.statusCurrent.level

    occupationSettings = HashMap.ofList [
        Character.Bushi.viewSetting.name, Character.Bushi.setting
    ]

    enemySettings = HashMap.ofList enemies

    intToEnemy = fun i ->
        enemies.[i % enemiesCount] |> fst
}