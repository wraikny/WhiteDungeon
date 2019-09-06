﻿module WhiteDungeon.App.GameSetting

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry

open wraikny.MilleFeuille.Fs.Math
open wraikny.MilleFeuille.Fs.UI
open WhiteDungeon
open WhiteDungeon.Core
open WhiteDungeon.Core.Game
open WhiteDungeon.Core.Game.Model
open WhiteDungeon.Core.Game.Model

open WhiteDungeon.Core.Game.Model.Actor.Skill

open FSharpPlus.Math.Applicative
open FSharpPlus

let gameSetting : Model.GameSetting = {
    Model.GameSetting.dungeonCellSize = Vec2.init 250.0f 250.0f
    minPlayerCount = 1
    maxPlayerCount = 1
    binarySearchCountMovingOnWall = 4
    characterSize = Vec2.init 100.0f 100.0f
    damageCalculation = fun v invoker target ->
        v * float32 invoker.statusCurrent.level / float32 target.statusCurrent.level

    occupationSettings = Map.ofList [
        Model.Bushi, Character.Bushi.setting
    ]

    enemySettings = Map.ofList [
        Model.EnemyKind.Slime, {
            EnemySetting.actorStatus = {
                Model.ActorStatus.level = 1
                hp = 30.0f
                walkSpeed = 4.0f
                dashSpeed = 6.0f
            }
            skill = fun model actor -> []

            visionAngle = 360.0f
            visionDistance = 1000.0f
            chaseKind = Model.Actor.ChaseKind.Losable Actor.AfterLoseSight.ChaseLosePoint

            attackDistance = 200.0f
        }
    ]
}