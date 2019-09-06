﻿module WhiteDungeon.App.GameSetting

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry

open wraikny.MilleFeuille.Fs.Math
open wraikny.MilleFeuille.Fs.UI
open WhiteDungeon
open WhiteDungeon.Core
open WhiteDungeon.Core.Game
open WhiteDungeon.Core.Game.Model

open WhiteDungeon.Core.Game.Model.Skill

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
}