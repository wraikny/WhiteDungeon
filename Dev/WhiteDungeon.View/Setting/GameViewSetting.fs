﻿namespace WhiteDungeon.View

open wraikny.Tart.Helper
open wraikny.Tart.Helper.Math

open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model

type ActorImages = {
    front : string
    frontRight : string
    frontLeft : string
    back : string
    backRight : string
    backLeft : string
    right : string
    left : string
}

module ActorImages =
    let fromDirection dir images =
        let path = dir |> function
            | MoveDirection.Front -> images.front
            | MoveDirection.FrontRight -> images.frontRight
            | MoveDirection.FrontLeft -> images.frontLeft
            | MoveDirection.Back -> images.back
            | MoveDirection.BackRight -> images.backRight
            | MoveDirection.BackLeft -> images.backLeft
            | MoveDirection.Right -> images.right
            | MoveDirection.Left -> images.left

        path

type GameViewSetting = {
    occupationImages : Map<Occupation, ActorImages>
}

open wraikny.MilleFeuille.Fs.UI

type MenuSceneSetting = {
    backColor : byte Vec3
    frameColor : byte Vec4
    buttonColor : ButtonColor
    
#if !DEBUG
    titleFont : string
    buttonFont : string
    textFont : string
#endif
}


type AppSetting = {
    windowSize : int Vec2

    menuSceneSetting : MenuSceneSetting

    gameViewSetting : GameViewSetting
    gameSetting : GameSetting
}
