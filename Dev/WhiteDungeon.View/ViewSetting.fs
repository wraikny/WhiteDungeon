﻿namespace WhiteDungeon.View

open wraikny.Tart.Helper
open wraikny.Tart.Helper.Collections
open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry

open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model

type TexParam<'a, 'b> = 'a * 'b * float32

type ActorImages<'a, 'b> = {
    sleepWalk : uint32
    sleepDash : uint32
    front : TexParam<'a, 'b> list
    back : TexParam<'a, 'b> list
    right : TexParam<'a, 'b> list
    left : TexParam<'a, 'b> list
    frontRight : TexParam<'a, 'b> list
    frontLeft : TexParam<'a, 'b> list
    backRight : TexParam<'a, 'b> list
    backLeft : TexParam<'a, 'b> list
}

module ActorImages =
    let empty =
        {
            sleepWalk = 0u
            sleepDash = 0u
            front = []
            back = []
            right = []
            left = []
            frontRight = []
            frontLeft = []
            backRight = []
            backLeft = []
        }
    let fromDirection dir images =
        dir |> function
        | MoveDirection.Front -> images.front
        | MoveDirection.FrontRight -> images.frontRight
        | MoveDirection.FrontLeft -> images.frontLeft
        | MoveDirection.Back -> images.back
        | MoveDirection.BackRight -> images.backRight
        | MoveDirection.BackLeft -> images.backLeft
        | MoveDirection.Right -> images.right
        | MoveDirection.Left -> images.left

    let map f x =
        {
            sleepWalk = x.sleepWalk
            sleepDash = x.sleepDash
            front = List.map f x.front
            back = List.map f x.back
            right = List.map f x.right
            left = List.map f x.left
            frontRight = List.map f x.frontRight
            frontLeft = List.map f x.frontLeft
            backRight = List.map f x.backRight
            backLeft = List.map f x.backLeft
        }

    // http://www.silversecond.com/WolfRPGEditor/
    let fromGraphicmaker sleepWalk sleepDash path =
        let size = Vec2.init 32 64
        let textures xi yi =
            [1; 2; 1; 0]
            |> List.map (fun i ->
                ( path
                , Rect.init (size * (Vec2.init (i + xi * 3) yi)) size
                , 0.0f)
            )
        {
            sleepWalk = sleepWalk
            sleepDash = sleepDash
            front = textures 0 0
            frontLeft = textures 1 0
            left = textures 0 1
            frontRight = textures 1 1
            right = textures 0 2
            backLeft = textures 1 2
            back = textures 0 3
            backRight = textures 1 3
        }

type ObjectViewSetting =
    {
        name : string
        characterImages : ActorImages<string, int Rect2>
    }

type DamageViewSetting =
    {
        font : string
        size : int
        sizeOutline : int
        color : byte Vec3
        colorOutline : byte Vec3
    }

type GameViewSetting = {
    occupationSetting : HashMap<Occupation, ObjectViewSetting>
    bgms : string list

    gameUIFrameColor : byte Vec4
    gameUITextColor : byte Vec3
    gameUITextFont : string
    gameUITextSize : int 
    gameUIDungeonFloor : float32 Rect2
    gameUIPlayerArea : float32 Rect2

    dungeonCellTexture : string

    damageTextFrame : int
    damageTextMove : float32
    damageView : DamageViewSetting
    healView : DamageViewSetting
}

module GameViewSetting =
    [<Literal>]
    let modForCulling = 40000.0f

open wraikny.MilleFeuille.Fs.UI


type MainSceneSetting = {
    backColor : byte Vec3
    frameColor : byte Vec4
    buttonColor : ButtonColor
    inputColor : ButtonColor
    inputFocusColor : ButtonColor

    textSize : int

    inputFont : string
    inputFontColor : byte Vec3
    inputFontSize : int

    windowWidthWRate : float32
    window2MarginRate : float32
    window2HeightRate : float32

    bgm : string
    
#if !DEBUG
    titleFont : string
    headerFont : string
#endif
}


type AppSetting = {
    windowSize : int Vec2

    menuSceneSetting : MainSceneSetting

    gameViewSetting : GameViewSetting
    gameSetting : GameSetting
}