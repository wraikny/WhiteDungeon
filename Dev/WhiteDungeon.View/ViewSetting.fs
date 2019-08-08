namespace WhiteDungeon.View

open wraikny.Tart.Helper
open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry

open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model

type TexParam<'a, 'b> = 'a * 'b * float32

type ActorImages<'a, 'b> = {
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

    let map f x =
        {
            front = List.map f x.front
            back = List.map f x.back
            right = List.map f x.right
            left = List.map f x.left
            frontRight = List.map f x.frontRight
            frontLeft = List.map f x.frontLeft
            backRight = List.map f x.backRight
            backLeft = List.map f x.backLeft
        }

type GameViewSetting = {
    occupationImages : Map<Occupation, ActorImages<string, int Rect2>>
}

open wraikny.MilleFeuille.Fs.UI

type MainSceneSetting = {
    backColor : byte Vec3
    frameColor : byte Vec4
    buttonColor : ButtonColor
    inputColor : ButtonColor
    inputFocusColor : ButtonColor

    inputFont : string
    inputFontColor : byte Vec3
    inputFontSize : int

    windowWidthWRate : float32
    window2MarginRate : float32
    window2HeightRate : float32
    
#if !DEBUG
    titleFont : string
    buttonFont : string
    textFont : string
#endif
}


type AppSetting = {
    windowSize : int Vec2

    menuSceneSetting : MainSceneSetting

    gameViewSetting : GameViewSetting
    gameSetting : GameSetting
}