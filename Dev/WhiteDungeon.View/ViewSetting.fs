namespace WhiteDungeon.View

type ViewSetting = {
    uiFontPath : string

    menuButtonSize : asd.Vector2DF
    menuButtonFontSize : int

    messageFontSize : int

    titleTextFontSize : int
    titleText : string

    button1 : asd.Vector2DF
    button2 : asd.Vector2DF
    button3 : asd.Vector2DF
}

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
    inputColor : ButtonColor
    inputFocusColor : ButtonColor

    inputFont : string
    inputFontColor : byte Vec3
    inputFontSize : int

    windowWidthWRate : float32
    window2MarginRate : float32
    
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