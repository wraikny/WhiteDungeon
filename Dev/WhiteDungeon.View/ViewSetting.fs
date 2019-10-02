namespace WhiteDungeon.View

open wraikny.Tart.Helper
open wraikny.Tart.Helper.Collections
open wraikny.Tart.Math


open WhiteDungeon.Core.Model

open  FSharpPlus

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
            front = empty
            back = empty
            right = empty
            left = empty
            frontRight = empty
            frontLeft = empty
            backRight = empty
            backLeft = empty
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
            front = map f x.front
            back = map f x.back
            right = map f x.right
            left = map f x.left
            frontRight = map f x.frontRight
            frontLeft = map f x.frontLeft
            backRight = map f x.backRight
            backLeft = map f x.backLeft
        }

    // http://www.silversecond.com/WolfRPGEditor/
    let fromGraphicmaker sleepWalk sleepDash (path : string) =
        let size = Vec2.init 32 64
        let textures xi yi =
            [1; 2; 1; 0]
            |>> fun i ->
                ( path
                , Rect.init (size * (Vec2.init (i + xi * 3) yi)) size
                , 0.0f)
            
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

    let occupationImage sleepWalk sleepDash (path : string) =
        let size = Vec2.init 256 256
        let textures yi =
            [1; 2; 1; 0]
            |>> fun i ->
                ( path
                , Rect.init (size * (Vec2.init i yi)) size
                , 0.0f)

        {
            sleepWalk = sleepWalk
            sleepDash = sleepDash
            front = textures 0
            frontLeft = textures 1
            left = textures 2
            backLeft = textures 3
            back = textures 4
            backRight = textures 5
            right = textures 6
            frontRight = textures 7
        }

type ObjectViewSetting = {
    name : string
    characterImages : ActorImages<string, int Rect2>
}

type FontSetting = {
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

    buildingTextuers: HashMap<BuildingKind, string * int Rect2>

    damageTextFrame : int
    damageTextMove : float32
    damageView : FontSetting
    healView : FontSetting

    actorGameText : FontSetting
}

module GameViewSetting =
    [<Literal>]
    let modForCulling = 40000.0f

open wraikny.MilleFeuille.UI


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