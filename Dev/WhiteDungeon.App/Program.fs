module WhiteDungeon.App.Program

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry

open wraikny.MilleFeuille.Fs.Math
open wraikny.MilleFeuille.Fs.UI
open WhiteDungeon
open WhiteDungeon.Core
open WhiteDungeon.Core.Game
open WhiteDungeon.View
open WhiteDungeon.View.Utils.Color

open WhiteDungeon.Core.Game.Model.Skill

open FSharpPlus

let black = Vec3.init 0uy 0uy 0uy
let white = Vec3.init 255uy 255uy 255uy

let ume = Vec3.init 234uy 173uy 189uy

let sumire = Vec3.init 85uy 69uy 98uy

let sakura = Vec3.init 250uy 219uy 224uy


let appSetting : View.AppSetting = {
    windowSize = (Vec2.init 16 9) .* 75

    menuSceneSetting =
        let createButtonColor col x y z =
            let c a = col |>> float32 |>> ( * ) a |>> byte |> Vec3.toColor
            {
                defaultColor = c x
                hoverColor = c y
                holdColor = c z
            }
            
        {
            backColor = white
            frameColor = Vec4.init ume.x ume.y ume.z 200uy
            buttonColor = createButtonColor sumire 1.0f 0.8f 0.6f
            inputColor = createButtonColor sakura 1.0f 0.8f 0.6f
            inputFocusColor = createButtonColor sakura 0.8f 0.8f 0.8f

            inputFont = "Font/mplus-1c-light.ttf"
            inputFontSize = 30
            inputFontColor = black

            windowWidthWRate = 0.8f
            window2MarginRate = 0.02f
            window2HeightRate = 0.9f

            #if !DEBUG
            #endif
        }

    gameViewSetting = {
        occupationImages = [
        #if DEBUG
            Model.DebugOccupation, ({
                sleepWalk = 10u
                sleepDash = 5u
                front = [
                    "Image/Debug/down.png", Rect.init zero (Vec2.init 128 128), 0.0f
                ]
                frontRight = [
                    "Image/Debug/rightdown.png", Rect.init zero (Vec2.init 128 128), 0.0f
                ]
                frontLeft = [
                    "Image/Debug/leftdown.png", Rect.init zero (Vec2.init 128 128), 0.0f
                ]
                back = [
                    "Image/Debug/up.png", Rect.init zero (Vec2.init 128 128), 0.0f
                ]
                backRight = [
                    "Image/Debug/rightup.png", Rect.init zero (Vec2.init 128 128), 0.0f
                ]
                backLeft = [
                    "Image/Debug/leftup.png", Rect.init zero (Vec2.init 128 128), 0.0f
                ]
                right = [
                    "Image/Debug/right.png", Rect.init zero (Vec2.init 128 128), 0.0f
                ]
                left = [
                    "Image/Debug/left.png", Rect.init zero (Vec2.init 128 128), 0.0f
                ]
            } : ActorImages<_, _>)
        #endif
            Model.Hunter, ActorImages.fromGraphicmaker 9u 6u "Image/Game/Occupation/hunter.png"
        ] |> Map.ofList
    }

    gameSetting = {
        dungeonCellSize = Vec2.init 200.0f 200.0f
        minPlayerCount = 1
        maxPlayerCount = 1
        binarySearchCountMovingOnWall = 4
        characterSize = Vec2.init 100.0f 200.0f
        occupationDefaultStatus = [
        #if DEBUG
            Model.DebugOccupation, ({
                level = 1
                hp = 100.0f
                walkSpeed = 10.0f
                dashSpeed = 15.0f
            } : Model.ActorStatus)
        #endif
            Model.Hunter, ({
                Model.ActorStatus.level = 1
                hp = 100.0f
                walkSpeed = 8.0f
                dashSpeed = 12.0f
            } : Model.ActorStatus)
        ] |> Map.ofList
    }
}



[<EntryPoint>]
let main _ =
    let windowSize = appSetting.windowSize |> Vec2.toVector2DI
    asd.Engine.Initialize("WhiteDungeon", windowSize.X, windowSize.Y, new asd.EngineOption())
    |> ignore

    asd.Engine.File.AddRootDirectory("Resources")

    //let scene = new View.Title.TitleScene(viewSetting)

    let scene = new MainScene(appSetting)

    asd.Engine.ChangeScene(scene)

    let rec loop() =
        if asd.Engine.DoEvents() then
            asd.Engine.Update()
            loop()

    loop()

    asd.Engine.Terminate()
    0
