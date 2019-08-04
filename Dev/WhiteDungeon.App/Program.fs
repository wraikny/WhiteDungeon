module WhiteDungeon.App.Program

open wraikny.Tart.Helper.Math

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
            inputColor = createButtonColor white 1.0f 0.8f 0.6f
            inputFocusColor = createButtonColor black 0.8f 0.8f 0.8f

            inputFont = "Font/mplus-1c-light.ttf"
            inputFontSize = 40
            inputFontColor = black

            windowWidthWRate = 0.8f
            window2MarginRate = 0.02f

            #if !DEBUG
            #endif
        }

    gameViewSetting = {
        occupationImages = [
            Model.Hunter, ({
                front = "Image/Debug/down.png"
                frontRight = "Image/Debug/rightdown.png"
                frontLeft = "Image/Debug/leftdown.png"
                back = "Image/Debug/up.png"
                backRight = "Image/Debug/rightup.png"
                backLeft = "Image/Debug/leftup.png"
                right = "Image/Debug/right.png"
                left = "Image/Debug/left.png"
            } : ActorImages)
        ] |> Map.ofList
    }

    gameSetting = {
        dungeonCellSize = Vec2.init 200.0f 200.0f
        minPlayerCount = 1
        maxPlayerCount = 1
        binarySearchCountMovingOnWall = 4
        characterSize = Vec2.init 100.0f 100.0f
        occupationDefaultStatus = [
            Model.Hunter, ({
                level = 1
                hp = 100.0f
                walkSpeed = 10.0f
                dashSpeed = 20.0f
            } : Model.ActorStatus)
        ] |> Map.ofList
    }
}



[<EntryPoint>]
let main _ =
    //let viewSetting : View.ViewSetting = {
    //    uiFontPath = "Font/mplus-1c-light.ttf"
    //    menuButtonSize = new asd.Vector2DF(400.0f, 100.0f)
    //    menuButtonFontSize = 70

    //    messageFontSize = 70

    //    titleText = "White Dungeon"
    //    titleTextFontSize = 150

    //    button1 = new asd.Vector2DF(250.0f, 400.0f)
    //    button2 = new asd.Vector2DF(250.0f, 550.0f)
    //    button3 = new asd.Vector2DF(250.0f, 700.0f)
    //}

    let windowSize = appSetting.windowSize |> Vec2.toVector2DI
    asd.Engine.Initialize("WhiteDungeon", windowSize.X, windowSize.Y, new asd.EngineOption())
    |> ignore

    asd.Engine.File.AddRootDirectory("Resources")

    //let scene = new View.Title.TitleScene(viewSetting)

    let scene = new MenuScene(appSetting)

    asd.Engine.ChangeScene(scene)

    let rec loop() =
        if asd.Engine.DoEvents() then
            asd.Engine.Update()
            loop()

    loop()

    asd.Engine.Terminate()
    0
