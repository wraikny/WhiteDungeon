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
            inputFocusColor = createButtonColor sakura 1.0f 1.0f 1.0f

            inputFont = "Font/mplus-1c-light.ttf"
            inputFontSize = 30
            inputFontColor = black

            windowWidthWRate = 0.8f
            window2MarginRate = 0.02f
            window2HeightRate = 0.9f

            bgm = "bgm/seirei_no_machi.ogg"

            #if !DEBUG
            #endif
        }

    gameViewSetting = {
        occupationImages = [
            Model.Seeker, ActorImages.fromGraphicmaker 8u 4u "Image/Game/Occupation/hunter.png"
        ] |> Map.ofList

        bgms = [
            "bgm/gensei_no_rakuen.ogg"
            "bgm/buriki_no_coffee_maker.ogg"
        ]
    }

    gameSetting = {
        dungeonCellSize = Vec2.init 200.0f 200.0f
        minPlayerCount = 1
        maxPlayerCount = 1
        binarySearchCountMovingOnWall = 4
        characterSize = Vec2.init 100.0f 200.0f
        occupationDefaultStatus = [
            Model.Seeker, ({
                Model.ActorStatus.level = 1
                hp = 100.0f
                walkSpeed = 6.0f
                dashSpeed = 12.0f
            } : Model.ActorStatus)
        ] |> Map.ofList
    }
}



[<EntryPoint>]
let main _ =
    try
        let windowSize = appSetting.windowSize |> Vec2.toVector2DI
        asd.Engine.Initialize("WhiteDungeon", windowSize.X, windowSize.Y, new asd.EngineOption(WindowPosition = asd.WindowPositionType.Centering))
        |> function
        | false -> ()
        | true ->
            asd.Engine.File.AddRootDirectory("Resources")

            //let scene = new View.Title.TitleScene(viewSetting)

            let scene = new MainScene.MainScene(appSetting)
            asd.Engine.ChangeScene(scene)

            while asd.Engine.DoEvents() do
                asd.Engine.Update()

            asd.Engine.Terminate()
    with e ->
        System.Console.WriteLine(e)

    0
