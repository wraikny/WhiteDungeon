module WhiteDungeon.App.Program

open wraikny.Tart.Helper.Math
open WhiteDungeon

open WhiteDungeon.Core.Game.Model.Skill


[<EntryPoint>]
let main _ =
    let viewSetting : View.ViewSetting = {
        uiFontPath = "Font/mplus-1c-light.ttf"
        menuButtonSize = new asd.Vector2DF(400.0f, 100.0f)
        menuButtonFontSize = 70

        messageFontSize = 70

        titleText = "White Dungeon"
        titleTextFontSize = 150

        button1 = new asd.Vector2DF(250.0f, 400.0f)
        button2 = new asd.Vector2DF(250.0f, 550.0f)
        button3 = new asd.Vector2DF(250.0f, 700.0f)
    }

    let windowSize = new asd.Vector2DI(16, 9) * 100
    asd.Engine.Initialize("WhiteDungeon", windowSize.X, windowSize.Y, new asd.EngineOption())
    |> ignore

    asd.Engine.File.AddRootDirectory("Resources")

    let scene = new View.Title.TitleScene(viewSetting)

    asd.Engine.ChangeScene(scene)

    let rec loop() =
        if asd.Engine.DoEvents() then
            asd.Engine.Update()
            loop()

    loop()

    asd.Engine.Terminate()
    0
