namespace WhiteDungeon.View.Title

open wraikny
open wraikny.MilleFeuille.Core.Object
open wraikny.MilleFeuille.Core.UI
open wraikny.MilleFeuille.Fs.Input.Controller

open WhiteDungeon.Core
open WhiteDungeon.View
open WhiteDungeon.View.Utils.Color


[<Class>]
type TitleScene(viewSetting) =
    inherit Scene()

    let viewSetting : ViewSetting = viewSetting

    let backLayer =
        let color = new asd.Color(255uy, 255uy, 255uy)
        new UI.BackGroundLayer(color)

    let uiLayer = new asd.Layer2D()

    let windowSize = asd.Engine.WindowSize.To2DF()

    let titleText =
        let position =
            new asd.Vector2DF(windowSize.X / 2.0f * 0.9f + viewSetting.button1.X, 200.0f)

        let font =
            asd.Engine.Graphics.CreateDynamicFont(
                viewSetting.uiFontPath
                , viewSetting.titleTextFontSize
                , ColorPalette.sumire
                , 0
                , new asd.Color()
            )

        let text = viewSetting.titleText

        let size =
            font
                .CalcTextureSize(text, asd.WritingDirection.Horizontal).To2DF()

        new asd.TextObject2D(
            Font = font
            , Text = text
            , Position = position - size / 2.0f
        )


    let buttonFont =
        let color = asd.Color(255uy, 255uy, 255uy)
        asd.Engine.Graphics.CreateDynamicFont(
            viewSetting.uiFontPath
            , viewSetting.menuButtonFontSize
            , ColorPalette.sumire
            , 0
            , color
        )

    let createButton text position =
        let size = viewSetting.menuButtonSize

        UI.ButtonRectangle.create text buttonFont position size

    let gameButton =
        createButton "Game" viewSetting.button1
            

    let keyboard = UI.Keybaord.createUIKeyboard()


    override this.OnRegistered() =
        this.AddLayer(backLayer)

        this.AddLayer(uiLayer)

        uiLayer.AddObject(titleText)

        uiLayer.AddObject(gameButton)

        let selecter = new Button.ControllerButtonSelecter(gameButton.Button)
        
        selecter.AddController(keyboard) |> ignore
        
        uiLayer.AddComponent(selecter, "Selecter")

        gameButton.Button.add_OnReleasedEvent(fun _ ->
            let createTitleScene = fun vs -> new TitleScene(vs) :> asd.Scene
            this.ChangeScene(new QuickPlay.QuickPlayScene(viewSetting, createTitleScene))
            |> ignore
        )