namespace WhiteDungeon.View

open wraikny.Tart.Core
open FSharpPlus


module MenuScene =
    type 'Msg MenuItem =
        | TitleText of string
        | HeaderText of string
        | Text of string
        | Button of string * 'Msg
        | WebsiteButton of string * string
        | InputField of int * string * (string -> 'Msg)
        | Separator
        | Space of float32


    type 'Msg ViewModel =
        | Window1 of 'Msg MenuItem list
        | Window2 of 'Msg MenuItem list * 'Msg MenuItem list


    type CreditMode =
        | CreditProject
        | CreditLibs
        | CreditBGM


    type UIMode =
        | Title
        | CharacterSelect
        | Credit of CreditMode


    type Msg =
        | SetUI of UIMode


    type ViewMsg = unit
    
    
    type Model = {
        uiMode : UIMode
    }

    let initModel = {
        uiMode = Title
    }


    let setUI ui model = { model with uiMode = ui }


    let update (msg : Msg) (model : Model) : Model * Cmd<Msg, ViewMsg> =
        msg |> function
        | SetUI uiMode ->
            model |> setUI uiMode, Cmd.none
    

    let titleUI = [
            Space 50.0f
            TitleText "九十九のラビリンス"
            Text "C96体験版"
            Text "Lepus Pluvia"
            Separator
            Button("始める", SetUI CharacterSelect)
            Button("クレジット", SetUI <| Credit CreditProject)
            Separator
            Space 50.0f
        ]

    let creditUISide = [
            Button("もどる", SetUI Title)
            Button("制作", SetUI <| Credit CreditProject)
            Button("ライブラリ", SetUI <| Credit CreditLibs)
            Button("BGM", SetUI <| Credit CreditBGM)
        ]

    let creditUIProj = [
        HeaderText "制作"
        Separator
        Text("Lepus Pluvia")
        WebsiteButton("Website", "http://LepusPluvia.com")
        WebsiteButton("Twitter", "http://twitter.com/LepusPluvia")
        Separator
    ]

    let creditUILibs = [
        HeaderText "ライブラリ"
        Separator
        WebsiteButton("FSharp.Core", "https://github.com/dotnet/fsharp")
        WebsiteButton("FSharpPlus", "https://github.com/fsprojects/FSharpPlus")
        WebsiteButton("Altseed", "http://altseed.github.io/")
        WebsiteButton("Tart", "https://github.com/wraikny/Tart")
        WebsiteButton("Mille Feuille", "https://github.com/wraikny/Mille-Feuille")
        Separator
    ]

    let creditUIBGM = [
        HeaderText "BGM"
        Separator
        Text "てすと"
        Text "あああああ"
        Text "あああああ"
        Separator
    ]

    // https://twitter.com/intent/tweet?text=「九十九のラビリンス C96体験版」をプレイしました！ @LepusPluvia


    let view (model : Model) : Msg ViewModel =
        model.uiMode |> function
        | Title ->
            Window1 titleUI
        | Credit CreditProject ->
            Window2(creditUISide, creditUIProj)
        | Credit CreditLibs ->
            Window2(creditUISide, creditUILibs)
        | Credit CreditBGM ->
            Window2(creditUISide, creditUIBGM)
        | _ ->
            Window1 []

    type WindowKind =
        | W1
        | W2



open System

open wraikny.Tart.Helper
open wraikny.Tart.Helper.Math

open wraikny.MilleFeuille.Core
open wraikny.MilleFeuille.Core.UI
open wraikny.MilleFeuille.Fs.Input
open wraikny.MilleFeuille.Fs.Math
open wraikny.MilleFeuille.Fs

open WhiteDungeon.Core
open WhiteDungeon.View
open WhiteDungeon.View.Utils.Color


type MenuScene(setting : AppSetting) =
    inherit Scene()

    let menuSetting = setting.menuSceneSetting

    let backLayer = new asd.Layer2D()

    let uiLayer = new asd.Layer2D()

    let ws = asd.Engine.WindowSize.To2DF()

    let backGround =
        let area = asd.RectF(asd.Vector2DF(0.0f, 0.0f), ws)
        let shape = new asd.RectangleShape(DrawingArea = area)
        new asd.GeometryObject2D(
            Shape = shape,
            Color = Vec3.toColor menuSetting.backColor
        )

    #if DEBUG
    let createFont size color =
        asd.Engine.Graphics.CreateDynamicFont(
            "C:\Windows\Fonts\UtsukushiFONT.otf",
            size, color, 0, asd.Color()
        )

    let titleFont = createFont 80 ColorPalette.black
    let headerFont = createFont 60 ColorPalette.black
    let textFont = createFont 40 ColorPalette.black

    let buttonFont = createFont 40 ColorPalette.sakura
    #endif

    let inputFont =
        asd.Engine.Graphics.CreateDynamicFont(
            menuSetting.inputFont,
            menuSetting.inputFontSize, Vec3.toColor menuSetting.inputFontColor,
            0, asd.Color()
        )

    let windowSize = setting.windowSize |>> float32

    let mainWindowWidth = windowSize.x * menuSetting.windowWidthWRate

    let windowSetting =
        { UI.WindowSetting.Default(null) with
            itemMargin = 20.0f
            itemAlignment = UI.WindowSetting.Center

            textFont = textFont
            buttonFont = buttonFont
            inputFont = inputFont

            frameColor = Vec4.toColor menuSetting.frameColor
            button = menuSetting.buttonColor
            inputColor = menuSetting.inputColor
            inputFocusColor = menuSetting.inputFocusColor

            rectColor = ColorPalette.sumire

            windowSize = UI.WindowSetting.WindowSize.FixWidth mainWindowWidth
            toggleDirection = UI.WindowSetting.ToggleDirection.Center(UI.WindowSetting.Y)
            buttonSize = UI.WindowSetting.ButtonSize.AutoFit(asd.Vector2DF(10.0f, 10.0f), 0.8f)
        }


    let mouse =
        let mouse = new Input.CollidableMouse(5.0f, ColliderVisible = true)
        new UI.MouseButtonSelecter(mouse)


    let uiWindowMain =
        new UI.MouseWindow(windowSetting, mouse,
            Position = Vec2.toVector2DF (setting.windowSize |>> float32 |> ( *. ) 0.5f )
        )


    let window2Height = float32 setting.windowSize.y * 0.8f

    let window2Margin = windowSize.x * menuSetting.window2MarginRate / 2.0f
    let window2Width a = windowSize.x * (menuSetting.windowWidthWRate * a / 3.0f) - window2Margin

    let uiWindow2 =
        let width = window2Width 2.0f
        let win2Setting =
            { windowSetting with
                itemMargin = 15.0f
                windowSize = UI.WindowSetting.WindowSize.Fixed(asd.Vector2DF(width, window2Height))
                toggleDirection = UI.WindowSetting.ToggleDirection.From(UI.WindowSetting.Direction.Left)
            }

        new UI.MouseWindow(win2Setting, mouse,
            Position = asd.Vector2DF(
                windowSize.x + mainWindowWidth - width * 2.0f,
                windowSize.y
            ) / 2.0f
        )

    let sideWindow =
        let width = window2Width 1.0f
        let sideSetting =
            { windowSetting with
                itemMargin = 30.0f
                windowSize = UI.WindowSetting.WindowSize.Fixed(asd.Vector2DF(width, window2Height))
                toggleDirection = UI.WindowSetting.ToggleDirection.From(UI.WindowSetting.Direction.Right)
            }

        new UI.MouseWindow(sideSetting, mouse,
            Position = asd.Vector2DF(
                windowSize.x - mainWindowWidth + width * 2.0f,
                windowSize.y
            ) / 2.0f
        )

    let messenger =
        Messenger.build { seed = 0 } {
            init = MenuScene.initModel, Cmd.none
            update = MenuScene.update
            view = MenuScene.view
        }

    let viewConverter item =
        item |> function
        | MenuScene.TitleText text ->
            UI.TextWith(text, titleFont)
        | MenuScene.HeaderText text ->
            UI.TextWith(text, headerFont)
        | MenuScene.Text text ->
            UI.Text(text)
        | MenuScene.Button(text, msg) ->
            UI.Button(text, fun() -> messenger.Enqueue(msg))
        | MenuScene.WebsiteButton(text, url) ->
            UI.Button(text, fun() ->
                Diagnostics.Process.Start(url) |> ignore
            )
        | MenuScene.InputField(maxLength, placeHolder, msg) ->
            UI.InputField(maxLength, placeHolder, fun s -> messenger.Enqueue(msg s))
        | MenuScene.Separator ->
            UI.Rect(5.0f, 0.8f)
        | MenuScene.Space x ->
            UI.Space x
    do
        messenger.Msg.Add(printfn "Msg: %A")
        messenger.ViewMsg.Add(printfn "Msg: %A")

        let mutable lastWindow = MenuScene.WindowKind.W1

        messenger.ViewModel.Add(fun viewModel ->
            viewModel |> function
            | MenuScene.Window1 items ->
                uiWindowMain.UIContents <- (map viewConverter items)

                if lastWindow = MenuScene.WindowKind.W2 then
                    lastWindow <- MenuScene.W1

                    sideWindow.Toggle(false)
                    uiWindow2.Toggle(false, fun () ->
                        uiWindowMain.Toggle(true)
                    )
            | MenuScene.Window2 (items1, items2) ->
                sideWindow.UIContents <- (map viewConverter items1)
                uiWindow2.UIContents <- (map viewConverter items2)

                if lastWindow = MenuScene.WindowKind.W1 then
                    lastWindow <- MenuScene.W2

                    uiWindowMain.Toggle(false, fun() ->
                        sideWindow.Toggle(true)
                        uiWindow2.Toggle(true)
                    )
        )


    override this.OnRegistered() =
        this.AddLayer(backLayer)
        this.AddLayer(uiLayer)

        backLayer.AddObject(backGround)

        uiLayer.AddMouseButtonSelecter(mouse, "Mouse")
        uiLayer.AddObject(uiWindowMain)
        uiLayer.AddObject(uiWindow2)
        uiLayer.AddObject(sideWindow)

        uiWindowMain.UIContents <- viewConverter <!> MenuScene.titleUI
        uiWindowMain.Toggle(true)

        messenger.StartAsync()

    override this.OnUpdated() =
        messenger.NotifyView()


    override this.OnDispose() =
        messenger.Dispose()
