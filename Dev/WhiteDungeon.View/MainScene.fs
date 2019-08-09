namespace WhiteDungeon.View

open wraikny.Tart.Core
open FSharpPlus


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
open WhiteDungeon.View.MainSceneTEA
open WhiteDungeon.View.Utils.Color


type MainScene(setting : AppSetting) =
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
    let headerFont = createFont 50 ColorPalette.black
    #endif

    let createDynamicFont size color =
        asd.Engine.Graphics.CreateDynamicFont(
            menuSetting.inputFont,
            size, color,
            0, asd.Color()
        )

    let textFont = createDynamicFont 30 ColorPalette.black

    let buttonFont = createDynamicFont 30 ColorPalette.sakura

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
            animationFrame = 20u
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


    let window2Height = float32 setting.windowSize.y * menuSetting.window2HeightRate

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
        Messenger.build { seed = Random().Next() } {
            init = initModel setting.gameSetting, Cmd.none
            update = update
            view = view
        }

    let viewConverter item =
        item |> function
        | TitleText text ->
            UI.TextWith(text, titleFont)
        | HeaderText text ->
            UI.TextWith(text, headerFont)
        | Text text ->
            UI.Text(text)
        | Button(text, msg) ->
            UI.Button(text, fun() -> messenger.Enqueue(msg))
        | WebsiteButton(text, url) ->
            UI.Button(text, fun() ->
                Diagnostics.Process.Start(url) |> ignore
            )
        | InputField(maxLength, placeHolder, current, msg) ->
            UI.InputField(maxLength, placeHolder, current, fun s -> messenger.Enqueue(msg s))
        | Separator ->
            UI.Rect(5.0f, 0.8f)
        | Space x ->
            UI.Space x

    let mutable lastWindow = WindowKind.W1

    let se_page = asd.Engine.Sound.CreateSoundSource("se/page03.ogg", true)

    let callbackAfterClosed (callBack : unit -> unit) =
        lastWindow |> function
        | W1 ->
            uiWindowMain.Toggle(false, fun() ->
                callBack()
            )
        | W2 ->
            sideWindow.Toggle(false)
            uiWindow2.Toggle(false, fun() ->
                callBack()
            )
    do
        messenger.Msg.Add(printfn "Msg: %A")
        messenger.ViewMsg.Add(printfn "Msg: %A")

        messenger.ViewModel.Add(fun viewModel ->
            viewModel |> function
            | Window1 items ->
                uiWindowMain.UIContents <- (map viewConverter items)


                if lastWindow = WindowKind.W2 then
                    callbackAfterClosed(fun () ->
                        // TODO
                        asd.Engine.Sound.Play(se_page) |> ignore
                        uiWindowMain.Toggle(true)
                    )

                lastWindow <- W1
            | Window2 (items1, items2) ->
                sideWindow.UIContents <- (map viewConverter items1)
                uiWindow2.UIContents <- (map viewConverter items2)


                if lastWindow = WindowKind.W1 then
                    callbackAfterClosed(fun() ->
                        // TODO
                        asd.Engine.Sound.Play(se_page) |> ignore
                        sideWindow.Toggle(true)
                        uiWindow2.Toggle(true)
                    )

                lastWindow <- W2
        )

    let bgmPlayer = new Utils.BGMPlayer<_>("BGM", [ setting.menuSceneSetting.bgm ], FadeSeconds = 1.0f)
    do

        base.AddComponent(bgmPlayer, bgmPlayer.Name)
        bgmPlayer.Start()


    override this.OnRegistered() =
        GC.Collect()

        messenger.ViewMsg.Add(function
            | SetBGMVolume v ->
                bgmPlayer.Volume <- v

            | CloseGame ->
                bgmPlayer.FadeOut(0.5f)
                callbackAfterClosed(asd.Engine.Close)

            | StartGame (gameModel, bgmVolume) ->
                callbackAfterClosed(fun() ->
                    let uiFonts : Game.UIArgs = {
                        windowSetting = windowSetting
                        headerFont = headerFont
                        textFont = textFont
                        buttonFont = buttonFont
                        bgmVolume = bgmVolume
                        createMainScene = fun() -> new MainScene(setting) :> asd.Scene
                    }
                    let gameScene = new Game.GameScene(gameModel, setting.gameViewSetting, uiFonts)
                    bgmPlayer.FadeOut(0.5f)
                    this.ChangeSceneWithTransition(gameScene, new asd.TransitionFade(0.5f, 0.5f))
                    |> ignore
                )
                printfn "%A" gameModel
        )

        this.AddLayer(backLayer)
        this.AddLayer(uiLayer)

        backLayer.AddObject(backGround)

        uiLayer.AddMouseButtonSelecter(mouse, "Mouse")
        uiLayer.AddObject(uiWindowMain)
        uiLayer.AddObject(uiWindow2)
        uiLayer.AddObject(sideWindow)

        uiWindowMain.UIContents <- viewConverter <!> titleUI

        // TODO
        asd.Engine.Sound.Play(se_page) |> ignore
        uiWindowMain.Toggle(true)

        messenger.StartAsync()

    override this.OnUpdated() =
        messenger.NotifyView()


    override this.OnDispose() =
        messenger.Dispose()
