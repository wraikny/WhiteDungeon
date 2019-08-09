﻿namespace WhiteDungeon.View.MainScene

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
open wraikny.MilleFeuille.Fs.Component.Coroutine

open WhiteDungeon.Core
open WhiteDungeon.View
open WhiteDungeon.View.MainScene
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
        { UI.WindowSetting.Default(textFont) with
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
            init =
                let initModel = Model.initModel setting.gameSetting
                
                initModel, Cmd.port(Update.SetBGMVolume(Update.bgmToFloat initModel.bgmVolume))
            update = Update.update
            view = ViewModel.view
        }

    let bgmPlayer =
        new Utils.BGMPlayer<_>("BGM", [ setting.menuSceneSetting.bgm ],
            FadeSeconds = 1.0f, Volume = 0.05f)
    do

        base.AddComponent(bgmPlayer, bgmPlayer.Name)
        bgmPlayer.Start()

    let playSE s =
        let id = asd.Engine.Sound.Play(s)
        asd.Engine.Sound.SetVolume(id, bgmPlayer.Volume)

    let se_button = asd.Engine.Sound.CreateSoundSource("se/button47.ogg", true)

    let viewConverter item =
        item |> function
        | ViewModel.TitleText text ->
            UI.TextWith(text, titleFont)
        | ViewModel.HeaderText text ->
            UI.TextWith(text, headerFont)
        | ViewModel.Text text ->
            UI.Text(text)
        | ViewModel.Button(text, msg) ->
            UI.Button(text, fun() ->
                // TODO
                playSE se_button
                messenger.Enqueue(msg))

        | ViewModel.WebsiteButton(text, url) ->
            UI.Button(text, fun() ->
                // TODO
                playSE se_button
                Diagnostics.Process.Start(url) |> ignore
            )
        | ViewModel.InputField(maxLength, placeHolder, current, msg) ->
            UI.InputField(maxLength, placeHolder, current, fun s -> messenger.Enqueue(msg s))
        | ViewModel.Separator ->
            UI.Rect(5.0f, 0.8f)
        | ViewModel.Space x ->
            UI.Space x

    let mutable lastWindow = ViewModel.WindowKind.W1

    let se_page = asd.Engine.Sound.CreateSoundSource("se/page03.ogg", true)

    let callbackAfterClosed (callBack : unit -> unit) =
        lastWindow |> function
        | ViewModel.W1 ->
            uiWindowMain.Toggle(false, fun() ->
                callBack()
            )
        | ViewModel.W2 ->
            sideWindow.Toggle(false)
            uiWindow2.Toggle(false, fun() ->
                callBack()
            )
    do
        messenger.Msg.Add(printfn "Msg: %A")
        messenger.ViewMsg.Add(printfn "ViewMsg: %A")

        messenger.ViewModel.Add(fun viewModel ->
            viewModel |> function
            | ViewModel.Window1 items ->
                uiWindowMain.UIContents <- (map viewConverter items)


                if lastWindow = ViewModel.W2 then
                    callbackAfterClosed(fun () ->
                        // TODO
                        playSE se_page
                        uiWindowMain.Toggle(true)
                    )

                lastWindow <- ViewModel.W1
            | ViewModel.Window2 (items1, items2) ->
                sideWindow.UIContents <- (map viewConverter items1)
                uiWindow2.UIContents <- (map viewConverter items2)


                if lastWindow = ViewModel.W1 then
                    callbackAfterClosed(fun() ->
                        // TODO
                        playSE se_page
                        sideWindow.Toggle(true)
                        uiWindow2.Toggle(true)
                    )

                lastWindow <- ViewModel.W2
        )

    let uiWindowsAnimating() =
        uiWindowMain.IsToggleAnimating
        || uiWindow2.IsToggleAnimating
        || sideWindow.IsToggleAnimating

    override this.OnRegistered() =
        GC.Collect()

        messenger.ViewMsg.Add(function
            | Update.SetBGMVolume v ->
                bgmPlayer.Volume <- v

            | Update.CloseGame ->
                messenger.Dispose()
                this.StartCoroutine("CloseGame", seq {

                    if uiWindowsAnimating() then
                        while uiWindowsAnimating() do
                            yield()
                        yield! Coroutine.sleep 10

                    bgmPlayer.FadeOut(0.5f)
                    callbackAfterClosed(asd.Engine.Close)
                    yield()
                })

            | Update.StartGame (gameModel, randomSeed, bgmVolume) ->
                messenger.Dispose()
                this.StartCoroutine("StartGame", seq {
                    
                    if not uiWindowMain.IsToggleOn then
                        while not uiWindowMain.IsToggleOn do
                            yield()
                        yield! Coroutine.sleep 10

                    callbackAfterClosed(fun() ->
                        let uiFonts : Game.GameSceneArgs = {
                            windowSetting = windowSetting
                            headerFont = headerFont
                            textFont = textFont
                            buttonFont = buttonFont
                            bgmVolume = bgmVolume
                            randomSeed = randomSeed
                            createMainScene = fun() -> new MainScene(setting) :> asd.Scene
                        }
                        let gameScene = new Game.GameScene(gameModel, setting.gameViewSetting, uiFonts)
                        bgmPlayer.FadeOut(0.5f)
                        this.ChangeSceneWithTransition(gameScene, new asd.TransitionFade(0.5f, 0.5f))
                        |> ignore
                    )

                    yield()
                })
        )

        this.AddLayer(backLayer)
        this.AddLayer(uiLayer)

        backLayer.AddObject(backGround)

        uiLayer.AddMouseButtonSelecter(mouse, "Mouse")
        uiLayer.AddObject(uiWindowMain)
        uiLayer.AddObject(uiWindow2)
        uiLayer.AddObject(sideWindow)

        uiWindowMain.UIContents <- viewConverter <!> ViewModel.titleUI

        // TODO
        playSE se_page
        uiWindowMain.Toggle(true)

        messenger.StartAsync()

    override this.OnUpdated() =
        messenger.NotifyView()


    override this.OnDispose() =
        messenger.Dispose()