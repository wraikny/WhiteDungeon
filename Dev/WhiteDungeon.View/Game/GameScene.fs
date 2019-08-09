namespace WhiteDungeon.View.Game


open wraikny.Tart.Helper.Utils
open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry
open wraikny.Tart.Core
open wraikny.Tart.Advanced
open wraikny.MilleFeuille.Fs.Objects
open wraikny.MilleFeuille.Fs.Input
open wraikny.MilleFeuille.Core
open wraikny.MilleFeuille.Core.Input

open WhiteDungeon.Core
open WhiteDungeon.Core.Game
open WhiteDungeon.View
open WhiteDungeon.View.Utils.Color

open wraikny.Tart.Helper.Collections
open wraikny.Tart.Helper.Extension
open wraikny.MilleFeuille.Fs
open wraikny.MilleFeuille.Fs.Objects
open wraikny.MilleFeuille.Fs.Math
open wraikny.MilleFeuille.Fs.Geometry

open FSharpPlus

open System
open System.Collections.Generic
open System.Linq
open System.Reactive
open System.Reactive.Linq


type UIArgs = {
    windowSetting : UI.WindowSetting
    headerFont : asd.Font
    textFont : asd.Font
    buttonFont : asd.Font

    bgmVolume : float32

    createMainScene : unit -> asd.Scene
}


[<Class>]
type GameScene(gameModel : Model.Model, gameViewSetting : GameViewSetting, uiArgs : UIArgs) =
    inherit Scene()

    let messenger : IMessenger<_, _, _> =
        Messenger.build
            { seed = 0 }
            {
                init = gameModel, Cmd.port (ViewMsg.UpdateDungeonView(gameModel.dungeonModel, gameModel.dungeonGateCells) )
                view = ViewModel.ViewModel.view
                update = Update.Update.update
            }

    let port _ = ()
    do
        messenger.ViewMsg.Subscribe(port) |> ignore

    let gameKeybaord =
        KeyboardBuilder.init()
        |> KeyboardBuilder.bindKeysList
            [
                Game.Msg.UpKey    , asd.Keys.W
                Game.Msg.DownKey  , asd.Keys.S
                Game.Msg.RightKey , asd.Keys.D
                Game.Msg.LeftKey  , asd.Keys.A
                Game.Msg.DashKey  , asd.Keys.LeftShift
            ]
        |> KeyboardBuilder.build
        :> IController<Game.Msg.PlayerInput>

    let controllers = [|
        Game.Model.PlayerID 0u, gameKeybaord
    |]

    let backLayer =
        let color = new asd.Color(255uy, 255uy, 255uy)
        new UI.BackGroundLayer(color)


    let dungeonLayer = new asd.Layer2D()
    let playerLayer = new asd.Layer2D()
    let skillEffectsLayer = new asd.Layer2D()
    let uiLayer = new asd.Layer2D()

    do
        // Players
        messenger.ViewModel
            .Select(fun v -> ViewModel.ViewModel.getPlayers v)
            .Subscribe(
                ActorsUpdater<_, _>(playerLayer, {
                    create = fun () -> new PlayerView(gameViewSetting)
                    onError = raise
                    onCompleted = fun () -> printfn "Completed PlayersUpdater"
                }))
        |> ignore

        // SkillEffects
        [
            ViewModel.ViewModel.getSkillAreaPlayer
            ViewModel.ViewModel.getSkillAreaEnemy
            ViewModel.ViewModel.getSkillAreaAll
        ]
        |>> fun s ->
            messenger.ViewModel
                .Select(fun v -> s v)
                .Subscribe(
                    ActorsUpdater<_, _>(skillEffectsLayer, {
                            create = fun () -> new SkillEmitView(gameViewSetting)
                            onError = raise
                            onCompleted = fun () -> printfn "Completed %A" s
                    }))
        |> ignore

    let dungeonCamera = new GameCamera()
    let playerCamera = new GameCamera()
    let skillEffectsCamera = new GameCamera()
    //let minimapCamera =
    //    new GameCamera(
    //        Zoom = 3.0f
    //        , Dst =(
    //            let windowSize = asd.Engine.WindowSize
    //            let size = windowSize / 8
    //            let pos = new asd.Vector2DI(windowSize.X - 50, 50)
    //            new asd.RectI(
    //                pos - (new asd.Vector2DI(size.X, 0))
    //                , size
    //            )
    //        )
    //    )
    do
        [
            dungeonCamera
            playerCamera
            skillEffectsCamera
        ]
        |>> fun o ->
            messenger.ViewModel
                .Select(fun v -> ViewModel.ViewModel.getCameras v)
                .Subscribe o
        |> ignore

    let dungeonCellUpdater = new MaptipsUpdater<_, _>({
            create = fun() -> new DungeonCellView(gameModel.gameSetting.dungeonCellSize)
            onError = raise
            onCompleted = fun () -> printfn "Completed Dungeon MapChips"
        })

    do
        messenger.ViewMsg
            .Add(function
                | ViewMsg.UpdateDungeonView (dungeonModel, gateCells) ->
                    let cellsDict = 
                        dungeonModel.cells
                        |> HashMap.toSeq
                        |>> fun (pos, id) -> (pos, DungeonCellKind.fromSpaceID id)
                        |> fun x -> Dictionary<_, _>(dict x)

                    for pos in gateCells do
                        cellsDict.[pos] <- DungeonCellKind.Gate

                    seq {
                        let mutable i = 0u
                        for KeyValue(p, k) in cellsDict do
                            yield (i, (p, k))
                            i <- i + 1u
                    }
                    |> toList
                    |> (dungeonCellUpdater :> IObserver<_>).OnNext
                    GC.Collect()
                //| _ -> ()
            )


    let uiMouse =
        let mouse = new Input.CollidableMouse(5.0f, ColliderVisible = true)
        new UI.MouseButtonSelecter(mouse)


    let uiWindowMain =
        new UI.MouseWindow(uiArgs.windowSetting, uiMouse,
            Position = asd.Engine.WindowSize.To2DF() / 2.0f
        )

    let bgmPlayer =
        new Utils.BGMPlayer<_>("BGM", gameViewSetting.bgms, Volume = uiArgs.bgmVolume, FadeSeconds = 0.5f)
    do
        base.AddComponent(bgmPlayer, bgmPlayer.Name)
        bgmPlayer.Start()

    let playSE s =
        asd.Engine.Sound.Play s
        |> fun i -> asd.Engine.Sound.SetVolume(i, uiArgs.bgmVolume)

    let se_button = asd.Engine.Sound.CreateSoundSource("se/button47.ogg", true)
    let se_page = asd.Engine.Sound.CreateSoundSource("se/page03.ogg", true)
    //let se_metal = asd.Engine.Sound.CreateSoundSource("se/metal03.ogg", true)

    let mutable gameMode = Model.HowToControl
    do
        messenger.ViewModel.Add(fun vm ->
            if gameMode <> vm.uiMode then
                gameMode <- vm.uiMode

                vm.uiMode |> function
                | Model.Pause ->
                    //asd.Engine.Sound.Play(se_metal) |> ignore
                    ()
                | _ -> ()
        )


    override this.OnRegistered() =
        GC.Collect()

        let convert (item : ViewModel.UIItem) =
            item |> function
            | ViewModel.HeaderText text ->
                UI.TextWith(text, uiArgs.headerFont)
            | ViewModel.Text(text) -> UI.Text(text)
            | ViewModel.Button(text, msg) ->
                UI.Button(text, fun() -> messenger.Enqueue msg)
            | ViewModel.Separator ->
                UI.Rect(3.0f, 0.8f)
            | ViewModel.URLButton(text, url) ->
                UI.Button(text, fun() ->
                    // TODO
                    playSE(se_button)

                    Diagnostics.Process.Start(url) |> ignore
                )
            | ViewModel.TitleButton(text) ->
                UI.Button(text, fun() ->
                    // TODO
                    playSE(se_button)

                    messenger.Dispose()

                    let scene = uiArgs.createMainScene()

                    uiWindowMain.Toggle(false, fun() ->
                        bgmPlayer.FadeOut(0.5f)
                        this.ChangeSceneWithTransition(scene, new asd.TransitionFade(0.5f, 0.5f))
                        |> ignore
                    )
                )
            | ViewModel.CloseButton(text) ->
                UI.Button(text, fun() ->
                    // TODO
                    playSE(se_button)

                    messenger.Dispose()
                    bgmPlayer.FadeOut(0.5f)
                    uiWindowMain.Toggle(false, fun() ->
                        asd.Engine.Close()
                    )
                )

        messenger.ViewModel.Select(fun vm -> vm.mainUIWindow)
            .Add(function
                | None ->
                    if uiWindowMain.IsToggleOn then
                        uiWindowMain.Toggle(false, fun() ->
                            bgmPlayer.Resume()
                        )
                | Some items ->
                    uiWindowMain.UIContents <- map convert items

                    if not uiWindowMain.IsToggleOn then
                        bgmPlayer.Pause()

                        // TODO
                        playSE(se_page)
                        uiWindowMain.Toggle(true)
            )


        // Layer
        this.AddLayer(backLayer)
        this.AddLayer(dungeonLayer)
        this.AddLayer(playerLayer)
        this.AddLayer(skillEffectsLayer)
        this.AddLayer(uiLayer)

        // Dungeon
        dungeonLayer.AddObject(dungeonCellUpdater)
        //this.AddDungeonView(gameModel)

        // Camera
        dungeonLayer.AddObject(dungeonCamera)
        playerLayer.AddObject(playerCamera)
        // dungeonLayer.AddObject(minimapCamera)
        skillEffectsLayer.AddObject(skillEffectsCamera)

        // UI
        uiLayer.AddMouseButtonSelecter(uiMouse, "Mouse")
        uiLayer.AddObject(uiWindowMain)

        messenger.StartAsync() |> ignore


    override this.OnDispose() =
        messenger.Dispose()


    override this.OnUpdated() =
        messenger.NotifyView()

        gameMode |> function
        | Model.GameMode ->
            // Mouse inside window
            let mousePos = asd.Engine.Mouse.Position |> Vec2.fromVector2DF
            let ws = asd.Engine.WindowSize.To2DF() |> Vec2.fromVector2DF
            let margin = 0.025f
            if Rect.isInside mousePos (Rect.init zero ws) |> not then
                let mousePosX = asd.MathHelper.Clamp(mousePos.x, ws.x * (1.0f - margin * 2.0f), ws.x * margin)
                let mousePosY = asd.MathHelper.Clamp(mousePos.y, ws.y * (1.0f - margin * 2.0f), ws.y * margin)
                asd.Engine.Mouse.Position <- asd.Vector2DF(mousePosX, mousePosY)


            // Pause
            if asd.Engine.Keyboard.GetKeyState(asd.Keys.Escape) = asd.ButtonState.Push then
                messenger.Enqueue(Msg.SetGameMode Model.Pause)
            
            else
                messenger.Enqueue(Msg.TimePasses)
                
                // Input
                let inline mouseLeftPushed() =
                    asd.Engine.Mouse.GetButtonInputState(asd.MouseButtons.ButtonLeft) = asd.ButtonState.Push

                this.PushControllerInput() |> function
                | [||] -> ()
                | msgs ->
                    //messenger.Enqueue(Msg.TimePasses)
                    msgs |> iter messenger.Enqueue

                if mouseLeftPushed() then
                    messenger.Enqueue(Msg.AppendSkillEmits)
        | Model.HowToControl
        | Model.Pause
        | Model.GameFinished true ->
            if asd.Engine.Keyboard.GetKeyState(asd.Keys.Escape) = asd.ButtonState.Push then
                messenger.Enqueue(Msg.SetGameMode Model.GameMode)

        | _ -> ()


    member this.PushControllerInput() : Msg.Msg [] =
        controllers
        |> filterMap(fun (id, controller) ->
            let getStateIs (state : asd.ButtonState) key =
                controller.GetState(key)
                |> Option.ofNullable
                |>> ((=) state)
                |> Option.defaultValue false

            Msg.PlayerInput.inputs
            |> filter (getStateIs asd.ButtonState.Hold)
            |> function
            | [] -> None
            | inputs ->
                Some <| Msg.PlayerInputs (id, inputs |> Set.ofList)
        )