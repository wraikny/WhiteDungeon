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


[<Class>]
type GameScene(errorHandler : Utils.ErrorHandler,gameModel : Model.Model, gameViewSetting : GameViewSetting, gameSceneArgs : GameSceneArgs) =
    inherit Scene()

    let messenger : IMessenger<_, _, _> =
        Messenger.build
            {
                seed = gameSceneArgs.randomSeed
            }
            {
                init = gameModel, Cmd.port (ViewMsg.UpdateDungeonView(gameModel.dungeonModel, gameModel.dungeonGateCells) )
                view = ViewModel.ViewModel.view
                update = Update.Update.update
            }
    do
        #if DEBUG
        messenger.OnError.Add(Console.WriteLine)
        #endif
        messenger.OnError.Add(fun e ->
            messenger.Enqueue(Msg.SetGameMode <| Model.ErrorUI e)
        )

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

    //let controllers = [|
    //    Game.Model.PlayerID 0u, gameKeybaord
    //|]

    let backLayer =
        let color = new asd.Color(255uy, 255uy, 255uy)
        new UI.BackGroundLayer(color)


    let dungeonLayer = new asd.Layer2D()
    let actorLayer = new asd.Layer2D()
    //let skillEffectsLayer = new asd.Layer2D()
    let uiLayer = new asd.Layer2D()

    let uiBackRect =
        let rect = new asd.RectangleShape(DrawingArea = asd.RectF(asd.Vector2DF(), asd.Engine.WindowSize.To2DF()))
        new asd.GeometryObject2D(Shape = rect, IsUpdated = false)


    do
        // Players
        messenger.ViewModel
            .Select(ViewModel.ViewModel.getPlayers)
            .Subscribe(
                ActorsUpdater<_, _>(actorLayer, {
                    create = fun () -> new PlayerView(gameViewSetting)
                    onError = raise
                    onCompleted = fun () -> printfn "Completed Players Updater"
                }))
        |> ignore

        // Enemies
        messenger.ViewModel
            .Select(ViewModel.ViewModel.getEnemies)
            .Subscribe(
                ActorsUpdater<_, _>(actorLayer, {
                    create = fun () -> new EnemyView(gameViewSetting)
                    onError = raise
                    onCompleted = fun () -> printfn "Completed Enemies Updater"
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
                    ActorsUpdater<_, _>(actorLayer, {
                            create = fun () -> new SkillEmitView(gameViewSetting)
                            onError = raise
                            onCompleted = fun () -> printfn "Completed %A" s
                    }))
        |> ignore

    let dungeonCamera = new GameCamera(true)
    let playerCamera = new GameCamera(false)
    let skillEffectsCamera = new GameCamera(false)

    do
        [|
            dungeonCamera
            playerCamera
            skillEffectsCamera
        |]
        |>> fun o ->
            messenger.ViewModel
                .Select(fun v -> ViewModel.ViewModel.getCameras v)
                .Subscribe o
        |> ignore

    let dungeonCellUpdater = new MaptipsUpdater<_, _>({
            create = fun() -> new DungeonCellView(gameModel.gameSetting.dungeonCellSize, gameViewSetting.dungeonCellTexture)
            onError = raise
            onCompleted = fun () -> printfn "Completed Dungeon MapChips"
        }, UpdatingOption = View.UpdatingOption.Updating)

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

    let gameUIWindows = new GameUI(gameViewSetting, gameSceneArgs)
    do
        messenger.ViewModel.Add(gameUIWindows.OnNext)
    //let gameUIWindows = gameUIWindows :> UI.IToggleWindow


    let uiMouse =
        let mouse = new Input.CollidableMouse(5.0f, ColliderVisible = false)
        new UI.MouseButtonSelecter(mouse)


    let uiWindowMain =
        new UI.MouseWindow(gameSceneArgs.windowSetting, uiMouse,
            Position = asd.Engine.WindowSize.To2DF() / 2.0f
        )

    let bgmPlayer =
        Utils.BGMPlayer<_>("BGM", gameViewSetting.bgms, Volume = gameSceneArgs.bgmVolume, FadeSeconds = 0.5f)
    do
        base.AddComponent(bgmPlayer, bgmPlayer.Name)

    let playSE s =
        asd.Engine.Sound.Play s
        |> fun i -> asd.Engine.Sound.SetVolume(i, gameSceneArgs.bgmVolume)

    let se_button = asd.Engine.Sound.CreateSoundSource("se/button47.ogg", true)
    let se_page = asd.Engine.Sound.CreateSoundSource("se/page03.ogg", true)

    let mutable gameMode = Model.HowToControl
    do
        let gameCursor =
            asd.Engine.CreateCursor("image/Debug/empty1x1.png", asd.Vector2DI())

        messenger.ViewModel.Add(fun vm ->
            if gameMode <> vm.uiMode then
                if gameMode = Model.HowToControl then
                    bgmPlayer.Start()

                asd.Engine.SetCursor <|
                    if vm.uiMode = Model.GameMode then
                        gameCursor
                    else
                        null

                gameMode <- vm.uiMode
        )


    override this.OnRegistered() =
        GC.Collect()

        let convert (item : ViewModel.UIItem) =
            item |> function
            | ViewModel.HeaderText text ->
                UI.TextWith(text, gameSceneArgs.headerFont)
            | ViewModel.Text(text) -> UI.Text(text)
            | ViewModel.Button(text, msg) ->
                UI.Button(text, fun() -> messenger.Enqueue msg)
            | ViewModel.Separator ->
                UI.Rect(3.0f, 0.8f)
            | ViewModel.Space x ->
                UI.Space x
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

                    let scene = gameSceneArgs.createMainScene()

                    uiWindowMain.Toggle(false, fun() ->
                        errorHandler.Clear()
                        bgmPlayer.FadeOut(0.5f)
                        this.ChangeSceneWithTransition(scene, new asd.TransitionFade(0.5f, 0.5f))
                        |> ignore
                    )
                )
            | ViewModel.CloseButton(text) ->
                UI.Button(text, fun() ->
                    // TODO
                    messenger.Dispose()
                    bgmPlayer.FadeOut(0.5f)

                    uiWindowMain.Toggle(false, fun() ->
                        errorHandler.Clear()
                        asd.Engine.Close()
                    )
                )

        messenger.ViewModel
            .Select(fun vm -> vm.mainUIWindow)
            .Add(function
                | None ->
                    if uiWindowMain.IsToggleOn then
                        uiWindowMain.Toggle(false, fun() ->
                            bgmPlayer.Resume()
                            uiBackRect.IsDrawn <- false

                            (gameUIWindows :> UI.IToggleWindow).Toggle(true)
                            GC.Collect()
                        )
                | Some items ->
                    uiWindowMain.UIContents <- map convert items

                    if not uiWindowMain.IsToggleOn then
                        bgmPlayer.Pause()

                        let openUI() =
                            // TODO
                            playSE(se_page)
                            uiBackRect.IsDrawn <- true
                            uiWindowMain.Toggle(true)
                            
                        if (gameUIWindows :> UI.IToggleWindow).IsToggleOn then
                            (gameUIWindows :> UI.IToggleWindow).Toggle(false, openUI)
                        else
                            openUI()

                    GC.Collect()
            )

        // Layer
        this.AddLayer(backLayer)
        this.AddLayer(dungeonLayer)
        this.AddLayer(actorLayer)
        //this.AddLayer(skillEffectsLayer)
        this.AddLayer(uiLayer)

        // Dungeon
        dungeonLayer.AddObject(dungeonCellUpdater)

        // Camera
        dungeonLayer.AddObject(dungeonCamera)
        actorLayer.AddObject(playerCamera)
        //skillEffectsLayer.AddObject(skillEffectsCamera)

        // UI
        uiLayer.AddMouseButtonSelecter(uiMouse, "Mouse")
        uiLayer.AddObject(uiBackRect)
        uiLayer.AddObject(gameUIWindows)
        uiLayer.AddObject(uiWindowMain)

        messenger.StartAsync()

        errorHandler.CallBack <- fun e ->
            messenger.Enqueue(Msg.SetGameMode <| Model.ErrorUI e)


    override this.OnDispose() =
        messenger.Dispose()


    override this.OnUpdated() =
        messenger.NotifyView()

        (  uiWindowMain.IsToggleAnimating
        || gameUIWindows.IsToggleAnimating
        ) |> function
        | true -> ()
        | false ->
            this.PlayerInput()


    member this.PlayerInput() =
        gameMode |> function
        | Model.GameMode ->
            asd.Engine.Mouse.Position <- asd.Engine.WindowSize.To2DF() / 2.0f

            // Pause
            if asd.Engine.Keyboard.GetKeyState(asd.Keys.Escape) = asd.ButtonState.Push then
                messenger.Enqueue(Msg.SetGameMode Model.Pause)
        
            else
                messenger.Enqueue(Msg.TimePasses)
            
                this.PushControllerInput()
                |> iter messenger.Enqueue

                // Input
                let mousePushed =
                    asd.Engine.Mouse.GetButtonInputState
                    >> (=) asd.ButtonState.Push

                [|
                    asd.MouseButtons.ButtonLeft, Model.Actor.Skill1
                    asd.MouseButtons.ButtonRight, Model.Actor.Skill2
                |]
                |> iter(fun (m, s) ->
                    if mousePushed m then
                        messenger.Enqueue(
                            Msg.PlayerSkill(Model.PlayerID 0u, s)
                        )
                )

        | Model.HowToControl
        | Model.Pause
        | Model.GameFinished true ->
            if asd.Engine.Keyboard.GetKeyState(asd.Keys.Escape) = asd.ButtonState.Push then
                messenger.Enqueue(Msg.SetGameMode Model.GameMode)

        | _ -> ()


    member this.PushControllerInput() : Msg.Msg option =
        let getStateIs (state : asd.ButtonState) key =
            gameKeybaord.GetState(key)
            |> Option.ofNullable
            |>> ((=) state)
            |> Option.defaultValue false

        Msg.PlayerInput.inputs
        |> filter (getStateIs asd.ButtonState.Hold)
        |> function
        | [] -> None
        | inputs ->

            (Model.PlayerID 0u, inputs |> Set.ofList)
            |> Msg.PlayerInputs
            |> Some