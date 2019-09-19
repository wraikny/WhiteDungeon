namespace WhiteDungeon.View.Game


open wraikny.Tart.Helper
open wraikny.Tart.Math
open wraikny.Tart.Core
open wraikny.Tart.Advanced
open wraikny.MilleFeuille.Objects
open wraikny.MilleFeuille.Input
open wraikny.MilleFeuille
open wraikny.MilleFeuille.Input

open WhiteDungeon.Core
open WhiteDungeon.View
open WhiteDungeon.View.Utils.Color

open wraikny.Tart.Helper.Collections
open wraikny.Tart.Helper.Extension
open wraikny.MilleFeuille
open wraikny.MilleFeuille.Objects

open FSharpPlus

open System
open System.Collections.Generic
open System.Linq
open System.Reactive
open System.Reactive.Linq


[<Class>]
type GameScene(errorHandler : Utils.ErrorHandler,gameModel : Model.Model, gameViewSetting : GameViewSetting, gameSceneArgs : GameSceneArgs) =
    inherit Scene()

    let messenger =
        Messenger.Create(
            {
                seed = gameSceneArgs.randomSeed
            },
            {
                init = gameModel, Cmd.ofPort (ViewMsg.UpdateDungeonView(gameModel.dungeonModel, gameModel.dungeonGateCells) )
                view = ViewModel.ViewModel.view
                update = Update.Update.update
            }
        )
    do
        //messenger.SleepTime <- 0u

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
                Select            , asd.Keys.Space
                Cancel            , asd.Keys.Escape
                Dash              , asd.Keys.LeftShift
                Direction Up      , asd.Keys.W
                Direction Down    , asd.Keys.S
                Direction Right   , asd.Keys.D
                Direction Left    , asd.Keys.A
                Skill Model.Skill1, asd.Keys.J
                Skill Model.Skill2, asd.Keys.K
            ]
        |> KeyboardBuilder.build
        :> IController<PlayerInput>

    //let controllers = [|
    //    Game.Model.PlayerID 0u, gameKeybaord
    //|]

    let backLayer =
        let color = new asd.Color(255uy, 255uy, 255uy)
        new UI.BackGroundLayer(color)


    let dungeonLayer = new asd.Layer2D()
    let actorLayer = new asd.Layer2D()
    let hpLayer = new asd.Layer2D()

    let pauseLayers = [
        dungeonLayer
        actorLayer
        hpLayer
    ]


    //let skillEffectsLayer = new asd.Layer2D()
    let uiLayer = new asd.Layer2D()

    let uiBackRect =
        let rect = new asd.RectangleShape(DrawingArea = asd.RectF(asd.Vector2DF(), asd.Engine.WindowSize.To2DF()))
        new asd.GeometryObject2D(Shape = rect, IsUpdated = false)


    do
        let playersImagesMap =
            gameViewSetting.occupationSetting
            |> HashMap.map (fun _ x -> x.characterImages)
        // Players
        messenger.ViewModel
            .Select(ViewModel.ViewModel.getPlayers)
            .Subscribe(
                ActorsUpdater<_, _>(actorLayer, {
                    create = fun () -> new PlayerView(gameViewSetting, playersImagesMap, hpLayer)
                    onError = raise
                    onCompleted = fun () -> printfn "Completed Players Updater"
                }))
        |> ignore

        // Enemies
        messenger.ViewModel
            .Select(ViewModel.ViewModel.getEnemies)
            .Subscribe(
                ActorsUpdater<_, _>(actorLayer, {
                    create = fun () -> new EnemyView(gameModel.gameSetting, gameViewSetting, hpLayer)
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

    let damagesView = new DamagesView(gameViewSetting)
    do
        messenger.ViewModel
            .Select(fun v -> v.camera.position)
            .Add(damagesView.OnNext)

        hpLayer.AddComponent(damagesView, "DamagesView")

    let dungeonCamera = new GameCamera(true)
    let actorCamera = new GameCamera(false)
    let hpCamera = new GameCamera(false)
    let skillEffectsCamera = new GameCamera(false)

    do
        [|
            dungeonCamera
            actorCamera
            hpCamera
            skillEffectsCamera
        |]
        |>> fun o ->
            messenger.ViewModel
                .Select(fun v -> v.camera)
                .Subscribe o
        |> ignore

    let dungeonCellUpdater = new MaptipsUpdater<_, _>({
            create = fun() -> new DungeonCellView(gameModel.gameSetting.dungeonCellSize, gameViewSetting.dungeonCellTexture)
            onError = raise
            onCompleted = fun () -> printfn "Completed Dungeon MapChips"
        }, UpdatingOption = UpdatingOption.Updating)

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
                | ViewMsg.DamagesView x -> damagesView.Add(x)
            )

    let gameUIWindows = new GameUI(gameViewSetting, gameModel.gameSetting, gameSceneArgs)
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

                            pauseLayers
                            |> Seq.iter(fun x -> x.IsUpdated <- true)
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
                            pauseLayers
                            |> Seq.iter(fun x -> x.IsUpdated <- false)
                            
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
        this.AddLayer(hpLayer)
        //this.AddLayer(skillEffectsLayer)
        this.AddLayer(uiLayer)

        // Dungeon
        dungeonLayer.AddObject(dungeonCellUpdater)

        // Camera
        dungeonLayer.AddObject(dungeonCamera)
        actorLayer.AddObject(actorCamera)
        hpLayer.AddObject(hpCamera)

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

                //// Input
                //let mousePushed =
                //    asd.Engine.Mouse.GetButtonInputState
                //    >> (=) asd.ButtonState.Push

                //[|
                //    asd.MouseButtons.ButtonLeft, Model.Skill1
                //    asd.MouseButtons.ButtonRight, Model.Skill2
                //|]
                //|> iter(fun (m, s) ->
                //    if mousePushed m then
                //        messenger.Enqueue(
                //            Msg.PlayerSkill(Model.PlayerID 0u, s)
                //        )
                //)

                //[|
                //    asd.Keys.J, Model.Skill1
                //    asd.Keys.K, Model.Skill2
                //|]
                //|> iter(fun (k, s) ->
                //    if asd.Engine.Keyboard.GetKeyState k = asd.ButtonState.Push then
                //        messenger.Enqueue(
                //            Msg.PlayerSkill(Model.PlayerID 0u, s)
                //        )
                //)

        | Model.HowToControl ->
            if gameKeybaord.IsPush Select || gameKeybaord.IsPush Cancel then
                messenger.Enqueue(Msg.SetGameMode Model.GameMode)

        | Model.Pause
        | Model.GameFinished true ->
            if gameKeybaord.IsPush(Cancel) then
                messenger.Enqueue(Msg.SetGameMode Model.GameMode)

        | Model.GameFinished false
        | Model.ErrorUI _
        | Model.Stair _
        | Model.WaitingGenerating -> ()


    member this.PushControllerInput() : Msg option =
        gameKeybaord.Keys
        |> filter gameKeybaord.IsHold
        |> toList
        |> function
        | [] -> None
        | inputs ->
            (Model.PlayerID 0u, inputs |> Set.ofList)
            |> Msg.PlayerInputs
            |> Some