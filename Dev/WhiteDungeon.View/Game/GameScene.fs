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
}


[<Class>]
type GameScene(gameModel : Model.Model, gameViewSetting : GameViewSetting, uiArgs : UIArgs) =
    inherit Scene()

    let messenger : IMessenger<_, _, _> =
        Messenger.build
            { seed = 0 }
            {
                init = gameModel, Cmd.port (ViewMsg.UpdateDungeonView gameModel.dungeonModel)
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
            new ActorsUpdater<_, _>(
                playerLayer, {
                    create = fun () -> new PlayerView(gameViewSetting)
                    onError = raise
                    onCompleted = fun () -> printfn "Completed PlayersUpdater"
                }
            ))
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
                    new ActorsUpdater<_, _>(skillEffectsLayer, {
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
                | ViewMsg.UpdateDungeonView dungeonModel ->
                    seq {
                        let cells = dungeonModel.cells |> HashMap.toSeq
                        let mutable index = 0u
                        for cell in cells do
                            yield (index, cell)
                            index <- index + 1u
                    }
                    |> toList
                    |> (dungeonCellUpdater :> IObserver<_>).OnNext
                | _ -> ()
            )


    let uiMouse =
        let mouse = new Input.CollidableMouse(5.0f, ColliderVisible = true)
        new UI.MouseButtonSelecter(mouse)


    let uiWindowMain =
        new UI.MouseWindow(uiArgs.windowSetting, uiMouse,
            Position = asd.Engine.WindowSize.To2DF() / 2.0f
        )


    override this.OnRegistered() =
        GC.Collect()

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
        uiLayer.AddObject(uiWindowMain)

        messenger.StartAsync() |> ignore

    override this.OnDispose() =
        messenger.Stop()


    override this.OnUpdated() =
        messenger.NotifyView()

        // Mouse inside window
        let mousePos = asd.Engine.Mouse.Position |> Vec2.fromVector2DF
        let ws = asd.Engine.WindowSize.To2DF() |> Vec2.fromVector2DF
        let margin = 0.025f
        if Rect.isInside mousePos (Rect.init zero ws) |> not then
            let mousePosX = asd.MathHelper.Clamp(mousePos.x, ws.x * (1.0f - margin * 2.0f), ws.x * margin)
            let mousePosY = asd.MathHelper.Clamp(mousePos.y, ws.y * (1.0f - margin * 2.0f), ws.y * margin)
            asd.Engine.Mouse.Position <- asd.Vector2DF(mousePosX, mousePosY)


        #if DEBUG
        this.PushControllerInput() |> function
        | [||] ->
            if asd.Engine.Keyboard.GetKeyState(asd.Keys.T) = asd.ButtonState.Hold then
                messenger.Enqueue(Msg.TimePasses)
        | msgs ->
            messenger.Enqueue(Msg.TimePasses)
            msgs |> iter messenger.Enqueue

        if asd.Engine.Keyboard.GetKeyState(asd.Keys.Space) = asd.ButtonState.Push then
            messenger.Enqueue(Msg.AppendSkillEmits)
            messenger.Enqueue(Msg.TimePasses)
        #else
        this.PushControllerInput() |> function
        | true ->
            messenger.Enqueue(Msg.TimePasses)
        | false ->
            ()
        #endif


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