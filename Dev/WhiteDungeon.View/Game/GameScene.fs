﻿namespace WhiteDungeon.View.Game


open wraikny.Tart.Helper.Utils
open wraikny.Tart.Helper.Math
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

open wraikny.MilleFeuille.Fs.Math
open wraikny.MilleFeuille.Fs.Geometry

open FSharpPlus

open System
open System.Linq
open System.Reactive
open System.Reactive.Linq


[<Class>]
type GameScene(gameModel : Model.Model, viewSetting, gameViewSetting) =
    inherit Scene()

    let viewSetting : ViewSetting = viewSetting
    let gameViewSetting : Setting.GameViewSetting = gameViewSetting

    let messenger : IMessenger<_, _, _> =
        Messenger.build
            { seed = 0 }
            {
                init = gameModel, Cmd.viewMsg [ViewMsg.GenerateDungeonView (ViewMsg.DungeonView.fromModel gameModel)]
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



    override this.OnRegistered() =
        this.AddLayer(backLayer)
        this.AddLayer(dungeonLayer)
        this.AddLayer(playerLayer)
        this.AddLayer(skillEffectsLayer)
        this.AddLayer(uiLayer)

        this.AddDungeonView(gameModel)

        dungeonLayer.AddObject(dungeonCamera)
        playerLayer.AddObject(playerCamera)
        // dungeonLayer.AddObject(minimapCamera)
        skillEffectsLayer.AddObject(skillEffectsCamera)

        messenger.StartAsync() |> ignore

    override this.OnDispose() =
        messenger.Stop()


    override this.OnUpdated() =
        messenger.NotifyView()

        #if DEBUG
        this.PushControllerInput() |> function
        | true ->
            messenger.Enqueue(Msg.TimePasses)
        | false ->
            if asd.Engine.Keyboard.GetKeyState(asd.Keys.T) = asd.ButtonState.Hold then
                messenger.Enqueue(Msg.TimePasses)
            ()
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


    member this.PushControllerInput() : bool =
        controllers
        |>> fun (id, controller) ->
            let getStateIs (state : asd.ButtonState) key =
                controller.GetState(key)
                |> Option.ofNullable
                |>> ((=) state)
                |> Option.defaultValue false

            Msg.PlayerInput.inputs
            |> filter (getStateIs asd.ButtonState.Hold)
            |> function
            | [] -> false
            | inputs ->
                Msg.PlayerInputs (id, inputs |> Set.ofList)
                |> messenger.Enqueue

                true
        |> fold (||) false


    member this.AddDungeonView(gameModel : Model.Model) =
        let dungeonView = ViewMsg.DungeonView.fromModel gameModel

        let getRectangleShape (rect) =
            new asd.RectangleShape(
                DrawingArea = Rect.toRectF rect
            )

        for space in dungeonView.corridors do
            new asd.GeometryObject2D(
                Shape = getRectangleShape space
                , Color = ColorPalette.ume
            )
            |> dungeonLayer.AddObject

        for space in dungeonView.smallRooms do
            new asd.GeometryObject2D(
                Shape = getRectangleShape space
                , Color = ColorPalette.sakura
            )
            |> dungeonLayer.AddObject

        for space in dungeonView.largeRooms do
            new asd.GeometryObject2D(
                Shape = getRectangleShape space
                , Color = ColorPalette.sakura
            )
            |> dungeonLayer.AddObject