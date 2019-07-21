namespace WhiteDungeon.View.Game


open wraikny.Tart.Helper.Utils
open wraikny.Tart.Helper.Math
open wraikny.Tart.Core
open wraikny.Tart.Advanced
open wraikny.MilleFeuille.Fs.Objects
open wraikny.MilleFeuille.Core.Object
open wraikny.MilleFeuille.Core.Input
open wraikny.MilleFeuille.Fs.Input.Controller

open WhiteDungeon.Core
open WhiteDungeon.Core.Game
open WhiteDungeon.View
open WhiteDungeon.View.Utils.Color

open wraikny.MilleFeuille.Fs.Math
open wraikny.MilleFeuille.Fs.Geometry

open FSharpPlus


[<Class>]
type GameScene(gameModel : Model.Model, viewSetting, gameViewSetting) =
    inherit Scene()

    let viewSetting : ViewSetting = viewSetting
    let gameViewSetting : Setting.GameViewSetting = gameViewSetting

    let messenger : IMessenger<_, _, _> =
        Messenger.buildMessenger
            { seed = 0 }
            {
                init = gameModel, Cmd.viewMsg [ViewMsg.GenerateDungeonView (ViewMsg.DungeonView.fromModel gameModel)]
                view = ViewModel.ViewModel.view
                update = Update.Update.update
            }

    let notifier = new Notifier<_, _, _>(messenger)

    let port = {
        new Port<_, ViewMsg.ViewMsg>(messenger) with
        override __.OnPopMsg(msg) = ()
    }

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
        :> Controller.IController<Game.Msg.PlayerInput>
        

    let controllers = [|
        Game.Model.PlayerID 0u, gameKeybaord
    |]


    let playersUpdater : ActorsUpdater<ViewModel.ViewModel, PlayerView, ViewModel.PlayerView> =
        ActorsUpdaterBuilder.build "PlayersUpdater" {
            initActor = fun () -> new PlayerView(gameViewSetting)
            selectActor = ViewModel.ViewModel.selectPlayers
        }

    let skillAreaPlayerUpdater : ActorsUpdater<ViewModel.ViewModel, SkillEmitView, ViewModel.AreaSkillEmitView> =
        ActorsUpdaterBuilder.build "skillAreaPlayerUpdater" {
            initActor = fun () -> new SkillEmitView(gameViewSetting)
            selectActor = fun vm -> vm.areaPlayer
        }

    let skillAreaEnemyUpdater : ActorsUpdater<ViewModel.ViewModel, SkillEmitView, ViewModel.AreaSkillEmitView> =
        ActorsUpdaterBuilder.build "skillAreaEnemyUpdater" {
            initActor = fun () -> new SkillEmitView(gameViewSetting)
            selectActor = fun vm -> vm.areaEnemy
        }


    let skillAreaAllUpdater : ActorsUpdater<ViewModel.ViewModel, SkillEmitView, ViewModel.AreaSkillEmitView> =
        ActorsUpdaterBuilder.build "skillAreaAllUpdater" {
            initActor = fun () -> new SkillEmitView(gameViewSetting)
            selectActor = fun vm -> vm.areaAll
        }

    let dungeonCamera = new GameCamera()
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

    let playerCamera = new GameCamera()

    let skillEffectsCamera = new GameCamera()

    let backLayer =
        let color = new asd.Color(255uy, 255uy, 255uy)
        new UI.BackGroundLayer(color)

    let dungeonLayer = new asd.Layer2D()

    let playerLayer = new asd.Layer2D()

    let skillEffectsLayer = new asd.Layer2D()

    let uiLayer = new asd.Layer2D()

    override this.OnRegistered() =
        this.AddLayer(backLayer)
        this.AddLayer(dungeonLayer)
        this.AddLayer(playerLayer)
        this.AddLayer(skillEffectsLayer)
        this.AddLayer(uiLayer)

        this.AddDungeonView(gameModel)

        dungeonLayer.AddObject(dungeonCamera)
        // dungeonLayer.AddObject(minimapCamera)
        playerLayer.AddComponent(playersUpdater, playersUpdater.Name)
        playerLayer.AddObject(playerCamera)

        [|
            skillAreaPlayerUpdater
            skillAreaEnemyUpdater
            skillAreaAllUpdater
        |]
        |> iter(fun u -> skillEffectsLayer.AddComponent(u, u.Name))
            

        skillEffectsLayer.AddObject(skillEffectsCamera)

        let notifier = notifier :> IObservable<_>
        notifier.Add(dungeonCamera) |> ignore
        // notifier.AddObserver(minimapCamera)

        // player
        notifier.Add(playerCamera) |> ignore
        notifier.Add(playersUpdater) |> ignore

        // Skill
        notifier.Add(skillAreaPlayerUpdater) |> ignore
        notifier.Add(skillAreaEnemyUpdater) |> ignore
        notifier.Add(skillAreaAllUpdater) |> ignore
        notifier.Add(skillEffectsCamera) |> ignore

        messenger.SetPort(port)
        messenger.StartAsync() |> ignore

    override this.OnDispose() =
        messenger.Stop()


    override this.OnUpdated() =
        port.Update()
        notifier.Pull() |> ignore

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