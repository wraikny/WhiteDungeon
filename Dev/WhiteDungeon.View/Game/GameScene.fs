namespace WhiteDungeon.View.Game


open wraikny.Tart.Helper.Utils
open wraikny.Tart.Helper.Math
open wraikny.Tart.Core
open wraikny.Tart.Advanced
open wraikny.MilleFeuille.Fs.Objects
open wraikny.MilleFeuille.Core.Object
open wraikny.MilleFeuille.Core.Input


open WhiteDungeon.Core
open WhiteDungeon.Core.Game
open WhiteDungeon.View
open WhiteDungeon.View.Utils.Color

open WhiteDungeon.View.Utils.Geometry


[<Class>]
type GameScene(gameModel : Model.Model, viewSetting, gameViewSetting, controllers) =
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
        override __.OnUpdate(msg) = ()
    }

    let controllers : (Model.PlayerID * Controller.IController<Msg.PlayerInput>) list =
        controllers


    let playersUpdater : ActorsUpdater<ViewModel.ViewModel, PlayerView, ViewModel.PlayerView> =
        ActorsUpdaterBuilder.build "PlayersUpdater" {
            initActor = fun () -> new PlayerView(gameViewSetting)
            selectActor = ViewModel.ViewModel.selectPlayers >> Some
        }

    let skillEmitsUpdater : ActorsUpdater<ViewModel.ViewModel, SkillEmitView, ViewModel.AreaSkillEmitView> =
        ActorsUpdaterBuilder.build "skillEmitsUpdater" {
            initActor = fun () -> new SkillEmitView(gameViewSetting)
            selectActor = ViewModel.ViewModel.selectAreaSkillEmits >> Some
        }

    let dungeonCamera = new GameCamera()
    let minimapCamera =
        new GameCamera(
            Zoom = 3.0f
            , Dst =(
                let windowSize = asd.Engine.WindowSize
                let size = windowSize / 8
                let pos = new asd.Vector2DI(windowSize.X - 50, 50)
                new asd.RectI(
                    pos - (new asd.Vector2DI(size.X, 0))
                    , size
                )
            )
        )

    let playerCamera = new GameCamera()

    let backLayer =
        let color = new asd.Color(255uy, 255uy, 255uy)
        new UI.BackGroundLayer(color)

    let dungeonLayer = new asd.Layer2D()

    let playerLayer = new asd.Layer2D()

    let uiLayer = new asd.Layer2D()

    override this.OnRegistered() =
        this.AddLayer(backLayer)
        this.AddLayer(dungeonLayer)
        this.AddLayer(playerLayer)
        this.AddLayer(uiLayer)

        this.AddDungeonView(gameModel)

        dungeonLayer.AddObject(dungeonCamera)
        // dungeonLayer.AddObject(minimapCamera)
        playerLayer.AddComponent(playersUpdater, playersUpdater.Name)
        playerLayer.AddObject(playerCamera)

        notifier.AddObserver(dungeonCamera)
        // notifier.AddObserver(minimapCamera)
        notifier.AddObserver(playerCamera)
        notifier.AddObserver(playersUpdater)

        messenger.SetPort(port)
        messenger.StartAsync() |> ignore

    override this.OnDispose() =
        messenger.Stop()


    override this.OnUpdated() =
        port.Update()
        notifier.Pull() |> ignore

        this.PushControllerInput()


    member this.PushControllerInput() =
        controllers
        |> List.map(fun (id, controller) ->
            let getStateIs (state : asd.ButtonState) key =
                controller.GetState(key)
                |> Option.ofNullable
                |> Option.map ((=) state)
                |> Option.defaultValue false

            [
                Msg.UpKey
                Msg.DownKey
                Msg.RightKey
                Msg.LeftKey
                Msg.DashKey
            ]
            |> List.filter (getStateIs asd.ButtonState.Hold)
            |> function
            | [] -> false
            | inputs ->
                Msg.PlayerInput (id, inputs |> Set.ofList)
                |> messenger.PushMsg

                true
        )
        |> List.fold (||) false
        |> function
        | true ->
            Msg.TimePasses
            |> messenger.PushMsg
        | false -> ()


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