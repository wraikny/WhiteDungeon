namespace WhiteDungeon.View.Game


open wraikny.Tart.Helper.Utils
open wraikny.Tart.Helper.Math
open wraikny.Tart.Core
open wraikny.MilleFeuille.Fs.Objects
open wraikny.MilleFeuille.Core.Object
open wraikny.MilleFeuille.Core.Input


open WhiteDungeon.Core
open WhiteDungeon.Core.Game
open WhiteDungeon.View

open WhiteDungeon.View.Utils.Geometry


[<Class>]
type GameScene(viewSetting, notifier, updater, controllers) =
    inherit Scene()

    let viewSetting : ViewSetting = viewSetting

    let notifyer : Notifier<Main.Msg, Main.ViewMsg, Main.ViewModel> = notifier
    let port : Tart.Port = updater
    let messenger = notifier.Messenger

    let controllers : (Model.Actor.PlayerID * Controller.IController<Msg.PlayerInput>) list =
        controllers


    let playersUpdater : ActorsUpdater<Main.ViewModel, PlayerView, ViewModel.PlayerView> =
        ActorsUpdaterBuilder.build "PlayersUpdater" {
            initActor = fun () -> new PlayerView()
            selectActor = function
                | Main.GameViewModel viewModel ->
                    viewModel |> ViewModel.ViewModel.selectPlayers |> Some
                | _ -> None
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

    let messageFont =
        asd.Engine.Graphics.CreateDynamicFont(
            viewSetting.uiFontPath
            , viewSetting.messageFontSize
            , ColorPalette.sumire
            , 0
            , ColorPalette.sumire
        )

    let messageTextPosition = asd.Engine.WindowSize.To2DF() / 2.0f
    let messageText = new asd.TextObject2D(Text = "", Font = messageFont)

    member this.MessageText
        with get() = messageText.Text
        and  set(value) =
            messageText.Text <- value
            let size =
                messageFont.CalcTextureSize(value, asd.WritingDirection.Horizontal).To2DF()

            messageText.Position <- messageTextPosition - size / 2.0f

    member val IsDungeonLoaded = false with get, set


    override this.OnRegistered() =
        this.AddLayer(backLayer)
        this.AddLayer(dungeonLayer)
        this.AddLayer(playerLayer)
        this.AddLayer(uiLayer)


        dungeonLayer.AddObject(dungeonCamera)
        // dungeonLayer.AddObject(minimapCamera)
        playerLayer.AddComponent(playersUpdater, playersUpdater.Name)
        playerLayer.AddObject(playerCamera)

        uiLayer.AddObject(messageText)
        this.MessageText <- "Loading Dungeon"

        notifier.ClearObservers()
        notifier.AddObserver(dungeonCamera)
        // notifier.AddObserver(minimapCamera)
        notifier.AddObserver(playerCamera)
        notifyer.AddObserver(playersUpdater)


        port.UpdateGame <- fun msg ->
            msg |> function
            | ViewMsg.GenerateDungeonView dungeonView ->
                this.MessageText <- ""
                this.IsDungeonLoaded <- true

                this.AddDungeonView(dungeonView)


    override this.OnUpdated() =
        port.Pop()
        notifyer.Pull() |> ignore

        if this.IsDungeonLoaded then
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
                |> Main.GameMsg
                |> messenger.PushMsg

                true
        )
        |> List.fold (||) false
        |> function
            | x when x ->
                Msg.TimePasses
                |> Main.GameMsg
                |> messenger.PushMsg
            | _ -> ()


    member this.AddDungeonView(dungeonView : ViewMsg.DungeonView) =
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