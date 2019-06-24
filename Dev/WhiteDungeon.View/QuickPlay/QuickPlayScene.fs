namespace WhiteDungeon.View.QuickPlay

open wraikny
open wraikny.Tart.Helper.Math
open wraikny.Tart.Core
open wraikny.Tart.Advanced

open wraikny.MilleFeuille.Core.Object
open wraikny.MilleFeuille.Core.UI
open wraikny.MilleFeuille.Fs.Objects
open wraikny.MilleFeuille.Core.Object
open wraikny.MilleFeuille.Core.Input

open WhiteDungeon.Core
open WhiteDungeon.View
open WhiteDungeon.View.Utils.Color


type QuickPlayScene(viewSetting, createTitleScene) as this =
    inherit Scene()

    let createTitleScene : ViewSetting -> asd.Scene = createTitleScene

    let viewSetting = viewSetting

    let gameSetting : Model.GameSetting = {
        dungeonCellSize = Vec2.init(200.0f, 200.0f)
        minPlayerCount = 1
        maxPlayerCount = 1
        binarySearchCountMovingOnWall = 4
        characterSize = Vec2.init(100.0f, 100.0f)
        occupationDefaultStatus = [
            Model.Hunter, ({
                level = 1
                hp = 100.0f
                walkSpeed = 4.0f
                dashSpeed = 8.0f
            } : Model.ActorStatus)
        ] |> Map.ofList
    }

    let gameViewSetting : Setting.GameViewSetting = {
        occupationImages = [
            Model.Hunter, ({
                front = "Image/Debug/down.png"
                frontRight = "Image/Debug/rightdown.png"
                frontLeft = "Image/Debug/leftdown.png"
                back = "Image/Debug/up.png"
                backRight = "Image/Debug/rightup.png"
                backLeft = "Image/Debug/leftup.png"
                right = "Image/Debug/right.png"
                left = "Image/Debug/left.png"
            } : Setting.ActorImages)
        ] |> Map.ofList
    }

    let dungeonBuilder : Dungeon.DungeonBuilder = {
        seed = 0
        roomCount = 300

        roomGeneratedRange = (100.0f, 100.0f)

        minRoomSize = (8, 8)
        maxRoomSize = (16, 16)

        roomMoveRate = 0.2f
        roomMeanThreshold = 1.25f
        restoreEdgeRate = 0.1f
        corridorWidth = 3
    }

    let viewSetting = viewSetting
    
    let messenger : IMessenger<QuickPlay.Msg, QuickPlay.ViewMsg, QuickPlay.ViewModel> =
        let model =
            QuickPlay.Model.init
                dungeonBuilder
                gameSetting

        let model =
            { model with
                playerCount = 1
                players = [
                    0u, ("Player1", Some Model.Occupation.Hunter)
                ]
                |> Map.ofList
            }

        Messenger.buildMessenger
            { seed = 0 }
            {
                init = model, Cmd.none
                view = QuickPlay.ViewModel.view
                update = QuickPlay.Update.update
            }


    // let notifier = new Notifier<QuickPlay.Msg, QuickPlay.ViewMsg, QuickPlay.ViewModel>(messenger)

    let keyboard = UI.Keybaord.createUIKeyboard()

    let backLayer =
        let color = new asd.Color(255uy, 255uy, 255uy)
        new UI.BackGroundLayer(color)

    let uiLayer = new asd.Layer2D()

    let windowSize = asd.Engine.WindowSize.To2DF()

    let buttonFont =
        let color = asd.Color(255uy, 255uy, 255uy)
        asd.Engine.Graphics.CreateDynamicFont(
            viewSetting.uiFontPath
            , viewSetting.menuButtonFontSize
            , ColorPalette.sumire
            , 0
            , color
        )

    let createButton text position =
        let size = viewSetting.menuButtonSize
        UI.ButtonRectangle.create text buttonFont position size

    let titleButton =
        createButton "Title" viewSetting.button1

    let characterButton =
        createButton "Character" viewSetting.button2

    let startButton =
        createButton "Start" viewSetting.button3

    //let characters =
    //    savedData.charactersList
    //    |> Map.toList
    //    |> List.map snd

    let port = {
        new Port<_, _>(messenger) with
        override __.OnUpdate(msg) =
            msg |> function
            | QuickPlay.ChangeToGame(gameModel) ->
                this.ChangeScene(new Game.GameScene(gameModel, viewSetting, gameViewSetting))
                |> ignore
    }
    

    override this.OnRegistered() =
        this.AddLayer(backLayer)
        this.AddLayer(uiLayer)

        uiLayer.AddObject(titleButton)
        uiLayer.AddObject(characterButton)
        uiLayer.AddObject(startButton)

        titleButton.Button
            .Chain(characterButton.Button, Button.ButtonDirection.Down)
            .Chain(startButton.Button, Button.ButtonDirection.Down)
            |> ignore

        titleButton.Button.add_OnReleasedEvent (fun _ ->
            messenger.Stop()
            this.ChangeScene(createTitleScene viewSetting) |> ignore
        )

        characterButton.Button.add_OnReleasedEvent (fun _ ->
            ()
        )

        startButton.Button.add_OnReleasedEvent (fun _ ->
            uiLayer.Dispose()
            this.AddLayer <|
                new UI.MessageLayer(
                    viewSetting,
                    MessageText = "Loading Dungeon"
                )
            
            messenger.Enqueue(QuickPlay.Msg.GenerateDungeon)
        )

        let selecter = new Button.ControllerButtonSelecter( titleButton.Button )

        selecter.AddController(keyboard) |> ignore
        
        uiLayer.AddComponent(selecter, "Selecter")

        messenger.SetPort(port)
        messenger.StartAsync() |> ignore


    override this.OnUpdated() =
        // notifier.Pull() |> ignore
        port.Update()