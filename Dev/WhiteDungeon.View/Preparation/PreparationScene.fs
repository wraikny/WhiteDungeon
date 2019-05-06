namespace WhiteDungeon.View.Preparation

open wraikny
open wraikny.Tart.Helper.Math
open wraikny.Tart.Core
open wraikny.Tart.Advanced

open wraikny.MilleFeuille.Core.Object
open wraikny.MilleFeuille.Core.UI
open wraikny.MilleFeuille.Fs.Input.Controller
open wraikny.MilleFeuille.Fs.Objects
open wraikny.MilleFeuille.Core.Object
open wraikny.MilleFeuille.Core.Input

open WhiteDungeon.Core
open WhiteDungeon.View

type PreparationScene(viewSetting, createTitleScene) =
    inherit Scene()

    let createTitleScene : ViewSetting -> asd.Scene = createTitleScene

    let viewSetting = viewSetting

    let gameSetting : Model.GameSetting = {
        dungeonCellSize = Vec2.init(200.0f, 200.0f)
        minPlayerCount = 1
        maxPlayerCount = 1
        characterSize = Vec2.init(100.0f, 100.0f)
    }

    let savedData : Model.SavedData = {
        charactersList = [
            ({
                id = Model.CharacterID 0
                name = "rainy"
                currentOccupation = Model.Sword
                occupations = [
                    Model.Sword, ({
                        level = 1
                        hp = 100
                        walkSpeed = 4.0f
                        dashSpeed = 8.0f
                    } : Model.ActorStatus)
                ] |> Map.ofList
            } : Model.Character)
        ]
        |> List.map(fun (c : Model.Character) -> c.id, c)
        |> Map.ofList
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
    let updater = new Tart.Updater()
    
    let messenger : IMessenger<Main.Msg, Main.ViewModel> =
        let pModel =
            Preparation.Model.init
                dungeonBuilder
                gameSetting
                savedData

        let pModel =
            { pModel with
                playerCount = 1
                players = [
                    0u, Some <| Model.CharacterID 0
                ]
                |> Map.ofList
            }

        Main.createMessenger (0, updater) pModel


    let notifier =
        new Notifier<Main.Msg, Main.ViewMsg, Main.ViewModel>(messenger)

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

    let gameKeybaord =
        KeyboardBuilder.init()
        |> KeyboardBuilder.bindKeysList
            [
                Game.Msg.UpKey    , asd.Keys.Up
                Game.Msg.DownKey  , asd.Keys.Down
                Game.Msg.RightKey , asd.Keys.Right
                Game.Msg.LeftKey  , asd.Keys.Left
                Game.Msg.DashKey  , asd.Keys.LeftShift
            ]
        |> KeyboardBuilder.build
        :> Controller.IController<Game.Msg.PlayerInput>
        

    let controllers = [
        Game.Model.Actor.PlayerID 0u, gameKeybaord
    ]
    

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
            messenger.PushMsg(Main.Msg.ToGame)
        )


        let selecter = new Button.ControllerButtonSelecter( titleButton.Button )
        
        selecter.AddController(keyboard) |> ignore
        
        uiLayer.AddComponent(selecter, "Selecter")

        updater.UpdatePreparation <- fun msg ->
            msg |> function
            | Preparation.ChangeToGame ->
                this.ChangeScene(new Game.GameScene(viewSetting, notifier, updater, controllers))
                |> ignore
                

        messenger.StartAsync() |> ignore


    override this.OnUpdated() =
        notifier.Update()
        updater.Update()