module WhiteDungeon.View.MainScene.Update

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry
open wraikny.Tart.Helper.Collections
open wraikny.Tart.Core
open wraikny.Tart.Core.Libraries
open wraikny.Tart.Advanced.Dungeon

open WhiteDungeon.Core
//open WhiteDungeon.Core.Game
open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game

open FSharpPlus

open WhiteDungeon.View.MainScene.Model


type ViewMsg =
    | SetBGMVolume of float32
    | CloseGame
    | StartGame of Game.Model.Model * int * float32


let bgmToFloat bgmVolume = float32 bgmVolume / 100.0f


let update (msg : Msg) (model : Model) : Model * Cmd<Msg, ViewMsg> =
    try
        let model = { model with msgHistory = msg::model.msgHistory }

        msg |> function
        | UndoModel ->
            model.prevModel |> function
            | Some(x) ->
                { x with prevModel = None; uiMode = Title }, Cmd.port(SetBGMVolume (bgmToFloat x.bgmVolume))
            | None ->
                model, Cmd.none
        | SetUIWithHistory uiMode ->
            { model with
                uiMode = uiMode
                occupationListToggle = false
                prevModel = Some { model with prevModel = None } }, Cmd.none
        | SetUI uiMode ->
            { model with
                uiMode = uiMode
                occupationListToggle = false }, Cmd.none

        | AddBGMVolume i ->
            let v = model.bgmVolume + i |> min 10 |> max 0 
            { model with bgmVolume = v}, Cmd.port(SetBGMVolume (bgmToFloat v))

        | OccupationListToggle x ->
            { model with occupationListToggle = x }, Cmd.none

        | InputName s ->
            { model with playerName = if s = "" then None else Some s }, Cmd.none

        | SelectOccupation x ->
            { model with
                selectOccupation = x
                occupationListToggle = false }, Cmd.none

        | SetDungeonParameters i ->
            { model with initSize = i }, Cmd.none

        | GenerateDungeon ->
            let randomCmd = Random.int minValue<int> maxValue<int> |> Random.generate SetGameSceneRandomSeed
            { model with uiMode = WaitingGenerating }, randomCmd

        | SetGameSceneRandomSeed x ->
            if model.uiMode = WaitingGenerating then
                let dungeonBuilder =
                    model.setting.gameSetting.createDungeonBuilder
                        1us model.initSize
                Game.Model.Dungeon.generateDungeonModel dungeonBuilder
                |> TartTask.perform (fun e ->
    #if DEBUG
                    System.Console.WriteLine(e)
    #endif
                    GenerateDungeon) GeneratedDungeonModel
                |> fun cmd ->
                    { model with gameSceneRandomSeed = x }, cmd
            else
                model, Cmd.none

        | GeneratedDungeonModel (dungeonBuilder, dungeonModel) ->
            if model.uiMode = WaitingGenerating then
                let gateCount = model.setting.gameSetting.gateCount 1us model.initSize
                let cmd =
                    Game.Model.Dungeon.generateDungeonParams
                        model.setting.gameSetting
                        gateCount
                        dungeonBuilder
                        dungeonModel
                        GeneratedDungeonParams
                model, cmd
            else
                model, Cmd.none

        | GeneratedDungeonParams dungeonParams ->
            if model.uiMode = WaitingGenerating then
                let gameModel =
                    //let size = model.setting.gameSetting.characterSize

                    let gameSetting = model.setting.gameSetting

                    let players =
                        [ model.playerName, model.selectOccupation ]
                        |> Seq.indexed
                        |> Seq.map(fun (index, (name, occupation)) ->
                            let name = Option.defaultValue (sprintf "Player%d" index) name

                            let setting =
                                gameSetting.occupationSettings
                                |> HashMap.find occupation

                            let size = setting.size

                            let level = model.setting.gameSetting.levelOffset

                            let status =
                                Game.Model.Actor.calcStatusOf
                                    setting.growthEasing
                                    gameSetting.playerGrowthRateOverMax
                                    gameSetting.maxLevel
                                    level
                                    setting.status

                            let character : Model.Character = {
                                id = Model.CharacterID -index
                                name = name
                                currentOccupation = occupation
                                //occupations = [
                                //    occupation, status
                                //] |> Map.ofList
                            }


                            let playerId = Game.Model.PlayerID (uint32 index)

                            let player =
                                Game.Model.Player.init
                                    size
                                    (dungeonParams.initPosition - (Vec2.init (float32 index) 0.0f) * size)
                                    level
                                    status
                                    playerId
                                    character

                            (playerId, player)
                        )
                        |> Map.ofSeq

                    Game.Model.Model.init
                        players
                        dungeonParams.dungeonBuilder
                        dungeonParams.dungeonModel
                        model.initSize
                        dungeonParams.gateCells
                        dungeonParams.enemyCells
                        model.setting.gameSetting

                model, Cmd.port(ViewMsg.StartGame (gameModel, model.gameSceneRandomSeed, bgmToFloat model.bgmVolume))
            else
                model, Cmd.none

        | CloseGameMsg ->
            model, Cmd.port CloseGame
    with e ->
        { model with uiMode = ErrorUI e }, Cmd.none