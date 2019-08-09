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

open FSharpPlus

open WhiteDungeon.View.MainScene.Model


type ViewMsg =
    | SetBGMVolume of float32
    | CloseGame
    | StartGame of Game.Model.Model * int * float32


let bgmToFloat bgmVolume = float32 bgmVolume / 100.0f


let update (msg : Msg) (model : Model) : Model * Cmd<Msg, ViewMsg> =
    msg |> function
    | UndoModel ->
        model.prevModel |> function
        | Some(x) ->
            { x with prevModel = None }, Cmd.port(SetBGMVolume (bgmToFloat x.bgmVolume))
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

    | SetDungeonParameters(count, minSize, maxSize, range, corridor, gateCount) ->
        { model with
            gateCount = gateCount
            dungeonBuilder =
                { model.dungeonBuilder with
                    roomCount = count
                    minRoomSize = (minSize, minSize)
                    maxRoomSize = (maxSize, maxSize)
                    roomGeneratedRange = (range, range)
                    corridorWidth = corridor
                }
        }, Cmd.none

    | GenerateDungeon ->
        let randomCmd = Random.int minValue<int> maxValue<int> |> Random.generate SetGameSceneRandomSeed
        { model with uiMode = WaitingGenerating }, randomCmd

    | SetGameSceneRandomSeed x ->
        Game.Model.Dungeon.generateTask model.gameSetting model.dungeonBuilder model.gateCount
        |> TartTask.perform (fun _ -> GenerateDungeon) GeneratedGameModel
        |> fun cmd ->
            { model with gameSceneRandomSeed = x }, cmd

    | GeneratedGameModel dungeonParams ->
        let gameModel =
            let size = model.gameSetting.characterSize
            let players =
                [ model.playerName, model.selectOccupation ]
                |> Seq.indexed
                |> Seq.map(fun (index, (name, occupation)) ->
                    let name = Option.defaultValue (sprintf "Player%d" index) name

                    let status =
                        model.gameSetting.occupationDefaultStatus
                        |> Map.find occupation

                    let character : Model.Character = {
                        id = Model.CharacterID -index
                        name = name
                        currentOccupation = occupation
                        occupations = [
                            occupation, status
                        ] |> Map.ofList
                    }


                    let playerId = Game.Model.PlayerID (uint32 index)

                    let player =
                        Game.Model.Actor.Player.init
                            size
                            (dungeonParams.initPosition - (Vec2.init (float32 index) 0.0f) * size)
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
                dungeonParams.gateCells
                model.gameSetting

        model, Cmd.port(ViewMsg.StartGame (gameModel, model.gameSceneRandomSeed, bgmToFloat model.bgmVolume))

    | CloseGameMsg ->
        model, Cmd.port CloseGame