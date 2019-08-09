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
    | StartGame of Game.Model.Model * float32


let update (msg : Msg) (model : Model) : Model * Cmd<Msg, ViewMsg> =
    msg |> function
    | UndoModel ->
        model.prevModel |> function
        | Some(x) ->
            { x with prevModel = None }, Cmd.port(SetBGMVolume (float32 x.bgmVolume / 10.0f))
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
        { model with bgmVolume = v}, Cmd.port(SetBGMVolume (float32 v / 10.0f))

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
        monad {
            let! seed = Random.int minValue<int> maxValue<int>
            
            //let roomsCount = (model.dungeonBuilder.roomCount - 1)
            let gen = Random.int minValue<int> maxValue<int>
            let! roomIndex = gen
            let! gateCellIndexs = Random.list model.gateCount (Random.pair gen gen)
            return (seed, roomIndex, gateCellIndexs)
        }
        |> TartTask.withEnv(fun (seed, roomIndex, gateCellIndexs) -> async {
            let dungeonModel =
                { model.dungeonBuilder with seed = seed }
                |> DungeonBuilder.generate

            let largeRooms = toList dungeonModel.largeRooms

            let largeRoomsCount = length largeRooms

            let initRoomIndex = roomIndex % largeRoomsCount

            let initRoom = snd largeRooms.[initRoomIndex]

            let fromCell =
                model.gameSetting.dungeonCellSize
                |> DungeonModel.cellToCoordinate

            let initPosition =
                initRoom.rect
                |>> fromCell
                |> Rect.centerPosition

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
                            (initPosition - (Vec2.init (float32 index) 0.0f) * size)
                            status
                            playerId
                            character

                    (playerId, player)
                )
                |> Map.ofSeq


            let gateCells =
                seq {
                    for (a, b) in gateCellIndexs ->
                        let room = snd largeRooms.[a % largeRoomsCount]
                        let cells = room |> Space.cells
                        let cell = fst cells.[length cells % b]
                        cell
                }
                |> toList


            let gameModel =
                Game.Model.Model.init
                    players
                    model.dungeonBuilder
                    dungeonModel
                    gateCells
                    model.gameSetting

            return gameModel
        })
        |> TartTask.perform (ErrorUI >> SetUI) GeneratedGameModel
        |> fun cmd ->
            { model with uiMode = WaitingGenerating }, cmd

    | GeneratedGameModel gameModel ->
        model, Cmd.port(ViewMsg.StartGame (gameModel, float32 model.bgmVolume / 10.0f))

    | CloseGameMsg ->
        model, Cmd.port CloseGame