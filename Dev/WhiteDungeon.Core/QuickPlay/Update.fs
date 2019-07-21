module WhiteDungeon.Core.QuickPlay.Update

open wraikny.Tart.Helper
open wraikny.Tart.Helper.Collections
open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry
open wraikny.Tart.Core
open wraikny.Tart.Core.Libraries
open wraikny.Tart.Advanced

open WhiteDungeon.Core

open FSharpPlus
open wraikny.Tart.Helper.Extension

let incrPlayer (model : Model) : Model * Cmd<Msg, _> =
    let playerCount =
        (model.playerCount + 1)
        |> min model.gameSetting.maxPlayerCount

    if model.playerCount < playerCount then
        { model with
            playerCount = playerCount
            players =
                let id = uint32 playerCount

                model.players
                |> Map.add id (sprintf "Player%d" id, None)
        }, Cmd.none
    else
        model, Cmd.none


let decrPlayer (model : Model) : Model * Cmd<Msg, _> =
    let playerCount =
        (model.playerCount - 1)
        |> max model.gameSetting.minPlayerCount
        |> max 0

    { model with
        playerCount = playerCount
    }, Cmd.none


let update (msg : Msg) (model : Model) : Model * Cmd<Msg, ViewMsg> =
    model.mode |> function
    | WaitingDungeonGenerating ->
        model, Cmd.none

    | Default ->
        msg |> function
        | IncrPlayer ->
            incrPlayer model

        | DecrPlayer ->
            decrPlayer model

        | SelectOccupation(index, occupation) ->
            model.gameSetting.occupationDefaultStatus
            |> Map.containsKey occupation
            |> function
            | true ->
                let name = model.players |> Map.find index |> fst
                { model with
                    players =
                        model.players
                        |> Map.add index (name, Some occupation)
                }, Cmd.none
            | false ->
                model, Cmd.none

        | SetRandomRoomIndex index ->
            { model with randomRoomIndex = index }, Cmd.none

        | GenerateDungeon ->
            let allPlayersSelectedChatacer =
                model.players
                |> Map.toList
                |> take model.playerCount
                |>> snd
                |> exists(snd >> (=) None)
                |> not

            if allPlayersSelectedChatacer then
                let rand =
                    let generator = Random.int 0 (model.dungeonBuilder.roomCount - 1)
                    Random.generate SetRandomRoomIndex generator
                    
                let task =
                    async {
                        return
                            model.dungeonBuilder
                            |> Dungeon.DungeonBuilder.generate }
                    |> Async.perform GeneratedDungeonModel

                model, Cmd.batch[rand; task]
            else
                model, Cmd.none

        | GeneratedDungeonModel dungeonModel ->
            let largeRooms =
                dungeonModel.largeRooms
                |> HashMap.toList

            let largeRoomsCount =
                largeRooms
                |> List.length

            let targetRoomIndex =
                 model.randomRoomIndex % largeRoomsCount

            let targetRoom = snd largeRooms.[targetRoomIndex]

            let fromCell =
                model.gameSetting.dungeonCellSize
                |> Dungeon.DungeonModel.cellToCoordinate

            let targetPosition =
                targetRoom.rect
                |>> fromCell
                |> Rect.centerPosition
            
            // TODO
            let players =
                model.players
                |> Map.toList
                |> List.indexed
                |> filterMap(fun (index, (id, (name, occupation))) ->
                    monad {
                        let! occupation = occupation
                        let status = model.gameSetting.occupationDefaultStatus |> Map.find occupation
                        let character : Model.Character = {
                            id = Model.CharacterID -(int id)
                            name = name
                            currentOccupation = occupation
                            occupations = [
                                occupation, status
                            ] |> Map.ofList
                        }

                        let size = model.gameSetting.characterSize

                        let player =
                            Game.Model.Actor.Player.init
                                size
                                (targetPosition - (Vec2.init(float32 index, 0.0f) * size))
                                status
                                (Game.Model.PlayerID id)
                                character

                        return
                            (Game.Model.PlayerID id, player)
                    }
                )
                |> Map.ofSeq

            let gameModel =
                Game.Model.Model.init
                    players
                    model.dungeonBuilder
                    dungeonModel
                    model.gameSetting

            model, Cmd.viewMsg [ViewMsg.ChangeToGame (gameModel)]
