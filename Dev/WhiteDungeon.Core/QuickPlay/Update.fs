﻿module WhiteDungeon.Core.Preparation.Update

open wraikny.Tart.Helper
open wraikny.Tart.Helper.Extension
open wraikny.Tart.Helper.Monad
open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry
open wraikny.Tart.Core
open wraikny.Tart.Core.Libraries
open wraikny.Tart.Advanced

open WhiteDungeon.Core


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
                |> Map.add id None
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

        | SelectCharacter(index, character) ->
            character |> function
            | None -> model, Cmd.none
            | Some(chara) ->
                if model.savedData.charactersList |> Map.containsKey chara then
                    { model with
                        players =
                            model.players
                            |> Map.add index character
                    }, Cmd.none
                else
                    model, Cmd.none

        | SetRandomRoomIndex index ->
            { model with randomRoomIndex = index }, Cmd.none

        | GenerateDungeon ->
            let allPlayersSelectedChatacer =
                model.players
                |> Map.toList
                |> List.take model.playerCount
                |> List.map(snd)
                |> List.exists(fun id -> id = None)
                |> not

            if allPlayersSelectedChatacer then
                let rand =
                    let generator = Random.int 0 (model.dungeonBuilder.roomCount - 1)
                    Random.generate SetRandomRoomIndex generator
                    
                let task =
                    (fun () ->
                        model.dungeonBuilder
                        |> Dungeon.DungeonBuilder.generate
                        |> Result<_, Basic.Never>.Ok
                    )
                    |> Task.init
                    |> Task.perform GeneratedDungeonModel

                model, Cmd.batch[rand; task]
            else
                model, Cmd.none
        | GeneratedDungeonModel dungeonModel ->
            let largeRooms =
                dungeonModel.largeRooms
                |> Map.toList

            let largeRoomsCount =
                largeRooms
                |> List.length

            let targetRoomIndex =
                 model.randomRoomIndex % largeRoomsCount

            let targetRoom = snd largeRooms.[targetRoomIndex]

            let fromCell =
                model.gameSetting.dungeonCellSize
                |> Model.GameSetting.fromDungeonCell

            let targetPosition =
                targetRoom.rect
                |> Rect.map fromCell
                |> Rect.centerPosition
            
            // TODO
            let players =
                model.players
                |> Map.toList
                |> List.indexed
                |> Seq.filterMap(fun (index, (id, charaID)) ->
                    maybe {
                        let! charaID = charaID
                        let! character =
                            model.savedData.charactersList
                            |> Map.tryFind charaID

                        let size =
                            model.gameSetting.characterSize

                        let! status =
                            character.occupations
                            |> Map.tryFind character.currentOccupation

                        let player =
                            Game.Model.Actor.Player.init
                                size
                                (targetPosition - (Vec2.init(float32 index, 0.0f) * size))
                                status
                                (Game.Model.Actor.PlayerID id)
                                character

                        return
                            (Game.Model.Actor.PlayerID id, player)
                    }
                )
                |> Seq.toList

            let gameModel =
                Game.Model.Model.init
                    players
                    model.dungeonBuilder
                    dungeonModel
                    model.gameSetting

            model, Cmd.viewMsg [ViewMsg.ChangeToGame (dungeonModel, gameModel)]
