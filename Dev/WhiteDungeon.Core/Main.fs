module WhiteDungeon.Core.Main

open WhiteDungeon.Core

open wraikny.Tart.Helper
open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry
open wraikny.Tart.Helper.Monad
open wraikny.Tart.Helper.Extension
open wraikny.Tart.Core
open wraikny.Tart.Core.Libraries
open wraikny.Tart.Advanced

[<Struct>]
type Scene =
    | Preparation
    | Game


type Model =
    | PreparationModel of Preparation.Model
    | GameModel of Game.Model.Model


type Msg =
    | PreparationMsg of Preparation.Msg
    | GameMsg of Game.Msg.Msg
    | ToGame
    | GeneratedDungeonModel of Dungeon.DungeonModel


type ErrorKind =
    | ModelMsgMismatch


type ViewMsg =
    | PreparationViewMsg of Preparation.ViewMsg
    | GameViewMsg of Game.ViewMsg.ViewMsg
    // | Error of ErrorKind


let update (msg : Msg) (model : Model) : Model * Cmd<Msg, ViewMsg> =
    (msg, model) |> function
    | PreparationMsg msg, PreparationModel model ->
        let model, cmd = Preparation.Update.update msg model

        let cmd =
            cmd
            |> Cmd.mapCommands PreparationMsg
            |> Cmd.mapViewMsgs PreparationViewMsg

        PreparationModel model, cmd

    | GameMsg msg, GameModel model ->
        let model, cmd = Game.Update.Update.update msg model

        let cmd =
            cmd
            |> Cmd.mapCommands GameMsg
            |> Cmd.mapViewMsgs GameViewMsg

        GameModel model, cmd

    | ToGame, PreparationModel pModel ->
        let allPlayersSelectedChatacer =
            pModel.players
            |> Map.toList
            |> List.take pModel.playerCount
            |> List.map(snd)
            |> List.exists(fun id -> id = None)
            |> not

        if allPlayersSelectedChatacer then
            let randMsg = Preparation.SetRandomRoomIndex >> PreparationMsg
            let generator = Random.int 0 (pModel.dungeonBuilder.roomCount - 1)

            let randCmd = Random.generate randMsg generator

            let taskCmd =
                (fun () ->
                    pModel.dungeonBuilder
                    |> Dungeon.DungeonBuilder.generate
                    |> Result<_, Basic.Never>.Ok
                )
                |> Task.init
                |> Task.perform GeneratedDungeonModel

            model, Cmd.batch [randCmd; taskCmd; Cmd.viewMsg [PreparationViewMsg Preparation.ViewMsg.ChangeToGame]]
        else
            model, Cmd.none


    | GeneratedDungeonModel dungeonModel, PreparationModel pModel ->
        let largeRooms =
            dungeonModel.largeRooms
            |> Map.toList

        let largeRoomsCount =
            largeRooms
            |> List.length

        let targetRoomIndex =
            pModel.randomRoomIndex % largeRoomsCount

        let targetRoom = snd largeRooms.[targetRoomIndex]

        let fromCell =
            pModel.gameSetting.dungeonCellSize
            |> Model.GameSetting.fromDungeonCell

        let targetPosition =
            targetRoom.rect
            |> Rect.map fromCell
            |> Rect.centerPosition
        
        // TODO
        let players =
            pModel.players
            |> Map.toList
            |> List.indexed
            |> Seq.filterMap(fun (index, (id, charaID)) ->
                maybe {
                    let! charaID = charaID
                    let! character =
                        pModel.savedData.charactersList
                        |> Map.tryFind charaID

                    let size =
                        pModel.gameSetting.characterSize

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
                pModel.dungeonBuilder
                dungeonModel
                pModel.gameSetting

        let dungeonView =
            dungeonModel
            |> Game.ViewMsg.DungeonView.fromModel pModel.gameSetting


        GameModel gameModel, Cmd.viewMsg [
            GameViewMsg <| Game.ViewMsg.GenerateDungeonView dungeonView
        ]

    | _ ->
        // TODO: Error ModelMsgMismatch
        model, Cmd.none


type ViewModel =
    | PreparationViewModel of Preparation.ViewModel
    | GameViewModel of Game.ViewModel.ViewModel


let view (model : Model) : ViewModel =
    model |> function
    | PreparationModel model ->
        Preparation.ViewModel.view model
        |> PreparationViewModel

    | GameModel model ->
        Game.ViewModel.ViewModel.view model
        |> GameViewModel



let init (pModel : Preparation.Model) =
    PreparationModel pModel, Cmd.none


let createMessenger (seed) pModel =
    Messenger.buildMessenger
        {
            seed = seed
        }
        {
            init = init pModel
            update = update
            view = view
        }