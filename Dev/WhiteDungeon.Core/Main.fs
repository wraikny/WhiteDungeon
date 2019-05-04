module WhiteDungeon.Core.Main

open WhiteDungeon.Core

open wraikny.Tart.Helper
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
        let taskCmd =
            (fun () ->
                pModel.dungeonBuilder
                |> Dungeon.DungeonBuilder.generate
                |> Result<_, Basic.Never>.Ok
            )
            |> Task.init
            |> Task.perform GeneratedDungeonModel

        model, taskCmd

    | GeneratedDungeonModel dungeonModel, PreparationModel pModel ->
        let players = []
        let gameModel =
            Game.Model.Model.init
                players
                pModel.dungeonBuilder
                dungeonModel
                pModel.gameSetting

        GameModel gameModel, Cmd.none

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



let init pModel =
    PreparationModel pModel, Cmd.none


let createMessenger (seed, updater) pModel =
    Messenger.buildMessenger
        {
            seed = seed
            updater = Some updater
        }
        {
            init = init pModel
            update = update
            view = view
        }