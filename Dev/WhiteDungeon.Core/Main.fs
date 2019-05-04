module WhiteDungeon.Core.Main

open WhiteDungeon.Core

open wraikny.Tart.Core

type Model =
    | PreparationModel
    | GameModel of Game.Model.Model


type Msg =
    | GameMsg of Game.Msg.Msg


type ErrorKind =
    | ModelMsgMismatch


type ViewMsg =
    | GameViewMsg of Game.ViewMsg.ViewMsg
    // | Error of ErrorKind


let update (msg : Msg) (model : Model) : Model * Cmd<Msg, ViewMsg> =
    (msg, model) |> function
    | GameMsg msg, GameModel model ->
        let model, cmd = Game.Update.Update.update msg model

        let cmd =
            cmd
            |> Cmd.mapCommands GameMsg
            |> Cmd.mapViewMsgs GameViewMsg

        GameModel model, cmd

    | _ ->
        // TODO: Error ModelMsgMismatch
        model, Cmd.none


type ViewModel =
    | Pseudo
    | GameViewModel of Game.ViewModel.ViewModel


let view (model : Model) : ViewModel =
    model |> function
    | PreparationModel ->
        Pseudo

    | GameModel model ->
        Game.ViewModel.ViewModel.view model
        |> GameViewModel



let init() =
    PreparationModel, Cmd.none


let createMessenger (seed, updater) =
    Messenger.buildMessenger
        {
            seed = seed
            updater = Some updater
        }
        {
            init = init()
            update = update
            view = view
        }