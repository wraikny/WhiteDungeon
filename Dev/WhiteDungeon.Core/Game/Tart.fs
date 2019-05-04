module WhiteDungeon.Core.Game.Tart

open wraikny.Tart.Core
open WhiteDungeon.Core.Game


let private initModel p =
    let (_, _, dungeonModel, gameSetting) = p

    let dungeonView =
        dungeonModel
        |> ViewMsg.DungeonView.fromModel gameSetting

    Model.Model.init p, Cmd.viewMsg [ViewMsg.GenerateDungeonView dungeonView]


let createMessage updater initParam =
    Messenger.buildMessenger
        {
            seed = 0
            updater = Some updater
        }
        {
            init = initModel initParam
            update = Update.Update.update
            view = ViewModel.ViewModel.view
        }