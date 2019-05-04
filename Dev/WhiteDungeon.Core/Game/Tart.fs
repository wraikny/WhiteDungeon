module WhiteDungeon.Core.Game.Tart

open wraikny.Tart.Core
open WhiteDungeon.Core.Game


let initModel p = Model.Model.init p, Cmd.none


let createMessage updater initParam =
    Messenger.buildMessenger
        {
            seed = 0
            updater = updater
        }
        {
            init = initModel initParam
            update = Update.Update.update
            view = ViewModel.ViewModel.view
        }