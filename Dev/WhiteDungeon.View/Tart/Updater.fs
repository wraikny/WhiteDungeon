module WhiteDungeon.View.Tart

open wraikny
open WhiteDungeon.Core

type Updater() =
    inherit Tart.Core.Updater<Main.ViewMsg>()

    let mutable updatePreparation = fun (_ : Preparation.ViewMsg) -> ()

    let mutable updateGame = fun (_ : Game.ViewMsg.ViewMsg) -> ()

    member this.UpdatePreparation
        with private get() = updatePreparation
        and public set(value) = updatePreparation <- value

    member this.UpdateGame
        with private get() = updateGame
        and public set(value) = updateGame <- value


    override this.OnUpdate(viewMsg) =
        viewMsg |> function
        | Main.PreparationViewMsg viewMsg ->
            this.UpdatePreparation(viewMsg)

        | Main.GameViewMsg viewMsg ->
            this.UpdateGame(viewMsg)
        