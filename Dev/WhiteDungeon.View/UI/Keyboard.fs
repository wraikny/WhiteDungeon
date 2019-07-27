namespace WhiteDungeon.View.UI

open wraikny.MilleFeuille.Core.UI
open wraikny.MilleFeuille.Fs.Input

module Keybaord =
    let inline createUIKeyboard() =
        KeyboardBuilder.init()
        |> KeyboardBuilder.bindKeysList
            [|
                ControllerSelect.Up    , asd.Keys.W
                ControllerSelect.Down  , asd.Keys.S
                ControllerSelect.Right , asd.Keys.D
                ControllerSelect.Left  , asd.Keys.A
                ControllerSelect.Select, asd.Keys.Space
                ControllerSelect.Cancel, asd.Keys.Escape
            |]
        |> KeyboardBuilder.build