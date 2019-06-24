namespace WhiteDungeon.View.UI

open wraikny.MilleFeuille.Core.UI
open wraikny.MilleFeuille.Fs.Input.Controller

module Keybaord =
    let createUIKeyboard() =
        KeyboardBuilder.init()
        |> KeyboardBuilder.bindKeysList
            [
                Button.ControllerSelect.Up    , asd.Keys.W
                Button.ControllerSelect.Down  , asd.Keys.S
                Button.ControllerSelect.Right , asd.Keys.D
                Button.ControllerSelect.Left  , asd.Keys.A
                Button.ControllerSelect.Select, asd.Keys.Space
                Button.ControllerSelect.Cancel, asd.Keys.Escape
            ]
        |> KeyboardBuilder.build