namespace WhiteDungeon.View.UI

open wraikny.MilleFeuille.Core.UI
open wraikny.MilleFeuille.Fs.Input.Controller

module Keybaord =
    let createUIKeyboard() =
        KeyboardBuilder.init()
        |> KeyboardBuilder.bindKeysList
            [
                Button.ControllerSelect.Up    , asd.Keys.Up
                Button.ControllerSelect.Down  , asd.Keys.Down
                Button.ControllerSelect.Right , asd.Keys.Right
                Button.ControllerSelect.Left  , asd.Keys.Left
                Button.ControllerSelect.Select, asd.Keys.Z
                Button.ControllerSelect.Cancel, asd.Keys.Escape
            ]
        |> KeyboardBuilder.build