namespace WhiteDungeon.View.UI

open WhiteDungeon.View
open WhiteDungeon.View.Utils.Color
// open wraikny.MilleFeuille.Core.Object

type MessageLayer(viewSetting) =
    inherit asd.Layer2D()

    let viewSetting : ViewSetting = viewSetting

    let messageFont =
        asd.Engine.Graphics.CreateDynamicFont(
            viewSetting.uiFontPath
            , viewSetting.messageFontSize
            , ColorPalette.sumire
            , 0
            , ColorPalette.sumire
        )

    let messageTextPosition = asd.Engine.WindowSize.To2DF() / 2.0f
    let messageText = new asd.TextObject2D(Text = "", Font = messageFont)

    member this.MessageText
        with get() = messageText.Text
        and  set(value) =
            messageText.Text <- value
            let size =
                messageFont.CalcTextureSize(value, asd.WritingDirection.Horizontal).To2DF()

            messageText.Position <- messageTextPosition - size / 2.0f

    override this.OnAdded() =
        this.AddObject(messageText)