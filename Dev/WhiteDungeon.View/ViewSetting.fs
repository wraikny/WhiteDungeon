namespace WhiteDungeon.View

type ViewSetting = {
    uiFontPath : string

    menuButtonSize : asd.Vector2DF
    menuButtonFontSize : int

    messageFontSize : int

    titleTextFontSize : int
    titleText : string

    button1 : asd.Vector2DF
    button2 : asd.Vector2DF
    button3 : asd.Vector2DF
}

module ColorPalette =
    let ume = new asd.Color(234uy, 173uy, 189uy)

    let sumire = new asd.Color(85uy, 69uy, 98uy)

    let sakura = new asd.Color(250uy, 219uy, 224uy)