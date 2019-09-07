namespace WhiteDungeon.View.Utils

type ErrorHandler() =
    member val CallBack : exn -> unit = ignore with get, set

    member x.Clear() = x.CallBack <- ignore