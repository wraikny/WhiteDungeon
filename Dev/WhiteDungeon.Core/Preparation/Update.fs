module WhiteDungeon.Core.Preparation.Update

open wraikny.Tart.Core

open WhiteDungeon.Core


let incrPlayer (model : Model) : Model * Cmd<Msg, _> =
    let playerCount =
        (model.playerCount + 1)
        |> min model.gameSetting.maxPlayerCount

    if model.playerCount < playerCount then
        { model with
            playerCount = playerCount
            players =
                let id = uint32 playerCount

                model.players
                |> Map.add id None
        }, Cmd.none
    else
        model, Cmd.none


let decrPlayer (model : Model) : Model * Cmd<Msg, _> =
    let playerCount =
        (model.playerCount - 1)
        |> max model.gameSetting.minPlayerCount
        |> max 0

    { model with
        playerCount = playerCount
    }, Cmd.none


let update (msg : Msg) (model : Model) : Model * Cmd<Msg, ViewMsg> =
    model.mode |> function
    | WaitingDungeonGenerating ->
        model, Cmd.none

    | Default ->
        msg |> function
        | IncrPlayer ->
            incrPlayer model

        | DecrPlayer ->
            decrPlayer model

        | SelectCharacter(index, character) ->
            character |> function
            | None -> model, Cmd.none
            | Some(chara) ->
                if model.savedData.charactersList |> Map.containsKey chara then
                    { model with
                        players =
                            model.players
                            |> Map.add index character
                    }, Cmd.none
                else
                    model, Cmd.none

        | SetRandomRoomIndex index ->
            { model with randomRoomIndex = index }, Cmd.none