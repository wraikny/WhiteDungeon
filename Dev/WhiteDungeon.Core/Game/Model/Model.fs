namespace WhiteDungeon.Core.Game.Model

open WhiteDungeon.Core.Game.Model

open wraikny.Tart.Advanced



type Model = {
    count : uint32

    players : (Actor.PlayerID * Actor.Player) list

    dungeonModel : Dungeon.DungeonModel

    gameSetting : GameSetting
}


module Model =
    let count m = m.count

    let dungeonModel m = m.dungeonModel

    let init (players, dungeonModel, gameSetting) = {
        count = 0u

        players = players

        dungeonModel = dungeonModel

        gameSetting = gameSetting
    }