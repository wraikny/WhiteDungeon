namespace WhiteDungeon.Core.Game.Model

open wraikny.Tart.Advanced.Dungeon


type Model = {
    count : uint32
    dungeonModel : DungeonModel
}


module Model =
    let count m = m.count

    let init (dungeonModel) =
        {
            count = 0u
            dungeonModel = dungeonModel
        }