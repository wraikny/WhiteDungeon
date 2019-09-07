module WhiteDungeon.Core.Game.Update.Actor.Enemy

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Collections
open wraikny.Tart.Advanced.Dungeon
open WhiteDungeon.Core.Game
open WhiteDungeon.Core.Game.Model
open WhiteDungeon.Core.Game.Model.Actor
open WhiteDungeon.Core.Game.Update
open WhiteDungeon.Core.Game.Update.Actor

let freeMove (gameSetting : GameSetting) (dungeonModel : DungeonModel) enemy : Enemy =
    let setting = gameSetting.enemySettings |> HashMap.find enemy.kind
    let status =
        enemy |> Actor.statusCurrent

    let dir = Vec2.fromAngle enemy.lookAngleRadian
    
    setting.freeMove |> function
    | FreeMove.Forward ->
        let enemy, newDirection =
            enemy |>
                ObjectBase.moveReflectable
                    gameSetting
                    dungeonModel
                    (dir .* status.walkSpeed)

        newDirection |> function
        | None -> enemy
        | Some dir ->
            let angle = Vec2.angle dir
            { enemy with lookAngleRadian = angle }


let inline move (model : Model) enemy : Enemy =
    enemy.mode |> function
    | EnemyMode.FreeMoving ->
        freeMove model.gameSetting model.dungeonModel enemy
    | EnemyMode.Chasing target ->
        enemy
    | EnemyMode.AfterChasing target ->
        enemy

let inline update model enemy : Enemy =
    enemy
    |> Actor.update
    |> move model
