module WhiteDungeon.Core.Game.Update.Actor.Enemy

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Collections
open wraikny.Tart.Advanced.Dungeon
open WhiteDungeon.Core.Game
open WhiteDungeon.Core.Game.Model
open WhiteDungeon.Core.Game.Model.Actor
open WhiteDungeon.Core.Game.Update
open WhiteDungeon.Core.Game.Update.Actor

open FSharpPlus

let freeMove (gameSetting : GameSetting) (dungeonModel : DungeonModel) enemy : Enemy =
    let setting = gameSetting.enemySettings |> HashMap.find enemy.kind
    let status =
        enemy |> Actor.statusCurrent

    let dir = Vec2.fromAngle enemy.lookingRadian
    
    setting.freeMove |> function
    | FreeMove.Forward ->
        let enemy, newDirection =
            enemy
            |> ObjectBase.moveReflectable
                gameSetting
                dungeonModel
                (dir .* status.walkSpeed)

        newDirection |> function
        | None -> enemy
        | Some dir ->
            let angle = Vec2.angle dir
            { enemy with lookingRadian = angle }


let chasingMove (model : Model) (pos) (enemy : Enemy) : Enemy * _ =
    let setting =
        model.gameSetting.enemySettings
        |> HashMap.find enemy.kind
    let atkDist = setting.attackDistance

    let dist = Vector.length(ObjectBase.position enemy - pos)

    let speed =
        (Actor.statusCurrent enemy).dashSpeed
    let diff = 
        if dist > atkDist then
            speed |> min (dist - atkDist)
        elif dist = atkDist then
            0.0f
        else
            -(speed |> min (atkDist - dist))

    enemy
    |> ObjectBase.moveXYAnother
        model.gameSetting
        model.dungeonModel
        (diff *. Enemy.lookingDirection enemy)
    |> fst
    , diff


let move (model : Model) enemy : Enemy =
    enemy.mode |> function
    | EnemyMode.FreeMoving ->
        freeMove model.gameSetting model.dungeonModel enemy
    | EnemyMode.Chasing (id, pos) ->
        enemy
        |> chasingMove model pos
        |> fst
    | EnemyMode.AfterChasing pos ->
        // TODO
        enemy
        |> chasingMove model pos
        |> fun (x, diff) ->
            if abs(diff) < 0.1f then
                { enemy with mode = FreeMoving }
            else
                x
            
        


let tryGetTarget (targets : seq<Player>) (enemy : Enemy) : Player option =
    targets
    |> Seq.sortBy(fun x ->
        enemy.hateMap
        |> Map.tryFind x.id
        |> Option.defaultValue 0.0f
        |> ( * ) -1.0f
    )
    |> Seq.tryFind(fun x -> Enemy.insideVision enemy (ObjectBase.position x))


let onApplyAreaSkill (areaSkill : Skill.AreaSkill) (enemy : Enemy) : Enemy =
    enemy.mode |> function
    | EnemyMode.FreeMoving ->
        let invoker = areaSkill.skillBase.invokerActor
        let dir =ObjectBase.calcAngle enemy invoker
        { enemy with lookingRadian = dir }
    | _ ->
        enemy

    


let updateMode (model : Model) (enemy : Enemy) : Enemy =
    let players = model.players |> Map.toSeq |> Seq.map snd |> Seq.toList

    let trySetTarget enemy =
        tryGetTarget players enemy
        |>> flip Enemy.setTarget enemy

    enemy.mode |> function
    | EnemyMode.FreeMoving ->
        enemy
        |> trySetTarget
        |> Option.defaultValue enemy
    | EnemyMode.Chasing (id, pos) ->
        let player = model.players |> Map.find id
        if Enemy.insideVision enemy (ObjectBase.position player) then
            enemy |> Enemy.setTarget player
        else
            enemy
            |> trySetTarget
            |> Option.defaultWith(fun() ->
                { enemy with mode = AfterChasing(pos) }
            )

    | EnemyMode.AfterChasing _ ->
        enemy
        |> trySetTarget
        |> Option.defaultValue enemy





let inline update model enemy : Enemy =
    enemy
    |> Actor.update
    |> move model
    |> updateMode model
