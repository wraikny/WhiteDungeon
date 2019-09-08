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
            

let insideVision (gameSetting) (dungeonModel) (enemy : Enemy) (point : float32 Vec2) : bool =
    let pos = enemy |> ObjectBase.position

    (
        let dist = (pos - point) |> Vector.squaredLength
        dist < enemy.visionDistance * enemy.visionDistance
    )
    &&
    (
        let angle = (point - pos) |> Vec2.angle

        let d =
            (angle - enemy.lookingRadian)
            |> Vec2.fromAngle
            |> Vec2.angle
        (d * d < enemy.visionAngle * enemy.visionAngle / 4.0f)
    )
    &&
    (
        GameSetting.insideDungeonOfLine
            gameSetting
            dungeonModel
            gameSetting.visionWallCheckCount
            pos
            point
    )
        


let tryGetTarget (model : Model) (targets : seq<Player>) (enemy : Enemy) : Player option =
    targets
    |> Seq.sortBy(fun x ->
        enemy.hateMap
        |> Map.tryFind x.id
        |> Option.defaultValue 0.0f
        |> ( * ) -1.0f
    )
    |> Seq.tryFind(fun x ->
        insideVision
            model.gameSetting
            model.dungeonModel
            enemy
            (ObjectBase.position x)
    )


let onApplyAreaSkill
    (areaSkill : Skill.AreaSkill)
    (prevEnemy : Enemy) (enemy : Enemy) : Enemy =
    areaSkill.skillBase.invokerActor.id |> function
    | ActorID.OfEnemyID _ ->
        enemy

    | ActorID.OfPlayerID id ->
        let statusDefault = Actor.statusDefault enemy
        let status = Actor.statusCurrent enemy
        let hpDiff =
            (Actor.statusCurrent prevEnemy).hp
            - status.hp

        enemy
        |> Enemy.addHate id (hpDiff / statusDefault.hp)

    |> fun enemy ->
        enemy.mode |> function
        | EnemyMode.FreeMoving ->
            let invoker = areaSkill.skillBase.invokerActor
            let dir = ObjectBase.calcAngle enemy invoker
            { enemy with lookingRadian = dir }
        | _ ->
            enemy

    


let updateMode (model : Model) (enemy : Enemy) : Enemy =
    let players = model.players |> Map.toSeq |> Seq.map snd |> Seq.toList

    let trySetTarget enemy =
        tryGetTarget model players enemy
        |>> flip Enemy.setTarget enemy

    enemy.mode |> function
    | EnemyMode.FreeMoving ->
        enemy
        |> trySetTarget
        |> Option.defaultValue enemy
    | EnemyMode.Chasing (id, pos) ->
        let player = model.players |> Map.find id
        let inVision =
            insideVision
                model.gameSetting
                model.dungeonModel
                enemy (ObjectBase.position player)
        if inVision then
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
