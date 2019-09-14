namespace WhiteDungeon.Core.Game.Model

open wraikny.Tart.Helper.Math

open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model
open WhiteDungeon.Core.Game.Model.Actor

open wraikny.Tart.Helper.Collections
open wraikny.Tart.Advanced
open wraikny.Tart.Core
open wraikny.Tart.Core.Libraries
open wraikny.Tart.Advanced.Dungeon
open wraikny.Tart.Helper.Geometry
open FSharpPlus

type GameSceneMode =
    | HowToControl
    | Stair
    | Pause
    | GameMode
    | WaitingGenerating
    | ErrorUI of exn
    //| GameOver
    | GameFinished of back:bool


type AfterLoseSight =
    | Stop
    | LookAround
    | ChaseLosePoint


type ChaseKind =
    | Losable of AfterLoseSight
    | ChaseTrace of time:float32


type FreeMove =
    | Forward
    | WithRotate of frame : uint16

//module FreeMove =
//    let toContainer = function
//        | Forward -> NoValue
//        | WithRotate x -> WithRotateContainer x


type EnemyInits = {
    kind : int
    lookAngleRadian : float32
}


type OccupationSetting =
    {
        status : ActorStatus
        skill1 : Model -> Actor.Player -> Skill.SkillEmitBuilder list
        skill2 : Model -> Actor.Player -> Skill.SkillEmitBuilder list

        skill1CoolTime : uint16
        skill2CoolTime : uint16
    }


and EnemySetting =
    {
        actorStatus : ActorStatus
        skillCoolTime : uint16
        skill : Model -> Actor.Enemy -> Skill.SkillEmitBuilder list

        visionAngleRate : float32
        visionDistance : float32
        //chaseKind : ChaseKind
        freeMove : FreeMove

        attackDistance : float32
        attackRange : float32

        hateDecrease : float32

        exPoint : int
    }


and GameSetting = {
    dungeonCellSize : float32 Vec2
    minPlayerCount : int
    maxPlayerCount : int
    binarySearchCountMovingOnWall : int

    visionWallCheckCount : uint32

    enemyUpdateDistance : float32
    characterSize : float32 Vec2
    damageCalculation : float32 -> Actor.Actor -> Actor.Actor -> float32
    occupationSettings : HashMap<Occupation, OccupationSetting>
    enemySettings : HashMap<EnemyKind, EnemySetting>

    intToEnemy : (int -> EnemyKind)
}


and Model =
    {
        count : uint32

        nextPlayerID : uint32
        players : Map<PlayerID, Player>

        enemies : Map<EnemyID, Enemy>

        dungeonBuilder: Dungeon.DungeonBuilder
        dungeonModel : Dungeon.DungeonModel
        dungeonGateCells : int Vec2 Set

        skillList : Skill.SkillList

        gameSetting : GameSetting

        timePassed : bool

        mode : GameSceneMode

        lastCollidedGate : bool

        dungeonFloor : uint32
    }
with
    static member SetSkillList (x, s) =
        { x with skillList = s }


module OccupationSetting =
    let inline skillOf kind x =
        kind |> function
        | Actor.Skill1 -> x.skill1CoolTime, x.skill1
        | Actor.Skill2 -> x.skill2CoolTime, x.skill2


module Model =
    let inline count (model : Model) = model.count

    let inline nextPlayerID (model : Model) = model.nextPlayerID
    let inline players (model : Model) = model.players
    
    let inline dungeonBuilder (model : Model) = model.dungeonBuilder
    let inline dungeonModel (model : Model) = model.dungeonModel

    let inline gameSetting (model : Model) = model.gameSetting

    let cellsToEnemies (gameSetting : GameSetting) (enemyCells : (EnemyInits * int Vec2) []) cellSize : Map<_, Enemy> =
        enemyCells
        |> Seq.indexed
        |> Seq.map(fun (index, (ei, cell)) ->
            let enemyId = EnemyID <| uint32 index
            let kind = gameSetting.intToEnemy ei.kind
            let setting = gameSetting.enemySettings |> HashMap.find kind
            enemyId
            , Actor.Enemy.init
                (Vec2.init 100.0f 100.0f)
                ( (DungeonModel.cellToCoordinate cellSize cell) + (cellSize .* 0.5f) )
                enemyId
                1us
                setting.actorStatus
                kind
                ei.lookAngleRadian
                setting.visionDistance
                setting.visionAngleRate
                //(FreeMove.toContainer setting.freeMove)
        )
        |> Map.ofSeq

    let inline init players dungeonBuilder dungeonModel dungeonGateCells enemyCells gameSetting = {
        gameSetting = gameSetting
        count = 0u

        nextPlayerID = players |> Map.count |> uint32
        players = players

        enemies =
            cellsToEnemies
                gameSetting
                enemyCells
                gameSetting.dungeonCellSize

        skillList = Skill.SkillList.init

        dungeonBuilder = dungeonBuilder
        dungeonModel = dungeonModel
        dungeonGateCells = Seq.toList dungeonGateCells |> Set.ofSeq


        timePassed = false

        mode = HowToControl

        lastCollidedGate = false

        dungeonFloor = 1u
    }

module GameSetting =
    open wraikny.Tart.Advanced
    open wraikny.Tart.Helper.Collections

    let inline collidedWithCell (gameSetting) cell =
        Dungeon.DungeonModel.coordinateToCell
            gameSetting.dungeonCellSize
        >> (=) cell
        |> Seq.exists

    let collidedWiithCells (gameSetting : GameSetting) cells =
        Dungeon.DungeonModel.coordinateToCell
            gameSetting.dungeonCellSize
        >> flip Set.contains cells
        |> Seq.exists

    let insideCells (gameSetting : GameSetting) cells =
        Dungeon.DungeonModel.coordinateToCell
            gameSetting.dungeonCellSize
        >> flip HashMap.containsKey cells

    let inline insideDungeon 
        (gameSetting : GameSetting)
        (dungeonModel : Dungeon.DungeonModel) =
        insideCells gameSetting dungeonModel.cells
        |> Seq.forall

    let inline insideDungeonOfLine
        (gameSetting : GameSetting)
        (dungeonModel : Dungeon.DungeonModel)
        count
        (p1) (p2) =

        let count = count - 1u

        [0u..count - 1u]
        |> Seq.map (fun x -> float32 x / float32 count)
        |> Seq.map(fun x -> p1 .* x + p2 .* (1.0f - x))
        |> insideDungeon gameSetting dungeonModel

module Dungeon =

    type GeneratedDungeonParams = {
        dungeonBuilder : DungeonBuilder
        dungeonModel : DungeonModel
        gateCells : int Vec2 Set
        enemyCells : (EnemyInits * int Vec2) []
        initPosition : float32 Vec2
    }

    let generateDungeonModel (dungeonBuilder : DungeonBuilder) =
        Random.int minValue<int> maxValue<int>
        |> TartTask.withEnv(fun seed ->
            let rec loop n = async {
                let builder = { dungeonBuilder with seed = seed + n }
                let dungeon = DungeonBuilder.generate builder

                if length dungeon.largeRooms > 3 then
                    return ( builder, dungeon )
                else
                    return! loop(n + 1)
                }
            loop 0
        )

    let generateDungeonParams gameSetting gateCount dungeonBuilder (dungeonModel : DungeonModel) =
        let largeRooms = toList dungeonModel.largeRooms
        let smallRooms = toList dungeonModel.smallRooms
        
        let largeRoomsCount = length largeRooms
        let smallRoomsCount = length smallRooms

        let fromCell =
            gameSetting.dungeonCellSize
            |> DungeonModel.cellToCoordinate

        let rec loop() = monad {
            try
                //let gen = Random.int minValue<int> maxValue<int>
                let genNatural = Random.int 0 maxValue<int>
                let smallRoomGen = Random.int 0 (smallRoomsCount - 1)
                //let largeRoomGen = Random.int 0 (largeRoomsCount - 1)

                let! largeRoomRnds = Random.list largeRoomsCount genNatural
                let largeRoomValues =
                    largeRoomRnds
                    |> Seq.indexed
                    |> Seq.sortBy snd
                    |> Seq.toList

                let initRoomIndex = fst largeRoomValues.[0]

                let initRoom = snd largeRooms.[initRoomIndex]

                let initPosition =
                    initRoom.rect
                    |>> fromCell
                    |> Rect.centerPosition


                let toLargeRoomCells values =
                    seq {
                        for (i, v) in values ->
                            let room = snd largeRooms.[i]
                            let cells = room |> Space.cells
                            let cell = fst cells.[ v % length cells]
                            cell
                    }

                let gateCells =
                    (toLargeRoomCells largeRoomValues.[1..gateCount])
                    |> Set.ofSeq

                (*
                let itemCells =
                    (getCells largeRoomValues.[(gateCount + 1)..])
                    |> toList

                let! itemRnds = Random.list (length itemCells) genNatural

                let items =
                    Seq.zip itemCells itemRnds
                    |> Seq.toList
                *)


                let! enemyCellIndexs =
                    Random.list smallRoomsCount smallRoomGen

                let! enemyInints =
                    Random.list smallRoomsCount ( monad {
                        let! kind = Random.int 0 maxValue<int>
                        let! angle = Random.double01
                        return {
                            EnemyInits.kind = kind
                            lookAngleRadian = 2.0f * Angle.pi * float32 angle
                        }
                    })

                let enemyCells =
                    seq {
                        for (index, (_, space)) in Seq.indexed smallRooms ->
                            let cells = space |> Space.cells
                            let cellIndex = (enemyCellIndexs : int list).[index]
                            let cell = fst cells.[ cellIndex % length cells]
                            let ei = (enemyInints : _ list).[index]
                            (ei, cell)
                    }
                    |> Seq.toArray

                return {
                    dungeonBuilder = dungeonBuilder
                    dungeonModel = dungeonModel
                    gateCells = gateCells
                    enemyCells = enemyCells
                    initPosition = initPosition
                }
            with e ->
                printfn "%A" e
                return! loop()
        }

        flip Random.generate (loop())
