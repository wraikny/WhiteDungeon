﻿namespace WhiteDungeon.Core.Game.Model

open wraikny.Tart.Helper.Math

open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model

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


type Model = {
    count : uint32

    nextPlayerID : uint32
    players : Map<PlayerID, Actor.Player>

    enemies : Map<EnemyID, Actor.Enemy>

    dungeonBuilder: Dungeon.DungeonBuilder
    dungeonModel : Dungeon.DungeonModel
    dungeonGateCells : int Vec2 Set

    skillList : Skill.SkillList

    gameSetting : Model GameSetting_

    timePassed : bool

    mode : GameSceneMode

    lastCollidedGate : bool

    dungeonFloor : uint32
}

module Model =
    let inline count (model : Model) = model.count

    let inline nextPlayerID (model : Model) = model.nextPlayerID
    let inline players (model : Model) = model.players
    
    let inline dungeonBuilder (model : Model) = model.dungeonBuilder
    let inline dungeonModel (model : Model) = model.dungeonModel

    let inline gameSetting (model : Model) = model.gameSetting

    let cellsToEnemies (enemyCells : (int * int Vec2) []) cellSize =
        enemyCells
        |> Seq.indexed
        |> Seq.map(fun (index, (i, cell)) ->
            let enemyId = EnemyID <| uint32 index
            enemyId
            , Actor.Enemy.init
                (Vec2.init 100.0f 100.0f)
                ( (DungeonModel.cellToCoordinate cellSize cell) + (cellSize .* 0.5f) )
                enemyId
                {   ActorStatus.level = 1
                    hp = 100.0f
                    walkSpeed = 10.0f
                    dashSpeed = 16.0f
                }
        )
        |> Map.ofSeq

    let inline init players dungeonBuilder dungeonModel dungeonGateCells enemyCells gameSetting = {
        gameSetting = gameSetting
        count = 0u

        nextPlayerID = players |> Map.count |> uint32
        players = players

        enemies = cellsToEnemies enemyCells gameSetting.dungeonCellSize

        skillList = Skill.SkillList.init()

        dungeonBuilder = dungeonBuilder
        dungeonModel = dungeonModel
        dungeonGateCells = Seq.toList dungeonGateCells |> Set.ofSeq


        timePassed = false

        mode = HowToControl

        lastCollidedGate = false

        dungeonFloor = 1u
    }

type GameSetting = Model GameSetting_

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
        |> Seq.forall

    let inline insideDungeon 
        (gameSetting : GameSetting)
        (dungeonModel : Dungeon.DungeonModel) =
        insideCells gameSetting dungeonModel.cells

module Dungeon =
    type GeneratedDungeonParams = {
        dungeonBuilder : DungeonBuilder
        dungeonModel : DungeonModel
        gateCells : int Vec2 Set
        enemyCells : (int * int Vec2) []
        initPosition : float32 Vec2
    }

    let generateDungeonModel (dungeonBuilder : DungeonBuilder) =
        Random.int minValue<int> maxValue<int>
        |> TartTask.withEnv(fun seed ->
            let rec loop n = async {
                let builder = { dungeonBuilder with seed = seed + n }
                let dungeon = DungeonBuilder.generate builder
                if length dungeon.largeRooms > 2 then
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
                let gen = Random.int minValue<int> maxValue<int>
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


                let! enemyCellIndexs=
                    Random.list smallRoomsCount smallRoomGen

                let enemyCells =
                    seq {
                        for (index, (_, space)) in Seq.indexed smallRooms ->
                            let cells = space |> Space.cells
                            let cellIndex = (enemyCellIndexs : int list).[index]
                            let cell = fst cells.[ cellIndex % length cells]
                            (cellIndex, cell)
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
