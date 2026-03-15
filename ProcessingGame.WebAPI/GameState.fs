namespace ProcessingGame.WebAPI

open ProcessingGame
open System

type GameConfig = {
    programs: int
    processors: int
    ados: int
}

type GameStateEngine() =

    let help = "API for ProcessingGame"
    let initialEnv = EnvironmentBuilder.sampleEnv
    let startResult = Result.Success initialEnv

    let gameAgent = MailboxProcessor.Start(GameInterface.play help startResult)

    member this.NewGame(config: GameConfig) =
        let reply = gameAgent.PostAndReply(fun r -> UserRequest.NewGame(config.programs, config.processors, config.ados), r)
        match reply with
        | Message.NewGameDone newEnv -> newEnv
        | _ -> failwith "Unexpected response from game agent after new game"

    member this.GetGameState() =
        let reply = gameAgent.PostAndReply(fun r -> UserRequest.Status, r)
        match reply with
        | Message.Status(_, env) -> env
        | _ -> failwith "Unexpected response from game agent when getting state"

    member this.MoveAdo(adoId: Guid, processorId: Guid) =
        let env = this.GetGameState()
        let adoOpt = GameInterface.readyAdoFromId (adoId.ToString()) env
        let processorOpt = GameInterface.processorFromId (processorId.ToString()) env

        match adoOpt, processorOpt with
        | Some ado, Some processor ->
            let reply = gameAgent.PostAndReply(fun r -> UserRequest.MoveUsing(ado, processor), r)
            match reply with
            | Message.MoveComplete newEnv -> newEnv
            | Message.Error (msg, _) -> failwith msg
            | _ -> failwith "Unexpected response from game agent after move"
        | None, _ -> failwith $"Ado with id {adoId} not found or not ready."
        | _, None -> failwith $"Processor with id {processorId} not found."

    member this.Tick(amount: int) =
        let reply = gameAgent.PostAndReply(fun r -> UserRequest.TickAmount amount, r)
        match reply with
        | Message.TickDone newEnv -> newEnv
        | Message.Error (msg, _) -> failwith msg
        | _ -> failwith "Unexpected response from game agent after tick"
