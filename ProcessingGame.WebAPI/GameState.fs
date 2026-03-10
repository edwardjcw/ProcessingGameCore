namespace ProcessingGame.WebAPI

open ProcessingGame
open System

[<RequireQualifiedAccess>]
module GameState =

    let private help = "API for ProcessingGame"
    let private initialEnv = EnvironmentBuilder.sampleEnv
    let private startResult = Result.Success initialEnv

    let private gameAgent =
        let agent = MailboxProcessor.Start(GameInterface.play help startResult)
        agent

    let getGameState () =
        let reply = gameAgent.PostAndReply(fun r -> UserRequest.Status, r)
        match reply with
        | Message.Status(_, env) -> env
        | _ -> failwith "Unexpected response from game agent when getting state"

    let moveAdo (adoId: Guid) (processorId: Guid) =
        let env = getGameState ()
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

    let tick (amount: int) =
        let reply = gameAgent.PostAndReply(fun r -> UserRequest.TickAmount amount, r)
        match reply with
        | Message.TickDone newEnv -> newEnv
        | Message.Error (msg, _) -> failwith msg
        | _ -> failwith "Unexpected response from game agent after tick"
