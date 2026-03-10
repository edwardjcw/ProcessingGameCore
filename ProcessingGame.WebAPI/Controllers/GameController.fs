namespace ProcessingGame.WebAPI.Controllers

open System
open Microsoft.AspNetCore.Mvc
open ProcessingGame
open ProcessingGame.WebAPI

type MoveRequest = {
    adoId: Guid
    processorId: Guid
}

[<ApiController>]
[<Route("api/game")>]
type GameController () as this =
    inherit ControllerBase()

    [<HttpGet>]
    member _.Get() : IActionResult =
        this.Ok(GameState.getGameState()) :> IActionResult

    [<HttpPost("move")>]
    member _.PostMove([<FromBody>] moveReq: MoveRequest) : IActionResult =
        try
            let newEnv = GameState.moveAdo moveReq.adoId moveReq.processorId
            this.Ok(newEnv) :> IActionResult
        with
        | ex -> this.BadRequest(ex.Message) :> IActionResult

    [<HttpPost("tick")>]
    member _.PostTick([<FromBody>] amount: int) : IActionResult =
        try
            if amount <= 0 then
                failwith "Tick amount must be positive"
            let newEnv = GameState.tick amount
            this.Ok(newEnv) :> IActionResult
        with
        | ex -> this.BadRequest(ex.Message) :> IActionResult
