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

    let gameState = new GameStateEngine()

    [<HttpGet>]
    member _.Get() : IActionResult =
        this.Ok(gameState.GetGameState()) :> IActionResult

    [<HttpPost("move")>]
    member _.PostMove([<FromBody>] moveReq: MoveRequest) : IActionResult =
        try
            let newEnv = gameState.MoveAdo(moveReq.adoId, moveReq.processorId)
            this.Ok(newEnv) :> IActionResult
        with
        | ex -> this.BadRequest(ex.Message) :> IActionResult

    [<HttpPost("tick")>]
    member _.PostTick([<FromBody>] amount: int) : IActionResult =
        try
            if amount <= 0 then
                failwith "Tick amount must be positive"
            let newEnv = gameState.Tick(amount)
            this.Ok(newEnv) :> IActionResult
        with
        | ex -> this.BadRequest(ex.Message) :> IActionResult

    [<HttpPost("new")>]
    member _.PostNewGame([<FromBody>] config: GameConfig) : IActionResult =
        try
            let newEnv = gameState.NewGame(config)
            this.Ok(newEnv) :> IActionResult
        with
        | ex -> this.BadRequest(ex.Message) :> IActionResult
