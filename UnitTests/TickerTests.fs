namespace UnitTests

open NUnit.Framework
open ProcessingGame
open ProcessingGame.WebAPI

[<TestFixture>]
type TickerTests () =

    [<Test>]
    member this.TestTickAmount () =
        let engine = GameStateEngine()
        let initialEnv = engine.NewGame { programs = 1; processors = 1; ados = 1 }
        let newEnv = engine.Tick 1
        Assert.That(newEnv.ticks, Is.EqualTo(initialEnv.ticks + 1))
