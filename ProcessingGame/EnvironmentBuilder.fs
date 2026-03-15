namespace ProcessingGame

[<RequireQualifiedAccess>]
module EnvironmentBuilder =
    let private createProcessors count power =
        (Seq.initInfinite (fun _ -> Game.emptyProcessor 10 power))
        |> Seq.take count
        |> Seq.map (fun p -> (p.id, p))
        |> Map.ofSeq

    let private createPrograms count ados =
        (Seq.initInfinite (fun _ -> Game.emptyProgram()))
        |> Seq.take count
        |> Seq.map (
            (fun p -> 
                let rec addAdo p' count =
                    if count = 0 then p'
                    else addAdo (Game.programWithAdo (System.Random().Next(1, 5)) p') (count - 1)
                addAdo p ados)
            >> (fun p -> p.id, p))
        |> Map.ofSeq

    let newEnv programs processors ados =
        { Game.emptyEnvironment with
            programs = createPrograms programs ados
            processors = createProcessors processors 1 }
        
    let sampleEnv = newEnv 3 5 3