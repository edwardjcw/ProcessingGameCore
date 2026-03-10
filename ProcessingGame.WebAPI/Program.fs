namespace ProcessingGame.WebAPI
#nowarn "20"
open System
open System.Collections.Generic
open System.IO
open System.Linq
open System.Threading.Tasks
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.HttpsPolicy
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging

module Program =
    let exitCode = 0
    let MyAllowSpecificOrigins = "_myAllowSpecificOrigins"

    [<EntryPoint>]
    let main args =

        let builder = WebApplication.CreateBuilder(args)

        builder.Services.AddCors(fun options ->
            options.AddPolicy(MyAllowSpecificOrigins, fun b ->
                b.WithOrigins("http://localhost:5173")
                    .AllowAnyHeader()
                    .AllowAnyMethod() |> ignore
            )
        )

        builder.Services.AddControllers().AddNewtonsoftJson() |> ignore

        let app = builder.Build()

        app.UseHttpsRedirection()

        app.UseCors(MyAllowSpecificOrigins)

        app.UseAuthorization()
        app.MapControllers()

        app.Run()

        exitCode
