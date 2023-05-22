namespace aleph.server

#nowarn "20"

open System

open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting

module Program =
    let exitCode = 0

    [<EntryPoint>]
    let main args =

        let builder = WebApplication.CreateBuilder(args)

        builder.Services
            .AddSingleton<IGraphsService>(new GraphsService())
            .AddControllers()

        let app = builder.Build()
        app.MapControllers()
        app.MapGet("/", Func<string>(fun (x) -> ":ℵ-0.9:"));

        app.Run()

        exitCode
