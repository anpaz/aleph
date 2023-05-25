using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.DependencyInjection;

using aleph.server;

var host = new HostBuilder()
    .ConfigureFunctionsWorkerDefaults()
    .ConfigureServices(s =>
    {
        s.AddSingleton<IGraphsService, GraphsService>();
    })
    .Build();

host.Run();
