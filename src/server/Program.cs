using aleph.server;

var builder = WebApplication.CreateBuilder(args);

// Add services to the container.
builder.Services
    .AddSingleton<IGraphsService>(new GraphsService())
    .AddControllers()
    .AddNewtonsoftJson();

var app = builder.Build();

app.MapControllers();
app.MapGet("/", () => ":ℵ-0.5:");

app.Run();
