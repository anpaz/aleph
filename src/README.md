# Aleph src

## Setup

1. Install dependencies. If using codespaces you may simply run **./bootstrap.sh**. Otherwise this should suffice:
   - [.NET Core 6](https://dotnet.microsoft.com/en-us/download): for parser & Q#
   - [.NET Core 7](https://dotnet.microsoft.com/en-us/download): for server.
   - [Python 3.10](https://www.python.org/downloads/), for Python package

1. Run the tests, to verify setup.
```bash
cd tests
dotnet test
```

1. Run the sample projects:
```bash
cd samples
dotnet run
```

## Debug F# tests

1. Set env variable: VSTEST_HOST_DEBUG="1"
2. Start dotnet test: dotnet test --filter TestQPUClassic
   > It starts and breaks. Print the process id to the console.
3. Attach to process.
