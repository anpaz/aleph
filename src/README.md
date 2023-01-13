

# Aleph

## Setup

1. [Install F#](https://docs.microsoft.com/en-us/dotnet/fsharp/get-started/install-fsharp). It comes [bundle with .Net 6](https://dotnet.microsoft.com/en-us/download).
2. Run the tests
```
cd tests
dotnet test
```
3. Run the sample projects:
```
cd samples
dotnet run
```

## Debug tests

1. Set env variable: VSTEST_HOST_DEBUG="1"
2. Start dotnet test: dotnet test --filter TestQPUClassic
   > It starts and breaks. Print the process id to the console.
3. Attach to process.