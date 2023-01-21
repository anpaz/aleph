

# Aleph

## Setup

1. Install [.NET Core](https://dotnet.microsoft.com/en-us/download) 6 (for compiler, Q#) and 7 (for server)
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