# API server

This projects exposes `aleph`'s runtime as an Azure Function.

Via this server-less environment, it provides REST APIs to create and modify a quantum graph, and to call `prepare` and `sample` on it.

## Running

1. **Install pre-reqs:** In Mac/Linux, use [../bootstrap.sh](../bootstrap.sh) to install pre-reqs. In Windows manually install:
    * [.NET 6.0](https://dotnet.microsoft.com/en-us/download)
    * [Azure Functions Core Tools 4.0](https://learn.microsoft.com/en-us/azure/azure-functions/functions-run-local?#install-the-azure-functions-core-tools)
    
2. Once pre-reqs have been installed, run `func start` from this folder to start a local instance of the API. If successful you should see all the available functions in the terminal:

```
Functions:

        ConstantBool: [GET,POST] http://localhost:7071/graph/{graphId}/~bool

        ConstantInt: [GET,POST] http://localhost:7071/graph/{graphId}/~int

        Create: [GET,POST] http://localhost:7071/graph/~create

        GetGraph: [GET,POST] http://localhost:7071/graph/{graphId}

        GetNode: [GET,POST] http://localhost:7071/graph/{graphId}/node/{ketId}

        Histogram: [GET,POST] http://localhost:7071/graph/{graphId}/~histogram

        Literal: [GET,POST] http://localhost:7071/graph/{graphId}/~literal

        MapId: [GET,POST] http://localhost:7071/graph/{graphId}/~map/id

        MapIf: [GET,POST] http://localhost:7071/graph/{graphId}/~map/if

        MapNot: [GET,POST] http://localhost:7071/graph/{graphId}/~map/not

        MapOther: [GET,POST] http://localhost:7071/graph/{graphId}/~map/{op}

        Prepare: [GET,POST] http://localhost:7071/graph/{graphId}/~prepare

        Sample: [GET,POST] http://localhost:7071/graph/{graphId}/~sample

        Where: [GET,POST] http://localhost:7071/graph/{graphId}/~where
```

## Using local version from Python

Once the server has started set the `ALEPH_BASE_URL` environment variable to point your API requests to this instance:

```bash
export ALEPH_BASE_URL=http://localhost:7071/
```


## Deployment

Using Azure Functions CLI tools:

```bash
func azure functionapp publish aleph-01  --slot ci  --dotnet-cli-params " -r win-x64 /p:PublishReadyToRun=true" 
```