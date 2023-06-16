
Source code

# Pre-reqs

1. Install dependencies. If using codespaces you may simply run **./bootstrap.sh**. Otherwise this should suffice:
   - [.NET Core 6](https://dotnet.microsoft.com/en-us/download): for framework, Q# & API functions.
   - [Python 3.10](https://www.python.org/downloads/), for Python bindings.


# .NET

`aleph` is built on .NET 6, mostly on F#. Once pre-requs are built, just build and test the solution file (src/aleph.sln).

```bash
cd src
dotnet build
dotnet test
```

# Python

We provide Python bindings for `aleph`. Use `pip` to install the source version. Include `pytest` for tests

```bash
cd src/python
pip install -e . pytest
pytest
```

# API server

The Python bindings require an API server to keep track of the quantum graph and evaluate it. Information about how to build and run can be found in the API server folder: [./api/README.md](./api/README.md)

# Release Binaries

1. Update package versions in:
      * **.NET**: [Common.props](./Common.props)
      * **Python**: [python/setup.py](./python/setup.py)

2. Merge changes into main. Build from main.

3. Build .NET packages:
   ```bash
   dotnet pack src/aleph.sln -c Release -o bin
   ```
   Packages will be in the `bin` folder. Upload all of them to NuGet (via the NuGet portal).

4. Build and publish Python package:
   ```bash
   cd src/python
   pip install build twine
   python -m build
   twine upload dist/*
   ```

# Other

## Debug F# tests

1. Set env variable: VSTEST_HOST_DEBUG="1"
2. Start dotnet test: dotnet test --filter TestQPUClassic
   > It starts and breaks. Print the process id to the console.
3. Attach to process.


