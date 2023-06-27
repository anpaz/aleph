#!/bin/bash

# Install .net 6:
curl -sSL https://dot.net/v1/dotnet-install.sh | bash /dev/stdin
dotnet --info

# Install azure functions cli
npm i -g azure-functions-core-tools@4 --unsafe-perm true

# assuming Python 3.9+ is already installed.

# Print all versions
echo 
echo 
echo ----------------------------------------------------------------
echo .NET versions: 
dotnet --list-sdks
echo
echo Azure functions version: `func --version`
echo 
python --version
echo ----------------------------------------------------------------
