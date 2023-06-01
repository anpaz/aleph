#!/bin/bash

# Install .net 6:
curl -sSL https://dot.net/v1/dotnet-install.sh | bash /dev/stdin

# Install azure functions cli
npm i -g azure-functions-core-tools@4 --unsafe-perm true

# assuming Python 3.9+ is already installed.
dotnet --info
echo Azure functions version: `func --version`
python --version