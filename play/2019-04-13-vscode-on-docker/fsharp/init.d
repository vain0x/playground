#!/bin/bash

dotnet new console -lang F# --name cli
mv cli.vscode cli/.vscode

code cli --wait
