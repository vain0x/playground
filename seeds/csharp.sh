#!/bin/sh

set -eu ;

match () {
    grep -Eq "^$1$" ;
} ;

# Parse arguments.

if [ $# -ne 2 ] ; then
    echo "USAGE:" ;
    echo "$ ./seeds/csharp.sh <slug> <namespace>" ;
    echo "Example:" ;
    echo "$ ./seeds/csharp.sh awesome-app John.AwesomeApp" ;
    exit 1 ;
fi ;

date="$(date --iso-8601)" ;
slug="$1" ;
namespace="$2" ;

if ! { echo "$slug" | match "[a-z][a-z0-9-]*" ] ; } ; then
    echo "Invalid slug." ;
    exit 1 ;
fi ;

if ! { echo "$namespace" | match "[A-Z][a-zA-Z0-9_.]*" ; } ; then
    echo "Invalid project name." ;
    exit 1 ;
fi ;

if ! [ -d ./play/ ] ; then
    echo "Run in playground dir." ;
    exit 1 ;
fi ;

# Ensure work tree is clean.

if ! [ -z "$(git clean -n)" ] ; then
    echo "Work tree must be clean." ;
    exit 1 ;
fi ;

# Create directory for the solution.

rel_path="./play/$date-$slug" ;

if [ -d $rel_path ] ; then
    echo "Already exists: $rel_path" ;
    exit 1 ;
fi ;

mkdir -p $rel_path ;
cd $rel_path ;

# Generate dot files.

gitignore () {
    curl -s -L "https://www.gitignore.io/api/$1" ;
} ;

gitignore "visualstudio,csharp" > .gitignore ;
echo "* text=auto" > .gitattributes ;

git add . ;
git commit -m "Create '$slug' project" ;

# Create solution and projects.

mkdir src ;
cd src ;

dotnet new sln -n $slug ;
dotnet new console -n $namespace ;
dotnet new xunit -n $namespace.Tests ;

git add . ;
git commit -m "Generate solution and projects" ;

# Add projects to solution.

dotnet sln add $namespace/$namespace.csproj ;
dotnet sln add $namespace.Tests/$namespace.Tests.csproj ;
dotnet add $namespace.Tests reference $namespace/$namespace.csproj ;

git add . ;
git commit -m "Add project references" ;

# Clean up.

dotnet test $namespace.Tests ;
