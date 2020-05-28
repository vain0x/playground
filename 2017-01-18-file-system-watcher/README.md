# FileSystemWatcher
## Install
0. Install .NET Core (>= 1.0).
0. Build the solution.
0. Run ``dotnet VainZero.FileSystemWatcher.dll``.

## Usage
### Standard inputs
The program reads standard inputs. Reading a line which matches with the following,

```
Add|path-to-directory
```

it starts to watch the directory (if exists).

And if the line matches the following:

```
Remove|path-to-directory
```

it stops to watch the directory (if watching).

### Standard outputs
When a file or a directory in a watched directory changed, the program emits a line, which matches with one of those:

```
Changed|path
Created|path
Deleted|path
Renamed|old-path|new-path
```

The head word describes what happened.

## LICENSE
MIT License
