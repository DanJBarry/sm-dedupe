# sm-dedupe

## Installation

See the [releases page](https://github.com/DanJBarry/sm-dedupe/releases) for the latest executables for your operating system.
It's most convenient to place the executable either in your Stepmania/Etterna game folder or a folder in your PATH.

## Usage

Assuming you've placed the executable in C:\Games\Etterna and your songs are located in C:\Games\Etterna\Songs, you would then open up a command line and do something like
```
cd C:\Games\Etterna
.\sm-dedupe.exe Songs
```

sm-dedupe has a number of arguments, which can be viewed with `sm-dedupe --help`:
```
sm-dedupe [OPTIONS] [DIRS]
  A tool to remove duplicates from your Stepmania songs folder

Common flags:
  -e --exclude=DIRS     Directories to exclude
  -f --force            Do not display a prompt before deleting or unlinking
                        a directory
  -d --dry-run          Do a dry run, printing out found duplicates without
                        deleting them
  -u --unlink           Undo symlinks and copy over their target directories
  -? --help             Display help message
  -V --version          Print version information
```

## Building

All you need to build sm-dedupe is [stack](https://docs.haskellstack.org/en/stable/README/).
