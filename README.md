# compiler-text

[『コンパイラ ー 原理と構造』](https://www.kyoritsu-pub.co.jp/bookdetail/9784320124783)のF#への翻訳。

## Requirements

- dotnet 5 or later

## Usage

### Turing Machine Emulator

```bash
$ dotnet run -p ./src/TM
```

### CoreML Processor

```bash
$ dotnet run -p ./src/CoreML -m [c|i] -l [path/to/CoreMl/file]
```

```
options:
  -m, --mode={c|i}                  Compile or Interpret (default mode: Compile).
  -l, --load-file=<STRING_VALUE>    Path to a CoreML file. Without this option, you'll be into interactive mode.
```
