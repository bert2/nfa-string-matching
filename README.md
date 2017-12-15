# Glob Pattern Matching using Nondeterministic Finite Automata

This is an implementation of a nondeterministic finite automaton (NFA) that matches text input against a [glob pattern](https://en.wikipedia.org/wiki/Glob_(programming)). The implementation is based on [Ken Thompson's NFA construction from regular expressions](https://en.wikipedia.org/wiki/Glob_(programming)) and inspired by Russ Cox' article [Regular Expression Matching Can Be Simple And Fast ](https://swtch.com/~rsc/regexp/regexp1.html).

## Supported Glob Syntax

| Pattern          | Description                                                 |
|:----------------:| ----------------------------------------------------------- |
| `a`, `z`         | Simple match looking for the specified character            |
| `?`              | Matches any character                                       |
| `*`              | Matches any string of characters including the empty string |
| `[a-f]`, `[0-9]` | Matches any character within the specified range            |

## Build

Build `GlobMatcher.sln` using Visual Studio 2017.

## Usage

```
> .\Globmatcher\bin\Release\GlobMatcher.exe "My * Pattern" "My Glob Pattern"
Match: true
```

## Performance

![Performance test results](/PerformanceTest/perftest-results.png)

## TODO

* Finish `README.md`