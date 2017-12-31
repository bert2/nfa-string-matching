# Glob Pattern Matching using Nondeterministic Finite Automata

This is an implementation of a nondeterministic finite automaton (NFA) that matches text input against a [glob pattern](https://en.wikipedia.org/wiki/Glob_(programming)). The implementation is based on [Ken Thompson's NFA construction from regular expressions](https://en.wikipedia.org/wiki/Thompson%27s_construction) and inspired by Russ Cox' article [Regular Expression Matching Can Be Simple And Fast](https://swtch.com/~rsc/regexp/regexp1.html).

Given a glob pattern and an input text the implementation first parses the pattern to construct the corresponding NFA and then feeds the input text into the NFA. Whether the input matches the pattern is printed to the output and reflected with the exit code of the application.

## Supported Glob Syntax

| Pattern                      | Description                                                 |
|:----------------------------:| ----------------------------------------------------------- |
| `a`, `b`, …                  | Simple match looking for the specified character            |
| `?`                          | Matches any character                                       |
| `*`                          | Matches any string of characters including the empty string |
| `[a-f]`, `[0-9]`, …          | Matches any character within the specified range            |
| `\*`, `\?`, `\\`, `\[`, `\]` | Escape character for matching meta characters literally     |

## Build

Build `GlobMatcher.sln` using Visual Studio 2017.

## Usage

```
> .\Globmatcher\bin\Release\GlobMatcher.exe "my * pattern" "my glob pattern"
Match: true
```

## Performance

In order to track the impact of optimization attempts a performance test project has been added to the solution. It repeatedly executes the implementation using a glob pattern and input text of increasing size. The results are labeled with the current git commit hash, stored in a CSV file and rendered to a graph.

The test matches the input text "a<sup>n</sup>" against the glob pattern "\*<sup>n</sup>a<sup>n</sup>" with *n* ranging from *1* to *100*. For instance, for *n = 2* the input "aa" is matched with the pattern "\*\*aa". The test provokes the worst case scenario where the implementation has to try to match the input with all "\*" wildcards, before it can match it successfully against the suffixed "a" characters.

![Graph of performance test results](/PerformanceTest/perftest-results.png)

The figure above shows the most recent performance graph. The pattern length *n* is plotted against the X axis, the runtime in milliseconds against the Y axis. The Y axis has been log-scaled in order to capture the wide range of measured runtimes more accurately. The legend shows the commit hash for each performance test ordered from oldest (top) to newest (bottom). Because the utilized plot library ([FSharp.Charting](https://fslab.org/FSharp.Charting/)) throws errors when rendering `0` values on a log-scaled axis, the measurements have been clamped to always be greater or equal *1 ms*.

For comparison the results when matching the input text against the analogous regular expression ".\*<sup>n</sup>a<sup>n</sup>" with [C#'s regex implementation](https://msdn.microsoft.com/en-us/library/system.text.regularexpressions.regex%28v=vs.110%29.aspx?f=255&MSPPError=-2147217396) have been plotted as well (test cut short at *n = 18*, because there are only so many milliseconds left until the sun expands and swallows our star system).

### A Note on the Performance Results

Up until commit 7333567 an NFA implementation was used that lend itself to easy concatination of automatons, but required more cycles to execute them. Later commits used a refactored version with opposite properties, i.e. faster execution at the expense of a more complex construction.

Commit 91ed910 introduced another refactoring that also fixed a major bug. Due to this bug incorrect automatons were generated that allowed for illegal shortcuts during execution and resulted in false runtimes.

### Reproducing the Performance Results

Each test run of the implementation was executed and recorded the following way:

```
> cd PerformanceTest\
> .\bin\Release\PerformanceTest.exe 1 100 5
Performance testing commit 46e6e40
Warming up with pattern of length 100...done
Running automaton with pattern of length 100 (05x)
Analyzing runtime behaviour...done
Saving results to perftest-results.csv...done
Rendering performance results to perftest-results.png...done
> git add *
> git commit -m '...'
```

The test run for C#'s regex class can be executed like this:

```
> cd PerformanceTest\
> .\bin\Release\PerformanceTest.exe --testCSharpRegex
```

## TODO

* ~~Finish `README.md`~~
* ~~Render performance graph y axis starting at 0~~ *(unable to produce a nicely looking rendering)*
* ~~Use FParsec instead of diy'ed parser combinator~~
* ~~Fix AutomatonPrinter to reflect breaking changes of the Automaton module~~
* ~~Cache DFA state~~ *(doesn't seem to be worth the overhead)*
* Implement regular expression syntax
  * CLI to regular expression matcher
  * ~~Kleene star~~
  * ~~Kleene plus~~
  * ~~Optional match~~
  * ~~Submatch expressions between '(' and ')'~~
  * Metacharacter '.'
  * Metacharacter escaping
  * Alternations
  * Character classes
  * Counted repetitions
* FSCheck regular expression matcher
  * Use random string generator that has a regex as input
* Try NFA `State` type that is a `Letter -> State list`
* Clean up messy performance test code
  * Split into modules
  * Use parser combinator to read CSV file, just because they are fun
