# Exercies for Chapter 24

Use `stack ghci` to load code and execute functions

## Exercise 5

Sorry, it is a bit of a mess. Here are the functions I created.

To verify that the logs are parsing correctly run: `parserTests`

For some summing tests run: `ChapterExercises_5.testSectionSum`

Summary of the time for each activity in the example log. `ChapterExercises_5.printSumTimeForEachActivity`

I had to be a little picky about how I was generating my Arbitrary data, but I was able to satisfy a bi-directional parsing test. See the function `ChapterExercises_5.testBidirectionalParsing`.

## Exercise 6

Run `ChapterExercises_6.parserTests` to run a few tests for IPv4.

## Exercise 7

Run `ChapterExercises_7.parserTests` to run a few Perser tests for IPv6.
Run `ChapterExercises_7.testIPv6Helpers` to run a more tests for IPv6 parsing and other helper functions.

## Exercise 8

See `Show` instances for `IPAddress` and `IPAddress6`

## Exersice 9

See function `ChapterExercises_7.toIPAddress6`

## Exercise 10

Very naive start to parsing the DOT language. I have basic parsing of undirected Dot Edges but have not completed building the DOT AST imported from `haphviz` package. See `ChapterExercises_10.parseTests`.
