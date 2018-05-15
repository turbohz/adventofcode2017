# Advent of Code 2017

Solutions to ["Advent Of Code 20017"](http://adventofcode.com/2017) problems in F#.

## Disclaimer

These solutions are purely a personal learning exercise in functional programing and F# in particular.

For that reason, they do not always follow best practices and the approach is might not be the most optimal or performant. 

## How to run and test each problem solution

The simplest way (if you use docker, that is), is to use the official F# container:

```sh
docker run -i --rm --volume `pwd`:/src fsharp fsharpi /src/day01.fsx < day01-input.txt
```

