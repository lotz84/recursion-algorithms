```
██████╗ ███████╗ ██████╗██╗   ██╗██████╗ ███████╗██╗ ██████╗ ███╗   ██╗
██╔══██╗██╔════╝██╔════╝██║   ██║██╔══██╗██╔════╝██║██╔═══██╗████╗  ██║
██████╔╝█████╗  ██║     ██║   ██║██████╔╝███████╗██║██║   ██║██╔██╗ ██║
██╔══██╗██╔══╝  ██║     ██║   ██║██╔══██╗╚════██║██║██║   ██║██║╚██╗██║
██║  ██║███████╗╚██████╗╚██████╔╝██║  ██║███████║██║╚██████╔╝██║ ╚████║
╚═╝  ╚═╝╚══════╝ ╚═════╝ ╚═════╝ ╚═╝  ╚═╝╚══════╝╚═╝ ╚═════╝ ╚═╝  ╚═══╝

 █████╗ ██╗      ██████╗  ██████╗ ██████╗ ██╗████████╗██╗  ██╗███╗   ███╗███████╗
██╔══██╗██║     ██╔════╝ ██╔═══██╗██╔══██╗██║╚══██╔══╝██║  ██║████╗ ████║██╔════╝
███████║██║     ██║  ███╗██║   ██║██████╔╝██║   ██║   ███████║██╔████╔██║███████╗
██╔══██║██║     ██║   ██║██║   ██║██╔══██╗██║   ██║   ██╔══██║██║╚██╔╝██║╚════██║
██║  ██║███████╗╚██████╔╝╚██████╔╝██║  ██║██║   ██║   ██║  ██║██║ ╚═╝ ██║███████║
╚═╝  ╚═╝╚══════╝ ╚═════╝  ╚═════╝ ╚═╝  ╚═╝╚═╝   ╚═╝   ╚═╝  ╚═╝╚═╝     ╚═╝╚══════╝
```

[![GitHub](https://img.shields.io/github/license/lotz84/recursion-algorithms)](LICENSE)
[![CircleCI](https://img.shields.io/circleci/build/gh/lotz84/recursion-algorithms)](https://app.circleci.com/pipelines/github/lotz84/recursion-algorithms)
[![GitHub contributors](https://img.shields.io/github/contributors/lotz84/recursion-algorithms)](https://github.com/lotz84/recursion-algorithms/graphs/contributors)

This respository is a collection of various algorithms written using recursion schemes. Recursion schemes brings a brilliant perspective guided by Category Theory to recursive data structures and algorithms. I was particularly impressed by ["A Duality of Sorts" written by R. Hinze et al.](https://www.semanticscholar.org/paper/A-Duality-of-Sorts-Hinze-Magalh%C3%A3es/62a1d9ecaea95fbceb42c644ca38dd577b85fe4d) that pointed out that there is a nice duality between the well-known sorting algorithms.

The purpose of this repository is to provide **a broad and comprehensive collection of algorithms written by recursion schemes** to discover hidden relationships between algorithms. This is a work in progress and I can only work on it in my free time, so contributions will be greatly appreciated. For more information on how to contribute, please refer to "How to get involved" below.

This repository uses [recursion-schemes](https://hackage.haskell.org/package/recursion-schemes) to implement the algorithm. If any recursion schemes are missing, they are implemented in the [Extra Recursion Schemes](RecursionSchemes/Extra.md).

## Table of Contents
### Data Structures
- [Nat](DataStructures/Nat.md)
- [List](DataStructures/List.md)
- [Tree](DataStructures/Tree.md)

### Algorithms
- Natural Number
  - Basic Operations
    - [add](Algorithms/Nat/BasicOperations/Add.md)
    - [product](Algorithms/Nat/BasicOperations/Product.md)
    - [factorial](Algorithms/Nat/BasicOperations/Factorial.md)
    - [lessThan](Algorithms/Nat/BasicOperations/LessThan.md)
  - [Fibonacci Numbers](Algorithms/Nat/Fibonacci.md)
  - [Catalan Numbers](Algorithms/Nat/Catalan.md)
- List
  - Basic Operations
    - [length](Algorithms/List/BasicOperations/Length.md)
    - [filter](Algorithms/List/BasicOperations/Filter.md)
    - [map](Algorithms/List/BasicOperations/Map.md)
    - [zip](Algorithms/List/BasicOperations/Zip.md)
    - [iterate](Algorithms/List/BasicOperations/Iterate.md)
    - [reverse](Algorithms/List/BasicOperations/Reverse.md)
    - [span](Algorithms/List/BasicOperations/Span.md)
    - [groupBy](Algorithms/List/BasicOperations/GroupBy.md)
    - [subsequences](Algorithms/List/BasicOperations/Subsequences.md)
    - [permutations](Algorithms/List/BasicOperations/Permutations.md)
    - [scanr / scanl](Algorithms/List/BasicOperations/Scanr.md)
    - [take / drop](Algorithms/List/BasicOperations/Take.md)
    - [takeWhile / dropWhile](Algorithms/List/BasicOperations/TakeWhile.md)
    - [inits / tails](Algorithms/List/BasicOperations/Inits.md)
  - [Edit Distance](Algorithms/List/EditDistance.md)
  - [Longest Common Subsequence](Algorithms/List/LongestCommonSubsequence.md)
  - [Run Length Conversion](Algorithms/List/RunLengthConversion.md)
  - Sorting
    - [Insertion Sort](Algorithms/List/Sorting/InsertionSort.md)
    - [Selection Sort](Algorithms/List/Sorting/SelectionSort.md)
- Tree
  - Basic Operations
    - [depth](Algorithms/Tree/BasicOperations/Depth.md)
  - [Depth First Search](Algorithms/Tree/DepthFirstSearch.md)
- [Extra Recursion Schemes](RecursionSchemes/Extra.md)

## How it works
All algorithms have been tested by [doctest](https://hackage.haskell.org/package/doctest). To make the code test compatible with the markdown used in GitHub Pages, there's a conversion process from markdown to Haskell in the stack test.

## How to get involved
Please, feel free to send a PR with

- implementing an algorithm that doesn't yet exist (don't forget to explain and doctest),
- adding a recursive data structure that can be represented as an fixed point,
- adding a description of some recursion schemes,
- and fixes of typos/bugs/etc.

And if any of the references are wrong or not appropriate, please let me know. If you have any feedback, please make an issue or contact [@lotz84\_](https://twitter.com/lotz84_).

## Related Projects
- [tayllan/awesome-algorithms: A curated list of awesome places to learn and/or practice algorithms.](https://github.com/tayllan/awesome-algorithms)
