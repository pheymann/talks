## Scala-IO 2018 - Let's derive HTTP clients from types

 - you can find the slides [here](https://pheymann.github.io/talks/scalaio-2018/)

What if I tell you that you can derive your HTTP client function from a single type? Aren't you believing me? Okay, I will show how to do it.

We will start slowly by working out a way to move our HTTP api description onto the type-level using exotic concepts like witness, or heterogenous list. Having accomplished that, we will shift gears and make a deep dive into type-level programming using type classes with recursive instance resolution to derive a fully fledge HTTP client function.

You will see many a type parameter and will ask yourself: How many of them can someone possibly put into a single function signature? (Spoiler: the answer will be "a lot")

In the end, you have learned the following things:

 1. represent information on the type-level,
 2. use type classes and recursive instance resolution to derive functions,
 3. type-level programming is awesome.
