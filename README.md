# advent-code

This year, I'm doing advent of code 2020 in Clojure on Github!

## Usage

### REPL

This project includes [rebel-readline](https://github.com/bhauman/rebel-readline)
for a better REPL experience.

```bash
% lein rebel
```

It also has some ns shortcuts predefined in `dev/user.clj` for convenience.

```clojure
; No tests yet!
user=> (test-problem "day1-2019")

Testing advent-code.problem.day1-2019

Ran 0 tests containing 0 assertions.
0 failures, 0 errors.
{:test 0, :pass 0, :fail 0, :error 0, :type :summary}
; Now add lots of tests
user=> (refresh)
:reloading (advent-code.interfaces advent-code.interfaces-test advent-code.data-helpers advent-code.problem.day1-2019 advent-code.problem.day1-2019-test advent-code.core advent-code.core-test advent-code.data-helpers-test user)
:ok
user=> (test-problem "day1-2019")

Testing advent-code.problem.day1-2019-test

Ran 2 tests containing 5 assertions.
0 failures, 0 errors.
{:test 2, :pass 5, :fail 0, :error 0, :type :summary}
```

### Command Line

You can execute a specific AoC problem from the command line as follows:

```bash
% lein run day1-2019 resources/2019-day1.input
3479429
```

And of course all of the unit tests can be run:

```bash
% lein test

lein test advent-code.core-test

lein test advent-code.data-helpers-test

lein test advent-code.interfaces-test

lein test advent-code.problem.day1-2019-test

Ran 10 tests containing 13 assertions.
0 failures, 0 errors.
```

I haven't bothered building an uberjar but it's there. :-)

## High-level Design

The core idea is that "problems" exist in their own namespace, and that there's
a single common function that each problem defines:

```clojure
; Lookup the method by the problem string
(defmulti run-problem (fn [problem data] problem))
```

`data` here is intended to be the *raw* data you can download from the AoC site.
Specifically, we `slurp` the second argument in command line mode. In the REPL,
you can do the same thing:

```clojure
user=> (slurp "resources/2019-day1.input")
```

AoC generally includes "examples", which have been translated directly into the
primary test cases. For example, for the 2019 day1 problem:

```clojure
(deftest examples
  (are [x y] (= y (mass->fuel x))
    12 2
    14 2
    1969 654
    100756 33583))
```

We do some dynamic namespace trickery in both `dev/user.clj` and
`src/advent_code/core.clj` to avoid having to list 30 different namespaces.

## License

Copyright © 2020 David Wang-Faulkner

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
