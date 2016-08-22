This is a history of changes to clara-rules.

### 0.12.0

* Eliminate unnecessary retractions in accumulators. See [issue 182](https://github.com/rbrush/clara-rules/issues/182).
* Rule activations fire int he order they are given to the compiler. See [issue 192](https://github.com/rbrush/clara-rules/issues/192).
* Fix bug where rule constrained may be ignored. See [issue 194](https://github.com/rbrush/clara-rules/issues/194).
* Make rule compilation deterministic by eliminating internal iteration over unordered data structures. See [issue 199](https://github.com/rbrush/clara-rules/issues/199).
* Improve testing of rule firing permutations. See [issue 205](https://github.com/rbrush/clara-rules/issues/205).
* Optimize common retraction pattern by checking fact identity first. See [issue 213](https://github.com/rbrush/clara-rules/issues/213).
* Correct several accumulator edge cases. See issues [189](https://github.com/rbrush/clara-rules/issues/189), [190](https://github.com/rbrush/clara-rules/issues/190), and [102](https://github.com/rbrush/clara-rules/issues/102).
* Working memory optimizations. See [issue 184](https://github.com/rbrush/clara-rules/issues/184).
* Clojure doc clarifications. 

### 0.11.1

* Generated code for the left-hand side should only access fields that are used. See [issue 180](https://github.com/rbrush/clara-rules/pull/180).
* Fix incorrect qualification of let variables on right-hand side. See [issue 178](https://github.com/rbrush/clara-rules/issues/178).
* Optimize fact retraction. See [issue 183](https://github.com/rbrush/clara-rules/pull/183).

### 0.11.0

* Add a "grouping-by" accumulator. See [issue 164](https://github.com/rbrush/clara-rules/pull/164).
* Fix truth maintenance when working with equal inserted facts. See [issue 171](https://github.com/rbrush/clara-rules/issues/171).
* Fix incorrect rule activation edge case when dealing with complex nested negations and unconditional inserts. See [issue 174](https://github.com/rbrush/clara-rules/issues/174).
* clara.rules/mk-session now loads rules stored in a var if given a qualified symbol for that var. See [issue 177](https://github.com/rbrush/clara-rules/issues/177).

### 0.10.0
Clara 0.10 is compatible with previous versions, with a couple caveats. These are:

* The intermediate representation of the Rete network changed as reflected in schema updates in clara.rules.schema. This only affects users that build tooling that inspect the network structure.
* The order of rule ordering and the state of queries prior to (fire-rules) being called may have changed. This ordering was not guaranteed previously, but users may have depended on it accidentally.
* ClojureScript users will need to use ClojureScript 1.7.170 or newer.

Here are the specifics on what changed since 0.9.2:

* Fix unification bugs when dealing with nested negations. See [issue 166](https://github.com/rbrush/clara-rules/issues/166).
* Properly handle tests nested in negation nodes. See [issue 165](https://github.com/rbrush/clara-rules/issues/165).
* Improve inspect function to explain the insertion of a given fact. See [issue 161](https://github.com/rbrush/clara-rules/issues/161).  
* Remove duplicate rules and dependency on order of rules when creating sessions. See [issue 157](https://github.com/rbrush/clara-rules/issues/157).
* Significantly improve performance of building the Rete network when dealing with large disjunctions. See [issue 153](https://github.com/rbrush/clara-rules/issues/153).
* Allow multiple binding and equality checks in a single expression. See [issue 151](https://github.com/rbrush/clara-rules/issues/151).
* Ensure variables bounded in a nested, negated conjunction are visible elsewhere in that conjunction. See [149](https://github.com/rbrush/clara-rules/issues/149).

### 0.9.2
* Report better error and line number when parsing malformed productions. See issue [144](https://github.com/rbrush/clara-rules/issues/144).
* Fix truth maintenance bug when using disjunctions. See issue [145](https://github.com/rbrush/clara-rules/pull/145).
* Target Java API compilation to Java 1.6. See issue [146](https://github.com/rbrush/clara-rules/issues/146).
* Catch exceptions thrown in rule actions and add context for debugging. See issue [147](https://github.com/rbrush/clara-rules/issues/147).

### 0.9.1
* Allow binding of arbitrary expressions that use previous variables. See [issue 142](https://github.com/rbrush/clara-rules/issues/142).
* Simplify variable dependencies with a topological sort of rule conditions. See [issue 133](https://github.com/rbrush/clara-rules/issues/133).

### 0.9.0
* Move to Clojure 1.7 and adopt modern ClojureScript best practices, such as reader conditionals and cljs.test.
* ClojureScript users may now use macros from clara.rules; clara.rules.macros should be considered deprecated. See [issue 128](https://github.com/rbrush/clara-rules/issues/128).
* Add an :exists operator. See [issue 130](https://github.com/rbrush/clara-rules/issues/130).
* Pre-defined accumulators now handle fact retraction. See [issue 127](https://github.com/rbrush/clara-rules/issues/127).
* Allow use of accumulator results in other rule conditions. See [issue 132](https://github.com/rbrush/clara-rules/issues/132).
* Support arbitrary comparisons in accumulators in ClojureScript, bringing it inline with the Clojure support. See [issue 131](https://github.com/rbrush/clara-rules/issues/131).
* Support multiple productions defined in a single var, useful for third-party macros. See [issue 134](https://github.com/rbrush/clara-rules/issues/134).
* Update several dependencies.
* Mark internal namespaces as internal, as they may be moved in a future release.

### 0.8.9
* Properly handle deeply nested conjunctions. See [issue 126](https://github.com/rbrush/clara-rules/pull/126).
* Report error for unbound condition variables across all condition types. See [issue 124](https://github.com/rbrush/clara-rules/pull/124).
* Support munged record field names. See [issue 121](https://github.com/rbrush/clara-rules/pull/121).
* Generalize schema used for s-expressions in rules. See [issue 120](https://github.com/rbrush/clara-rules/pull/120).
* Support multiple expressions on right-hand side of defrule. See [issue 118](https://github.com/rbrush/clara-rules/issues/118).
* Properly call retract-facts-logical! listener. See [issue 117](https://github.com/rbrush/clara-rules/issues/117).
* Fix retraction when using custom fact type. See [issue 116](https://github.com/rbrush/clara-rules/issues/116).
* Support type ancestors in ClojureScript. See [issue 115](https://github.com/rbrush/clara-rules/pull/115).
* Handle aliased symbols in ClojureScript. See [issue 113](https://github.com/rbrush/clara-rules/issues/113).

### 0.8.8
* Upgrade to Prismatic Schema 0.4.3
* Handle use of Clojure .. macro in rule expressions. See [issue 108](https://github.com/rbrush/clara-rules/pull/108).
* Fix edge case yielding an NPE when analysis some expressions. See [issue 109](https://github.com/rbrush/clara-rules/pull/109).

### 0.8.7
* Properly qualify references to Java classes on the RHS of rules, supporting try/catch and static method calls. See [issue 104](https://github.com/rbrush/clara-rules/issues/104).
* Fix bug when retracting a subset of facts blocked by a negation rule. See [issue 105](https://github.com/rbrush/clara-rules/issues/105).

### 0.8.6
* Fix a collection of issues surrounding referencing bound variables in nested functions. See [issue 90](https://github.com/rbrush/clara-rules/issues/90) and items referenced from there.
* Fix a truth maintenance issue for accumulators that offer an initial value when there is nothing to accumulate over. See [issue 91](https://github.com/rbrush/clara-rules/issues/91).
* Fix bug that caused options to be dropped in cljs. See [issue 92](https://github.com/rbrush/clara-rules/pull/92).
* Allow explicitly specifying productions in CLJS. See [issue 94](https://github.com/rbrush/clara-rules/pull/94).
* Better handle macro-generated rules. See [issue 100](https://github.com/rbrush/clara-rules/pull/100).
* The :no-loop property now applies to facts retracted due to truth maintenance. See [issue 99](https://github.com/rbrush/clara-rules/issues/99).

### 0.8.5
* Fix specific filtered accumulator bug. See [issue 89](https://github.com/rbrush/clara-rules/pull/89).
* Allow binding variables in set and map literals. See [issue 88](https://github.com/rbrush/clara-rules/pull/88).
* Fix truth maintenance consistency when working with equal facts. See [issue 84](https://github.com/rbrush/clara-rules/issues/84).

### 0.8.4
* Ensure all truth maintenance updates are flushed. See [issue 83](https://github.com/rbrush/clara-rules/issues/83).

### 0.8.3
* Fix for truth maintenance when an accumulator produces a nil value. See [issue 79](https://github.com/rbrush/clara-rules/pull/79).
* Use bound facts in unification. See [issue 80](https://github.com/rbrush/clara-rules/issues/80).
* Improve inspection and explainability support in the clara.tools.inspect namespace.

### 0.8.2
* Batch up inserts done by rules and apply them as a group. See [issue 58](https://github.com/rbrush/clara-rules/issues/58).
* Optimize some internal functions based on real-world profiling.

### 0.8.1
* Fix stack overflow under workloads with many individually inserted facts. See [issue 76](https://github.com/rbrush/clara-rules/pull/76).

### 0.8.0
* Support for salience. See [issue 25](https://github.com/rbrush/clara-rules/issues/25).
* Rule compilation is significantly faster. See [issue 71](https://github.com/rbrush/clara-rules/issues/71).
* Handle use cases where there are a large number of retracted facts. See [issue 74](https://github.com/rbrush/clara-rules/pull/74).
* Add insert-all! and insert-all-unconditional!. See [issue 75](https://github.com/rbrush/clara-rules/issues/75).

### 0.7.0
* Allow bound variables to be used by arbitrary functions in subsequent conditions. See [issue 66](https://github.com/rbrush/clara-rules/issues/66)
* Add metadata to rule's right-hand side so we see line numbers in compilation errors and call stacks. See [issue 69](https://github.com/rbrush/clara-rules/issues/69)
* Improved memory consumption in cases where rules may be retracted and re-added frequently.

### 0.6.2
* Properly handle retractions in the presence of negation nodes; see [issue 67](https://github.com/rbrush/clara-rules/issues/67).
* Report error if the fact type in a rule appears to be malformed; see [issue 65](https://github.com/rbrush/clara-rules/issues/65).

### 0.6.1
* Reduce depth of nested function for [issue 64](https://github.com/rbrush/clara-rules/issues/64).
* Clean up reflection warnings.

### 0.6.0
* Several performance optimizations described in [issue 56](https://github.com/rbrush/clara-rules/issues/56) and [this blog post](http://www.toomuchcode.org/blog/2014/06/16/micro-bench-macro-optimize/).
* Session durability as an experimental feature. See [issue 16](https://github.com/rbrush/clara-rules/issues/16) and [the wiki](https://github.com/rbrush/clara-rules/wiki/Durability).
* Improved ability to inspect session state and explain why rules and queries were activated. See the [inspection page on the wiki](https://github.com/rbrush/clara-rules/wiki/Inspection) for details.
* A list of smaller changes can be seen via the [milestone summary](https://github.com/rbrush/clara-rules/issues?milestone=8&page=1&state=closed)

### 0.5.0
Contains several bug fixes and some usage enhancements, including:

* The clara.tools.inspect package allows for inspection and explanation of why rules fired. Also see [issue 48](https://github.com/rbrush/clara-rules/issues/48).
* [File and line information is preserved when compiling.](https://github.com/rbrush/clara-rules/pull/51)
* A list of several other fixes can be seen via the [milestone summary](https://github.com/rbrush/clara-rules/issues?milestone=7&page=1&state=closed)

### 0.4.0
This is a major refactoring of the Clara engine, turning all rule and Rete network representations into well-defined data structures. Details are at these links:

* [Rules as Data Support](http://www.toomuchcode.org/blog/2014/01/19/rules-as-data/)
* [ClojureScript Rete network on the server side](https://github.com/rbrush/clara-rules/issues/34)

### 0.3.0
* [ClojureScript support](https://github.com/rbrush/clara-rules/issues/4)
* [No-loop option for rules](https://github.com/rbrush/clara-rules/issues/23)
* [Improved variable binding](https://github.com/rbrush/clara-rules/pull/26)

### 0.2.2
* [Accumulators should always fire if all variables can be bound](https://github.com/rbrush/clara-rules/issues/22)

### 0.2.1
A fix release with some internal optimizations, including the following:

* [Use activation list for rules](https://github.com/rbrush/clara-rules/issues/19)
* [Support symbols with "-" in them](https://github.com/rbrush/clara-rules/issues/20)

### 0.2.0
* [Remove need for == macro](https://github.com/rbrush/clara-rules/pull/18)
* [Support for arbitrary Clojure maps](https://github.com/rbrush/clara-rules/issues/6)

### 0.1.1
A number of bug fixes, see the [milestone summary](https://github.com/rbrush/clara-rules/issues?milestone=1&page=1&state=closed)

### 0.1.0
The initial release.
