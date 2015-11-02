Chess Challenge
===============

As usual, this code uses sbt for its compilation, testing and running.

The problem is the main method (by default) of the "Main" object. You can call it directly by using "run" from sbt. But take care, it is very verbose. Maybe you would like to use the "blind_fold_results" for do the job without dumping so much test. Up to you.

In the other hand, if you want to run the tests, you only need to use "test" from sbt.

The code should use Java 8, because of the restriction of Scala 2.11.7. But I think you can edit the build.sbt file and change this requirements for matching your actual JVM version.

I tried to use a parallel algorithm for improving the speed, but despite there should be a inflection point for grid sizes and big pieces distributions, for our main purpose, a single threat do the job with the smaller code readability/performance ratio.
