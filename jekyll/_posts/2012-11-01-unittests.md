---
layout: post
title: Unit Tests
tags: faq
---

One of the reviewers of the [theory paper]() asks how `softclassval` ensures computational
correctness.

## Unit Tests 

`softclassval` uses [`svUnit`](http://cran.r-project.org/web/packages/svUnit/index.html) for [unit
testing](http://en.wikipedia.org/wiki/Unit_testing).

<!-- end excerpt --> 
This means that to each function a piece of testing code can be attached,
additional tests can be stored in separate testing functions. In the tests, the function to be tested
is called. If the function's results do not meet the expectations, an error occurs.

`softclassval`'s unit tests consist of ca. twice as many lines of code compared to coding the
functionality of the package.


## Running the Unit Tests

All unit tests in package `softclassval` can be executed inside R:

{% highlight rconsole %}
> softclassval.unittest ()
                    kind timing                time unit msg
test(.make01)         OK  0.000 2012-11-01 14:27:23         
test(sens)            OK  0.079 2012-11-01 14:27:23         
test(checkrp)         OK  0.006 2012-11-01 14:27:23         
   [...snip...]
test(gdl)             OK  0.000 2012-11-01 14:27:24         
Summary statistics on all tests run:

         OK   **FAILS**   **ERROR** DEACTIVATED 
         21           0           0           0 
{% endhighlight %}

` softclassval.unittest ()` is also called during `R CMD check`.
Successful passing of the unit tests is somewhat hidden in the last lines of `R CMD check`'s output:

{% highlight console %}
$ R CMD check softclassval_0.99-20120820.tar.gz 
   [...snip...]
* checking tests ...
  Running ‘tests.R’
 OK
   [...snip...]
{% endhighlight %}
## Automatic Check results

Because of `R CMD check` running the unit tests, all unit tests need to be successful during the
automatic package building process at [R-Forge](https://r-forge.r-project.org) and
[CRAN](http://www.cran.r-project.org/).

- [summary table of CRAN check results for all packages](http://www.cran.r-project.org/web/checks/check_summary.html)
- [summary table of CRAN check results for `softclassval`](http://www.cran.r-project.org/web/checks/check_results_softclassval.html)
