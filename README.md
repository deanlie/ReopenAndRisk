Shiny app to display various statistics about COVID-19 pandemic
===============================================================

General Features
----------------

 * Include statistics on vaccination, case occurrence, mortality, etc;
 * Display boxplots, for state-by-state or county-by-county statistics within a selected state
 * Display trend lines for up to six selected states or up to six selected counties within a single state
 * Display data used to compute the above boxplots or trend lines


Available in https://deanlie.shinyapps.io/ReopenAndRisk/ (as of Aug 22, 2021)
Updated, Redeployed Oct 7, 2021

Bugs
----
 * 2021-10-08: Test Positivity boxplots have a bug! Scale is off,
maybe by a factor of 10000, or maybe two data points are way off,
throwing the whole plot scale off. Hold off on debugging until tables are
displayed!
 * 2021-10-11: Run errorsOct11.R to see some bugs in data display

Other TODO
----------
 * Highlight lines of data tables containing data for selected states or counties

Open Branches
-------------
>FullTestCoverage:

> NOTE! The large tables generate huge output in the JSON files which take prohibitively long to diff. Before it is practical to create tests which cover these table outputs, it is imperative to create and use a small set of static data. As of Oct 13 I am checkpointing progress on the FullTestCoverage branch and opening a new branch, based on the master branch at a point where there are known bugs to exhibit errors to exercise the test system, to create and use such small data.

> 1. Added tests, per main tab, which go through all subtabs of each of
vaccination, new case, total case, new deaths, total deaths, test growth,
and test results tabs
> 2. TODO: Upgrade mytest.R so it goes through all of the above in one test
> 3. TODO: Remove BackOutCounties; it was to detect a particular bug
> 4. TODO: Remove newCasesTest, totalCasesTest, newDeathsTest, totalDeathsTest
> 5. TODO: Make sure that newCasesTabTest etc can do comparisons with expected; you may have to remove and reconstruct expected files
  
Notes to self:
--------------
To view differences between expected and current results, run:
    
      viewTestDiff(".", "mytest")

To save current results as expected results, run:

      snapshotUpdate(".", "mytest")

