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
Updated, Redeployed Nov 18, 2021 with test positivity page
Updated, Redeployed Dec 7, 2021 not to save the downloaded TS_VACC data file

Bugs
----
 * 2021-10-24: I just noticed that static data for Puerto Rico in US_State_People_Tested is NA for Sept 18, and less on Sept 17 than Sept 16. This causes problems for overall US test positivity computations and messes up the graphs for state test positivity.
 * 2021-10-21: Routine assembleDirectBoxPlot_B, an attempt to refactor
  assembleDirectBoxPlot, failed when called from doTotalCaseTab.R. It gave
  "Warning: Error in *: non-numeric argument to binary operator
  172: leastSquaresTrendParams [./assemblePlotObject.R#18]
  171: computePlotDataFromFrame [./assemblePlotObject.R#111]
->  170: assembleDirectBoxPlot_B [./assemblePlotObject.R#386]
  169: plotTotalCaseBoxplots [doTotalCaseTab.R#75]
  ...
Called from: eval(expr, p)
debug at ./assemblePlotObject.R#18: xy <- x * y
Browse[2]> x
 [1]  2  3  4  5  6  7  8  9 10 11 12
Browse[2]> y
 [1] "38158"            "Natchitoches"     "Louisiana"        "15464.6469940773"
 [5] "15538.026101997"  "15615.5234251571" "15705.3753940383" "15794.1042133086"
 [9] "15882.8330325788" "15971.5618518491" "16079.0098313029"
 
 
 * 2021-10-14: updatePopulationEstimateData should be modified to check date of current population estimate files, only update if they're old (say, older than 1 month)
 * 2021-10-14: US_State_Testing_Rate.csv, US_State_Total_Test_Results, US_State_Case_Fatality_Ratio, and US_State_Incident_Rate were rewritten to eliminate bad column names. See DEVELOPMENT/zapStateTestingRate.R. There is a bug in computation of the corresponding US_ data files.
 * 2021-10-08: Test Positivity boxplots have a bug! Scale is off,
maybe by a factor of 10000, or maybe two data points are way off,
throwing the whole plot scale off. Hold off on debugging until tables are
displayed!

Other TODO
----------
 * Highlight lines of data tables containing data for selected states or counties

Open Branches
-------------

CleanDataDirAsYouGo
 * Set up a test of updating:
    1. Save current setup
    2. Update
    3. Get set of results sufficient for comparison:
      for each output file
        select range of output columns (just last few)
        filter out just a few rows (e.g. those in static set)
    4. Refactor-wise, make the update limited in date; test as you go
 * Redo static data with spaces in combined keys
 * Clean up DATA subdirectory; modify update routines to clean up as you go, that is,
    delete downloaded data as soon as the needed time series files are updated.

    1. Add tracing to show when anything is downloaded
      a. Find includes of RCurl
      b. Find uses of all routines returning a URL
      c. Put 'em in a single module (URLFunctions.R will do)
      c. Put in tracing
      
    2. Create tests to update by one day; verify that updates are
        only called once.
        NOTE that DATA/STATIC is under version control AS ARE TESTS
        so I can have a separate set of tests and test data for this
        branch! Tests which display the updated, non-averaged, data
        arr all I need as regression tests.
    3. Create tests to update by two days; verify that they work
    4. Implement cleanups
    5. Test to ensure that nothing is left over

Closed branches:
----------------

FixTestingDataComputations

> 1. DONE: Test Positivity boxplots on static data look good and corresponding data
      tables are displayed correctly.
> 2. DONE: TestGrowth tab changes to TestingRate
> 3. DONE: Three decimal places are shown in testing rate table display

SmallSetOfStaticData

> 1. Created new static files with data for only a limited number of states, and only a limited number of counties for each of them, and a limited number of dates, sufficient to exercise all program options.
> 2. States include ones with unique Admin2 names, i.e. Louisiana and Puerto Rico, to exercise that
> 3. Enough states and counties to test highlighting of minimum & maximum selections as well as of the selection somewhere; that could be just in one major tab. I kept seven, to allow either 1 or 2 each maximum & minimum values, one not shown, and one in the middle selected, for Massachusetts and fewer for other
> 4. Dates are enough to compute 7-day moving averages with enough left over to compute trend lines.

FullTestCoverage:

> Upgrade test suite to cover all program features with a single test, but have tests for limited sets of features for quick checking. 

> 1. App no longer recreates existing, up-to-date data files every time you open the program.
> 2. Added tests, per main tab, which go through all subtabs of each of
vaccination, new case, total case, new deaths, total deaths, test growth,
and test results tabs
> 3. My original design for this branch specified "Upgrade mytest.R so it goes through all of the above in one test", however the command "testApp()" [note! no arguments] runs all tests in one command, apparently one startup of the app; so there is no need to duplicate tests in a "tests" directory 
> 4. Removed newCasesTest, totalCasesTest, newDeathsTest, totalDeathsTest; they are replaced by newCaseTabTest, totalCaseTabtest, newDeathsTabTest, and totalDeathsTestTab respectively (see (2.) above)
> 6. App now provides an id parameter to gt() calls so random table IDs no longer cause comparison failures for json files

Notes to self:
--------------
To view differences between expected and current results, run:
    
      viewTestDiff(".", "mytest")

To save current results as expected results, run:

      snapshotUpdate(".", "mytest")

