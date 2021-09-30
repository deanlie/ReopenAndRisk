Shiny app to display various statistics about COVID-19 pandemic

Include vaccination, case occurrence, mortality, etc;

Display boxplots for state-by-state or county-by-county statistics within a state
for selected items

Available in https://deanlie.shinyapps.io/ReopenAndRisk/ (as of Aug 22, 2021)

Branch Testing1234 merged in 2021-09-27:
  Accomplished:
  * Learn to use shinytest.
  * Set up automatic test to run vaccination tab through just US, MA & ME, and 4 MA counties DONE
  * Ditto for remaining tabs as of Sept 26
  * Plots all show static data used when manual testing is hardcoded in app, live data
      otherwise
  * Tests pass when manual testing is hardcoded in app
  * When manual testing is turned off in app, test script must have line
      app$setInputs(countyChoices = c("Barnstable", "Dukes", "Suffolk", "Worcester"),
              allowInputNoBinding_ = TRUE)
    rather than just
      app$setInputs(countyChoices = c("Barnstable", "Dukes", "Suffolk", "Worcester"))
    Note that this changes the output images which did not formerly have the "county"
      selection box.

    To view differences between expected and current results, run:
      viewTestDiff(".", "mytest")
    To save current results as expected results, run:
      snapshotUpdate(".", "mytest")

Branch Per100_Cases_And_Mortality:
  Accomplished:
  * Change Cases and Deaths tabs to New Cases and New Deaths resp;
  * Display on New Cases tab shows correct "per 100K" data except for one test, see To Do below
  * Added Total Cases tab with correct display and test for it
  * There are now tests newCasesTest, newDeathsTest, totalCasesTest, totalDeathsTest
  * Changed text on those four tabs
  
  To Do:
  * newCasesTest is broken for test6 "Problem with `filter() input `..1`"
                                      object `Province_State' not found"
  * Correct display for New Deaths and Total Deaths tabs 
  * show numbers per 100K population in last day in the graphs on those four tabs.
    That's what the graph labels say, but it's a lie. I need to get the right data
    and process it properly.

