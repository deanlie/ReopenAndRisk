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
  * Add Total Cases and Total Deaths tabs with content duplicating New<resp> tabs
  * There are now tests newCasesTest, newDeathsTest, totalCasesTest, totalDeathsTest
  * Changed text on those four tabs
  
  To Do:
  * show numbers per 100K population in last day in the graphs on those four tabs.
    That's what the graph labels say, but it's a lie. I need to get the right data
    and process it properly.

