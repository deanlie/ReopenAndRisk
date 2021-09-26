Shiny app to display various statistics about COVID-19 pandemic

Include vaccination, case occurrence, mortality, etc;

Display boxplots for state-by-state or county-by-county statistics within a state
for selected items

Available in https://deanlie.shinyapps.io/ReopenAndRisk/ (as of Aug 22, 2021)

Branch Testing1234:
  * Learn to use shinytest. DONE
  * Deal with dynamic data. Docs say, 
    "Problem: Dynamic data. If your application uses a data source that changes over time, 
      then a snapshot taken yesterday may not match a snapshot taken today,
      even if the app itself hasn’t changed. Dynamic data inherently poses a challenge for
      snapshot-based testing.

    This problem can be avoided by detecting when the application is being tested,
      and in that case use a static data set instead. To do the detection,
      you can do something like the following:
      
      if (isTRUE(getOption("shiny.testmode"))) {
        # Load static/dummy data here
      } else {
        # Load normal dynamic data here
      }""

  * Set up automatic test to run vaccination tab through just US, MA & ME, and 4 MA counties DONE
  * Ditto for New Deaths tab
