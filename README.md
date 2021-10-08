Shiny app to display various statistics about COVID-19 pandemic

Include vaccination, case occurrence, mortality, etc;

Display boxplots for state-by-state or county-by-county statistics within a state
for selected items

Available in https://deanlie.shinyapps.io/ReopenAndRisk/ (as of Aug 22, 2021)

2021-10-08: Test Positivity boxplots have a bug! Scale is off,
  maybe by a factor of 10000, or maybe two data points are way off,
  throwing the whole plot scale off. Hold off on debugging until tables are
  displayed!
  
Branch OneDisplayItemPerNestedTab:
  * 2021-10-08: All tabs (except summary, special case) have nested tabs
      for Boxplot / Trend Line
  * Test mytest is updated for vaccination tab, default (Boxplot) subtab
  * TODO: Add "Table" subtab with some static display to each tab
  * TODO: Add appropriate table data with no styling to each "Table" subtab
  * TODO: Add styling to highlight selected rows on tables

    To view differences between expected and current results, run:
      viewTestDiff(".", "mytest")
    To save current results as expected results, run:
      snapshotUpdate(".", "mytest")

