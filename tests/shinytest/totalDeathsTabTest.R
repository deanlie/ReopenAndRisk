app <- ShinyDriver$new("../../", loadTimeout = 100000)
app$snapshotInit("totalDeathsTabTest")

app$setInputs(mainTabsetPanel = "Total Deaths")
app$snapshot()
app$setInputs(totalDeathsTabsetPanel = "Trend Line")
app$snapshot()
app$setInputs(totalDeathsTabsetPanel = "Data", timeout_ = 5000)
app$snapshot()
app$setInputs(movingAvg = FALSE, wait_ = FALSE, values_ = FALSE)
app$snapshot()
app$setInputs(totalDeathsTabsetPanel = "Trend Line")
app$snapshot()
app$setInputs(totalDeathsTabsetPanel = "Boxplot")
app$snapshot()
app$setInputs(stateChoices = c("MA", "ME"), wait_ = FALSE, values_ = FALSE)
app$snapshot()
app$setInputs(totalDeathsTabsetPanel = "Trend Line")
app$snapshot()
app$setInputs(totalDeathsTabsetPanel = "Data", timeout_ = 5000)
app$snapshot()
app$setInputs(movingAvg = TRUE)
app$snapshot()
app$setInputs(totalDeathsTabsetPanel = "Trend Line")
app$snapshot()
app$setInputs(totalDeathsTabsetPanel = "Boxplot")
app$snapshot()
app$setInputs(chooseCounty = TRUE, timeout_ = 4000)
app$setInputs(countyChoices = c("Barnstable", "Dukes", "Suffolk", "Worcester"))
app$snapshot()
app$setInputs(totalDeathsTabsetPanel = "Trend Line")
app$snapshot()
app$setInputs(totalDeathsTabsetPanel = "Data", timeout_ = 5000)
app$snapshot()
app$setInputs(movingAvg = FALSE, timeout_ = 4000)
app$snapshot()
app$setInputs(totalDeathsTabsetPanel = "Trend Line")
app$snapshot()
app$setInputs(totalDeathsTabsetPanel = "Boxplot")
app$snapshot()
