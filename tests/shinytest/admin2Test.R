app <- ShinyDriver$new("../../", loadTimeout = 100000)
app$snapshotInit("admin2Test")

app$setInputs(stateChoices = c("LA", "MA"))
app$snapshot()
app$setInputs(chooseCounty = TRUE)
app$setInputs(countyChoices = "Plaquemines")
app$snapshot()
app$setInputs(vaccinationTabsetPanel = "Data")
app$snapshot()
app$setInputs(mainTabsetPanel = "New Cases")
app$snapshot()
app$setInputs(newCaseTabsetPanel = "Data")
app$snapshot()
app$setInputs(mainTabsetPanel = "Total Cases")
app$setInputs(totalCaseTabsetPanel = "Data")
app$snapshot()
app$setInputs(stateChoices = "PR")
app$setInputs(countyChoices = c("Arecibo", "Barceloneta"))
app$snapshot()
app$setInputs(mainTabsetPanel = "New Deaths")
app$snapshot()
app$setInputs(newDeathsTabsetPanel = "Data")
app$snapshot()
app$setInputs(mainTabsetPanel = "Total Deaths")
app$setInputs(totalDeathsTabsetPanel = "Data")
app$snapshot()
