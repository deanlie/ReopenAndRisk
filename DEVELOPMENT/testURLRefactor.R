library(rlang)
library(purrr)

reportComparison <- function(baseFctName, val1, val2) {
  oldFctName <- paste(baseFctName, "_OLD", sep = "")
  if (identical(val1, val2)) {
    print(paste("   ", baseFctName, "Passes"))
  } else {
    print("************ FAIL *************")
    print(paste(oldFctName,  "->", val1))
    print(paste(baseFctName, "    ->", val2))
    print("*******************************")
  }
}

compareURLRtn <- function(baseFctName) {
  oldFctName <- paste(baseFctName, "_OLD", sep = "")
  val1 <- eval(call2(oldFctName))
  val2 <- eval(call2(baseFctName))
  reportComparison(baseFctName, val1, val2)
}

compareURLRtnNeedingDate <- function(baseFctName) {
  aDate = as_date("2021-09-01")
  oldFctName <- paste(baseFctName, "_OLD", sep = "")
  val1 <- eval(call2(oldFctName, aDate))
  val2 <- eval(call2(baseFctName, aDate))
  reportComparison(baseFctName, val1, val2)
}

compareURLRtnNeedingType <- function(baseFctName, aType, aLocale) {
  oldFctName <- paste(baseFctName, "_OLD", sep = "")
  val1 <- eval(call2(oldFctName, aType, aLocale))
  val2 <- eval(call2(baseFctName, aType, aLocale))
  reportComparison(baseFctName, val1, val2)
}

testURLRefactor <- function() {
  names <- c("JHU_repository",
             "Vacc_URL",
             "Vacc_TS_URL",
             "peopleVacc_URL")
  routinesWithDate <- c("updateSansCountyDataForDate_URL",
                        "updateStateLevelDataForDate_URL",
                        "JHUDailyStateDataURLForDate")
  for (aName in names) {
    compareURLRtn(aName)
  }
  compareURLRtnNeedingType("TS_URL", "deaths", "US")
  for (aName in routinesWithDate) {
    compareURLRtnNeedingDate(aName)
  }
}
