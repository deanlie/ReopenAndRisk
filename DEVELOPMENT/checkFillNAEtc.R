est2VsEst3 <- function() {
  uniformSelectionA <- function(aTibble) {
    result <- aTibble %>%
      select(matches("^8/1[2-8]"))
  }
  uniformSelectionB <- function(aTibble) {
    result <- aTibble %>%
      select(matches("^9/1[6-9]"),
             matches("^9/[2-3][0-9]"),
             matches("^10"),
             matches("^11/[1-2]"))
  }
  uniformSelection <- function(aTibble) {
    result <- aTibble %>%
      select(matches("^8/1[2-9]"),
             matches("^8/[2-3][0-9]"),
             matches("^9"),
             matches("^10"),
             matches("^11/[1-2]"))
  }

  est0 <- read_csv("./DATA/US_State_Total_Test_Results.csv", show_col_types = FALSE)
  est2 <- read_csv("./DATA/US_State_Total_Test_Results_EST2.csv", show_col_types = FALSE)
  est3 <- read_csv("./DATA/US_State_Total_Test_Results_EST3.csv", show_col_types = FALSE)
  
  data0 <- uniformSelection(est0)
  data2 <- uniformSelection(est2)
  data3 <- uniformSelection(est3)
  
  sum0 <- data0 %>%
    summarize(across(.cols = everything(), .fns = sum))
  sum2 <- data2 %>%
    summarize(across(.cols = everything(), .fns = sum))
  sum3 <- data3 %>%
    summarize(across(.cols = everything(), .fns = sum))
  
  diffState0 <- data3 - data0
  View(diffState0)
  diffUS0    <- sum3 - sum0
  View(diffUS0)
  
  # diffState2 <- data3 - data2
  # View(diffState2)
  # View(diffState2[,75:dim(data2)[2]])
  # diffUS2 <- sum3 - sum2

  return(data3)
}