library('stringr')
library('lubridate')

dxbDateString <- function(aDate) {
  theDate <- format(aDate, "20%y%m%d")
}

jhuFileDateString <- function(aDate) {
  theDate <- format(aDate, "%m-%d-20%y")
}

# Accepts date as string mm-dd-yy
formatJHUDateForColumnName <- function(aString) {
  res <- NA
  matches <- str_match(aString, "^0?([1-9][0-9]?)-0?([1-9][0-9]?)-20([2-9][0-9])")
  if (length(matches) == 4) {
    res <- paste(matches[2], "/", matches[3], "/20", matches[4], sep="")
  }
  res
}

# Accepts date as type date
formatDateForColumnName <- function(aDate) {
  formatJHUDateForColumnName(jhuFileDateString(aDate))
}

cleanXmmddyyVector <- function(aVector) {
  res <- rep(NA, times=length(aVector))
  for (i in 1:length(aVector)) {
    matches <- str_match(aVector[i], "^X0?([0-9]+.)0?([0-9]+.20)")
    if (length(matches) == 3) {
      res[i] <- paste("X", matches[2], matches[3], sep="")
    }
  }
  res
}

cleanMmmddVector <- function(aVector) {
  res <- rep(NA, times=length(aVector))
  for (i in 1:length(aVector)) {
    matches <- str_match(aVector[i], "^([A-Z][a-z]{2}).0?([0-9]+)$")
    if (length(matches) == 3) {
      res[i] <- paste(matches[2], ".", matches[3], sep="")
    }
  }
  res
}

canonicalDateString <- function(Xmdy) {
  date_comps <- str_match(Xmdy, "^X([0-9]+)[\\./]([0-9]+)[\\./]([0-9]{2})")
  month_number <- as.integer(date_comps[2])
  paste(month.abb[month_number], ".", date_comps[3], sep="")
}

rezeroXmmddyyVector <- function(aVector) {
  # Input: date in format "X4.1.20", "X4.01.20", "X04.1.20", or "X04.01.20" e.g.
  # Output: list of (mmmdd = "Apr.01", yyyy = "2020-04-01")
  
  f2 <- c("01", "02", "03", "04", "05", "06", "07", "08", "09",
          "10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
          "20", "21", "22", "23", "24", "25", "26", "27", "28", "29",
          "30", "31")
  
  matches <- str_match(aVector, "^X0?([0-9]+)[\\./]0?([0-9]+)[\\./]20")
  res <- list(mmmdd = paste(month.abb[as.integer(matches[,2])],
                            ".",
                            f2[as.integer(matches[,3])],
                            sep=""),
              yyyy  = paste("2020",
                            f2[as.integer(matches[,2])],
                            f2[as.integer(matches[,3])],
                            sep="-"))
}

rezeroMDVector <- function(aVector) {
  # Input: date in format "Apr.1" e.g.; Mmm.d or Mmm.dd
  # Output: list of (mmmdd = "Apr.01", yyyy = "2020-04-01")
  
  f2 <- c("01", "02", "03", "04", "05", "06", "07", "08", "09",
          "10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
          "20", "21", "22", "23", "24", "25", "26", "27", "28", "29",
          "30", "31")
  ddf2 <- c("01", "02", "03", "04", "05", "06", "07", "08", "09",
            "10", "11", "12")
  
  matches <- str_match(aVector, "^([A-Z][a-z]{2})[\\. ]0?([0-9]+)$")
  mm <- rep(NA, times=length(aVector))
  for (i in 1:length(aVector)) {
    mm[i] <- ddf2[month.abb == matches[i,2]]
  }
  dd <- f2[as.integer(matches[,3])]
  
  res <- list(mmmdd = paste(matches[,2], dd, sep="."),
              yyyy = paste("2020", mm, dd, sep="-"))
}

rezeroMDDate <- function(mmmd) {
  rezeroMDVector(mmmd)
}

rezeroMdyVector <- function(aVector) {
  # Input: date in format "mm/dd/yy" or "mm.dd.yy"
  # Output: list of (mmmdd = "Apr.01", yyyy = "2020-04-01")
  
  f2 <- c("01", "02", "03", "04", "05", "06", "07", "08", "09",
          "10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
          "20", "21", "22", "23", "24", "25", "26", "27", "28", "29",
          "30", "31")
  
  monthsdf <- data.frame(abb = month.abb, ix = f2[1:12])
  matches <- str_match(aVector, "^([0-9]+).([0-9]+).([0-9]{2})$")
  
  mm <- monthsdf$ix[as.integer(matches[,2])]
  MM <- monthsdf$abb[as.integer(matches[,2])]
  dd <- f2[as.integer(matches[,3])]
  yy <- paste("20", matches[,4], sep = "")
  res <- list(mmmdd = paste(MM, ".", dd, sep=""),
              yyyy = paste(yy, mm, dd, sep="-"))
}

rezeroMdyDate <- function(mdy) {
  rezeroMdyVector(mdy)
}
