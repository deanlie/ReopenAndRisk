library(tidyverse)

testWtAvg <- function(aTibble) {
  # res <- aTibble %>%
  #   mutate(across(.cols = matches("^[1-9]+/"), ~(.x * Population))) %>%
  #   summarize(across(.cols = (Population | matches("^[1-9]+/")), sum)) %>%
  #   mutate(Combined_Key = "US", .before = Population) %>%
  #   mutate(across(.cols = matches("^[1-9]+/"), ~(.x / Population)))

  res <- aTibble %>%
    mutate(across(.cols = matches("^[1-9]+/"), ~(.x * Population))) %>%
    summarize(Combined_Key = "US", across(.cols = (Population | matches("^[1-9]+/")), sum)) %>%
    # mutate(Combined_Key = "US", .before = Population) %>%
    mutate(across(.cols = matches("^[1-9]+/"), ~(.x / Population)))
}

aTibble1 <- as_tibble(list(Combined_Key = c("Alabama, US", "California, US", "Massachusetts, US"),
                           Population = c(200000, 17123456, 800000),
                           '7/13/21' = c(10, 20, 10), # 10
                           '7/14/21' = c(10, 18, 20), # 18
                           '7/15/21' = c(40, 24, 20), # 24
                           '7/16/21' = c(80, 33, 20)  # 32
                        ))

aTibbleA <- as_tibble(list(Combined_Key = c("Alabama, US", "Alaska, US", "California, US", "Massachusetts, US"),
                           Population = c(200000, 100000, 17123456, 800000),
                           '7/13/21' = c(10, 10, 20, 10), # 10
                           '7/15/21' = c(40, 24, 24, 20), # 24
                           '7/16/21' = c(80, 32, 33, 20)  # 32
))

aTibbleB <- as_tibble(list(Combined_Key = c("Alabama, US", "California, US", "Colorado, US", "Massachusetts, US"),
                           Population = c(200000, 17123456, 100000, 800000),
                           '7/13/21' = c(10, 20, 10, 10), # 10
                           '7/14/21' = c(10, 18, 18, 20), # 18
                           '7/16/21' = c(80, 33, 32, 20)  # 32
))

commonNames <- intersect(names(aTibbleA), names(aTibbleB))
commonRows <- intersect(aTibbleA$Combined_Key, aTibbleB$Combined_Key)

numData <- aTibbleA %>%
  select(any_of(commonNames)) %>%
  filter(Combined_Key %in% {commonRows})

denomData <- aTibbleB %>%
  select(any_of(commonNames)) %>%
  filter(Combined_Key %in% {commonRows})

print(numData)

# print(testWtAvg(aTibble1))

