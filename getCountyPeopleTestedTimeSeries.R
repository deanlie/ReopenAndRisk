library(tidyverse)

CountyTimeSeries <- read_csv("https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/testing_data/county_time_series_covid19_US.csv",
                             show_col_types = FALSE)