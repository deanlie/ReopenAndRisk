FOO <- loadATypeOfData(TRUE, "Confirmed",
                       myTSColTypes(), myTSColTypes(),
                       computeCounty = TRUE,
                       computeNew = TRUE,
                       computeAvg = TRUE,
                       computePercent = TRUE,
                       traceThisRoutine = TRUE,
                       prepend = "")

print(paste("US", as.character(FOO$US[1, "Combined_Key"]), as.character(FOO$US[1, "9/20/21"])))
print(paste("US_Pct", as.character(FOO$US_Pct[1, "Combined_Key"]), as.character(FOO$US_Pct[1, "9/20/21"])))
print(paste("US_Avg", as.character(FOO$US_Avg[1, "Combined_Key"]), as.character(FOO$US_Avg[1, "9/20/21"])))
print(paste("US_PctAvg", as.character(FOO$US_PctAvg[1, "Combined_Key"]), as.character(FOO$US_PctAvg[1, "9/20/21"])))
print(paste("US_New", as.character(FOO$US_New[1, "Combined_Key"]), as.character(FOO$US_New[1, "9/20/21"])))
print(paste("US_NewAvg", as.character(FOO$US_NewAvg[1, "Combined_Key"]), as.character(FOO$US_NewAvg[1, "9/20/21"])))
print(paste("State", as.character(FOO$State[1, "Combined_Key"]), as.character(FOO$State[1, "9/20/21"])))
print(paste("State_Pct", as.character(FOO$State_Pct[1, "Combined_Key"]), as.character(FOO$State_Pct[1, "9/20/21"])))
print(paste("State_Avg", as.character(FOO$State_Avg[1, "Combined_Key"]), as.character(FOO$State_Avg[1, "9/20/21"])))
print(paste("State_PctAvg", as.character(FOO$State_PctAvg[1, "Combined_Key"]), as.character(FOO$State_PctAvg[1, "9/20/21"])))
print(paste("State_New", as.character(FOO$State_New[1, "Combined_Key"]), as.character(FOO$State_New[1, "9/20/21"])))
print(paste("State_NewAvg", as.character(FOO$State_NewAvg[1, "Combined_Key"]), as.character(FOO$State_NewAvg[1, "9/20/21"])))
print(paste("County", as.character(FOO$County[1, "Combined_Key"]), as.character(FOO$County[1, "9/20/21"])))
print(paste("County_Avg", as.character(FOO$County_Avg[1, "Combined_Key"]), as.character(FOO$County_Avg[1, "9/20/21"])))
print(paste("County_New", as.character(FOO$County_New[1, "Combined_Key"]), as.character(FOO$County_New[1, "9/20/21"])))
print(paste("County_NewAvg", as.character(FOO$County_NewAvg[1, "Combined_Key"]), as.character(FOO$County_NewAvg[1, "9/20/21"])))
