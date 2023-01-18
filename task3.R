library(tidyverse)
library(ggplot2)

data <- get(load('trades.Rdata'))
data <- bind_rows(data)[ , -3]
data <- data[str_detect(data$indic_et, "Imports in|Exports in"), ]


wideData <- pivot_wider(data, names_from = indic_et, values_from = values)
colnames(wideData)

coupledData <- aggregate(wideData[[4]], list(wideData$time), sum)
names(coupledData) <- c("Time", "Sum.Exp")
print(coupledData)

minIndex <- which(coupledData == min(coupledData[["Sum.Exp"]]), arr.ind = TRUE)
aes2 <- coupledData[-minIndex, ]

ggplot(coupledData, aes(x = Time, y = Sum.Exp)) + geom_line() + geom_point() +
      geom_text(data = aes2, aes(x = Time, y = Sum.Exp, label = Sum.Exp),
      vjust = 0, hjust=1, nudge_x = 100, nudge_y = 35000, size = 3) +
  
      geom_point(data = coupledData, aes(x = coupledData[minIndex[1,1], minIndex[1,2] - 1],
      y = coupledData[minIndex[1,1], minIndex[1,2]]), size = 10, shape = "o", color = "red") +
  
      geom_text(data = coupledData[minIndex[1,1], ], aes(x = Time, y = Sum.Exp, label = Sum.Exp),
      color = "red", size = 3.5, vjust = 0, hjust = 1, nudge_x = 150, nudge_y = -35000, check_overlap = TRUE) +
  
      labs(title = "Dependence of exports on the year", x = "Years", y = "Total exports, million of ECU/EURO")


