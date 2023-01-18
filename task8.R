library(tidyverse)
library(ggplot2)

load(file='ExpImp.RData')

data <- filter(ExpImp, !str_detect(ExpImp$Регион, 'Федерация|федеральный округ|в том числе'))

for (col_name in colnames(data)[colnames(data) != "Регион"]) {
  data[[col_name]] <- as.numeric(gsub("-", NA, data[[col_name]]))
}

dataMet <- select(data, c("Регион", "МетЭкспорт", "МетИмпорт"))

fullExport <- select_at(data, vars(matches("Экспорт")))
fullImport <- select_at(data, vars(matches("Импорт")))

dataMet$СуммарныйЭкспорт <- rowSums(fullExport, na.rm = TRUE)
dataMet$СуммарныйИмпорт <- rowSums(fullImport, na.rm = TRUE)

dataMet <- dataMet[complete.cases(dataMet),]
coupledData <- select(dataMet, -c("МетЭкспорт", "МетИмпорт"))
dataMet <- select(dataMet, c("Регион", "МетЭкспорт", "МетИмпорт"))


plotData <- pivot_longer(dataMet, !Регион, names_to = "Показатель", values_to = "Значение")
ggplot(data = plotData, mapping = aes(x = Регион, y = Значение, fill = Показатель)) +
  geom_col(color = 'black', position = 'dodge') +
  
  scale_fill_manual(labels = c("Импорт", "Экспорт"), values = c("blue", "red")) +
  
  ggtitle("Экспорт и импорт металлургической промышленности") +
  
  ylab('Объем, млн долл. США') + theme(legend.position='top') +
  coord_flip()



dataMet[,"МетЭкспорт"] <- -dataMet[,"МетЭкспорт"]

plotData <- pivot_longer(dataMet, !Регион, names_to = "Показатель", values_to = "Значение")
ggplot(data = plotData, mapping = aes(x = Регион, y = Значение, fill = Показатель)) +
  geom_col(color = 'black', position = 'dodge') +
  
  scale_fill_manual(labels = c("Импорт", "Экспорт"), values = c("blue", "red")) +
  
  geom_text(data = plotData[plotData$Показатель == "МетЭкспорт",], aes(x = Регион, y = Значение, 
        label = paste(-Значение, " (", round(-Значение / coupledData$СуммарныйЭкспорт * 100, digits = 1), "%)", sep = "")), size = 2.5,
        hjust = 1, nudge_y = -50, nudge_x = 0.2, color = "dark red") +
  
  geom_text(data = plotData[plotData$Показатель == "МетИмпорт",], aes(x = Регион, y = Значение, 
        label = paste(Значение, " (", round(Значение / coupledData$СуммарныйИмпорт * 100, digits = 1), "%)", sep = "")), size = 2.5, 
        hjust = 0, nudge_y = 50, nudge_x = -0.2, color = "dark blue") +
  
  coord_flip(ylim = c(-6500, 7500)) +
  
  ggtitle("Экспорт и импорт металлургической промышленности") +
  
  ylab('Объем, млн долл. США') + theme(legend.position='top') +
  
  theme(plot.caption = element_text(hjust = 0))

