# Visualization data
library(dplyr)	
library(ggplot2)

setwd("C:/Documents and Settings/Максуд/Мои документы/RStatistics/Week2")

flats <- read.csv("flats.csv", stringsAsFactors=FALSE, dec= ",", encoding="UTF-8")	

ggplot(flats, aes(x=Кімнат)) + 	
  geom_bar(fill="lightblue",	
           col="grey") +	
  ylab('Кількість')
  #Побудуємо	стовпчикову	діаграму. ми	додаємо	ще	один	рівень	(об'єднавши	їх	знаком	+	)	щоб	задати	geometric	об'єкт.

ggplot(flats, aes(x=Ціна)) + 	
  geom_histogram(breaks=seq(0, 12500000, by = 50000),	
                 fill="lightblue",	
                 col="grey") +	
  ylab('Кількість')	
  # побудуємо гістограму з кроком 50

ggplot(flats, aes(x=Загальна_площа, y=Ціна)) + 	
  geom_point()
  # побудова графіка розсіювання

flats1 <- flats %>%	
  filter(Кімнат == 1) %>%	
  filter(Місто != "Київ")

ggplot(flats1, aes(x=Місто, y=Ціна)) +
  geom_boxplot() +
  coord_flip() #переворот сис координат
  #побудова коробчатої діаграми
  