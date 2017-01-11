#Lab1

library(dplyr)	
library(ggplot2)

setwd("C:/Documents and Settings/Максуд/Мои документы/RStatistics/Week2")

flats <- read.csv("flats.csv", stringsAsFactors=FALSE, dec= ",", encoding="UTF-8")	
  #Параметр	encoding="UTF-8"	використовується	для	коректного	відображення	
  #кирилиці	у	OS	Windows.

glimpse(flats)
  #функція виводить основну інформацію

count(flats, Місто)
  #підраховує к-сть значень змінної місто в файлі

flats %>%	
  count(Місто) %>%	
  arrange(n)	
  #оператор	%>%,	який	дозволяє	застосувати	наступну	команду	до	результатів	виконання	поточної
  # arrange сортує значення змінної, отриманої в count

flats %>%	
  filter(Кімнат == 2) %>%	
  filter(Місто != "Києво-Святошинський") %>%	
  count(Місто) %>%	
  arrange(desc(n))
  #фільтрування значень змінної "кімнат" та "місто"

flats %>%	
  filter(Кімнат == 1) %>%	
  filter(Місто != "Києво-Святошинський") %>%	
  group_by(Місто) %>%	
  summarise(mean=mean(Загальна_площа), sd=sd(Загальна_площа))	
  #фільтруємо + групуємо по містам + виконуємо обрахунки

