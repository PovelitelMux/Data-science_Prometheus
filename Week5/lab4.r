#LAB4

#Р§Рё РјР°С” С„Р°Р·Р° РјС–СЃСЏС†СЏ РІРїР»РёРІ РЅР° РєС–Р»СЊРєС–СЃС‚СЊ Р·Р»РѕС‡РёРЅС–РІ?

#lubridate - Р±С–Р±Р»С–РѕС‚РµРєР° РґР»СЏ РѕР±СЂРѕР±РєРё РґР°С‚Рё С‚Р° С‡Р°СЃСѓ
install.packages("lubridate")

library(lubridate)
library(dplyr)
library(ggplot2)

setwd("H:/R_СЃС‚Р°С‚РёСЃС‚РёРєР°/Week5")
getwd()
dir()

crime <- read.csv("crimes.csv" , header = TRUE)
str(crime)

#Р”Р»СЏ С„РѕСЂРјР°С‚СѓРІР°РЅРЅСЏ РґР°С‚Рё Р±СѓРґРµРјРѕ РІРёРєРѕСЂРёСЃС‚РѕРІСѓРІР°С‚Рё Р±С–Р±Р»С–РѕС‚РµРєСѓ lubridate, Р·РѕРєСЂРµРјР° С„СѓРЅРєС†С–СЋ ymd_hms(СЏРєР° РІРєР°Р·СѓС”,
#СЏРє РїРµСЂРµС‚РІРѕСЂРёС‚Рё СЃС‚СЂС–С‡РєСѓ РІ РґР°С‚Сѓ):
crime$POSIX <- ymd_hms(as.character(crime$Dates))
crime$Dates <- as.Date(ymd_hms(as.character(crime$Dates)))

#Р—Р°РІР°РЅС‚Р°Р¶РёРјРѕ РЅР°Р±С–СЂ РґР°РЅРёС… РїСЂРѕ С„Р°Р·Рё РњС–СЃСЏС†СЏ
moon <- read.csv("moon.csv", header = TRUE)

#РџРµСЂРµС‚РІРѕСЂРёРјРѕ РїРѕР»Рµ date РІ РѕРґРЅР°РєРѕРІС–Р№ С„РѕСЂРјР°С‚ С–Р· РїРѕР»РµРј Dates РІ crime:
moon$date <- as.Date(moon$date, "%m/%d/%Y")

#РћР±'С”РґРЅР°С”РјРѕ РЅР°Р±РѕСЂРё РґР°РЅРёС… Р·Р° РґР°С‚РѕСЋ.
full_data <- merge(crime, moon, by.x = "Dates", by.y="date")
#РћСЃРєС–Р»СЊРєРё РІ РЅР°СЃ РїРѕР»Рµ Р· РґР°С‚РѕСЋ РјР°С” СЂС–Р·РЅС– РЅР°Р·РІРё РІ РЅР°Р±РѕСЂР°С… РґР°РЅРёС…, РІРєР°Р¶РµРјРѕ С†С– С–РјРµРЅР° СЏРє РїР°СЂР°РјРµС‚СЂ
#by=c("Dates"="date")
full_data <- inner_join(crime, moon, by=c("Dates"="date"))

#РџРѕРґРёРІРёРјРѕСЃСЊ, СЏРє РІРёРіР»СЏРґР°С” РєС–Р»СЊРєС–СЃС‚СЊ Р·Р»РѕС‡РёРЅС–РІ РїРѕ РґРЅСЏС…:
date_phase <- full_data %>%
  group_by(Dates, phase) %>%
  count() %>%
  arrange(desc(n))
glimpse(date_phase)

ggplot(date_phase, aes(Dates, n)) +
  geom_line(alpha = 0.5) +
  labs(title = "Р—Р»РѕС‡РёРЅРё РІ РЎР°РЅ-Р¤СЂР°РЅС†РёСЃРєРѕ (2003-2015)",
       x = "Р”Р°С‚Р°",
       y = "РљС–Р»СЊРєС–СЃС‚СЊ Р·Р»РѕС‡РёРЅС–РІ") +
  geom_point(data = date_phase[date_phase$phase == "Full Moon", ], color = "red" ) +
  geom_smooth()

x <- mean(date_phase$n[date_phase$phase == "Full Moon"])
x
mu <- mean(date_phase$n[date_phase$phase != "Full Moon"])
mu
n <- length(date_phase$n[date_phase$phase == "Full Moon"])
n
s <- sd(date_phase$n[date_phase$phase == "Full Moon"])
s
p_value <- 2*pt(0.839, df=76, lower.tail = FALSE)
p_value

#РўР°РєРѕР¶ РјРѕР¶РµРјРѕ РІРёРєРѕСЂРёСЃС‚Р°С‚Рё С„СѓРЅРєС†С–СЋ t-test, РІРєР°Р·Р°РІС€Рё РІРµРєС‚РѕСЂ
#Р·РЅР°С‡РµРЅСЊ С‚Р° РїР°СЂР°РјРµС‚СЂРё mu = 391.75, alternative =
#"two.sided", conf.level = 0.95.
x_vector <- date_phase$n[date_phase$phase == "Full Moon"]
t.test(x_vector, mu = 391.75, alternative = "two.sided", conf.level = 0.95)

day_of_week_crimes <- full_data %>%
  group_by(DayOfWeek) %>%
  count()
glimpse(day_of_week_crimes)

ggplot(data=day_of_week_crimes, aes(x=DayOfWeek, y=n)) +
  geom_bar(stat="identity" , fill="lightblue")

day_of_week_crimes$DayOfWeek <- factor(day_of_week_crimes$DayOfWeek,
                                       levels = c("Monday", "Tuesday", "Wednesday",
                                                            "Thursday", "Friday", "Saturday", "Sunday"))
ggplot(data=day_of_week_crimes, aes(x=DayOfWeek, y=n)) +
  geom_bar(stat="identity" , fill="lightblue")

crimes_by_day <- full_data %>%
  group_by(Dates, DayOfWeek) %>%
  count()
str(crimes_by_day)
n <- length(crimes_by_day$n[crimes_by_day$DayOfWeek =="Friday"])
n
x <-mean(crimes_by_day$n[crimes_by_day$DayOfWeek=="Friday"])
x
s <-sd(crimes_by_day$n[crimes_by_day$DayOfWeek == "Friday"])
s
t <- (x-391.75)*sqrt(n)/s
t
p.value <- 2*pt(t, n-1, lower.tail = FALSE)
p.value
