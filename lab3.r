library(ggplot2)

anscombe

cor(anscombe$x1, anscombe$y1)

lin_req <- lm(formula = anscombe$y1 ~ anscombe$x1)

summary(lin_req)

lin_req$fitted.values
#оцінюємо розподіл залишків (різниця між реальними даними та модельними)

anscombe$y1 - lin_req$fitted.values
#визначаємо залишки

lin_req$residuals
#визначаємо залишки

anscombe$residuals_lm1 <-  lin_req$residuals
ggplot(anscombe, aes(x =  residuals_lm1)) +
  geom_dotplot(fill=  "orange")
#точковий графік

qqnorm(lin_req$residuals, col=  "orange", pch=  20)
qqline(lin_req$residuals, col =  "blue")
#Для оцінки нормальності розподілу, будемо використовувати функції qqnorm та qqline

anscombe$fitted_lm1 <-  lin_req$fitted.values
ggplot(data=  anscombe, aes(x=  fitted_lm1, y=  residuals_lm1)) +
  geom_point(col=  "orange")
#Оцінюємо варіативність залишків

str(diamonds)

ggplot(data=  diamonds, aes(x=  carat, y=  price)) +
  geom_point(col=  "lightblue")

cor(x = diamonds$carat, y = diamonds$price)

ggplot(data=  diamonds, aes(x=  carat, y=  price, col=  cut)) +
  geom_point()
# як розподілені вага та ціна в залежності від ступеня обробки діамантів

ggplot(data=  diamonds, aes(x=  carat, y=  price)) +
  geom_point(col=  "lightblue") +
  facet_wrap(~cut)
#Інший тип відображення

ggplot(data=  diamonds, aes(x=  carat, y=  price)) +
  geom_point(col=  "lightblue") +
  geom_smooth(method=  "lm", se=  FALSE) +
  facet_wrap(~cut)
#Додамо до графіка ще лінію лінійної регресії

dim_ideal <- diamonds %>%	
  filter(cut == "Ideal")

dim_fair <- diamonds %>%	
  filter(cut == "Fair")

ideal <- lm(formula = dim_ideal$price ~ dim_ideal$carat)
summary(ideal)

fair <- lm(formula = dim_fair$price ~ dim_fair$carat)
summary(fair)
