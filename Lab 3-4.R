#Пользуясь примером из лекции файл (6.0.R) проанализируйте данные
#о возрасте и физ. характеристиках молюсков
#https://archive.ics.uci.edu/ml/datasets/abalone
data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data", header=TRUE, sep=",")
summary(data)
colnames(data)
colnames(data) <- c("sex", "length", "diameter", "height", 
                    "whole_weight", "shucked_weight",
                    "viscera_weight", "shell_weight", "rings")

colnames(data)
data$sex <- as.factor(data$sex)
par(mfrow=c(1,4)) 
hist(data$diameter, main = "Диаметр, мм")
hist(data$height, main = "Высота, мм")
hist(data$whole_weight, main = "Полный вес, гр")
hist(data$rings, main = "Кол-во, шт")
#Видим ассиметрию https://en.wikipedia.org/wiki/Skewness
#и выбросы (от них нужно избавиться)
plot(data$diameter)
boxplot(data$whole_weight)
boxplot(data$height)
boxplot(data$rings)

#Найти строки с выбросами и с помо
data<-data[data$height<=0.18&data$height>0.08,]
hist(data$height)

data<-data[data$diameter<=0.52&data$diameter>0.26,]
hist(data$diameter)

data<-data[data$rings<=15.0&data$rings>4.0,]
hist(data$rings)


#Визулизируем возможные зависимости
par(mfrow=c(1,3)) 
plot(data$diameter, data$whole_weight,'p',main = "Зависимость веса от диаметра")
plot(data$height, data$whole_weight,'p',main = "Зависимость веса от высоты")
plot(data$rings, data$whole_weight,'p',main = "Зависимость веса от кол-ва колец")


#Хорошо видна зависимость, нужно её исследовать
#построить линейные модели при помощи функции lm, посмотреть их характеристики
linear.model.1 <- lm(data$diameter ~ data$whole_weight)
summary(linear.model.1)
plot(linear.model.1)

linear.model.2 <- lm(data$height ~ data$whole_weight)
summary(linear.model.2)
plot(linear.model.2)

linear.model.3 <- lm(data$rings ~ data$whole_weight)
summary(linear.model.3)
plot(linear.model.3)

#разделить массив данных на 2 случайные части

odds <- seq(1, nrow(data), by=2)
keep.row <- data[odds,]
delete.row <- data[-odds,]


#подогнать модель по первой части
keep.model.1 <- lm(keep.row$diameter ~ keep.row$whole_weight)
summary(keep.model.1)

keep.model.2 <- lm(keep.row$height ~ keep.row$whole_weight)
summary(keep.model.2)

keep.model.3 <- lm(keep.row$rings ~ keep.row$whole_weight)
summary(keep.model.3)

#спрогнозировать (функция predict) значения во второй части
predict1 <- predict(keep.model.1)
cor(keep.row$whole_weight, predict1)
plot(keep.row$whole_weight, predict1)

predict2 <- predict(keep.model.2)
cor(keep.row$whole_weight, predict2)
plot(keep.row$whole_weight,predict2)

predict3 <- predict(keep.model.3)
cor(keep.row$whole_weight, predict3)
plot(keep.row$whole_weight,predict3)

#проверить качество прогноза
total.predict.1 <- predict(keep.model.1, newdata = delete.row)
cor(delete.row$whole_weight, total.predict.1)
plot(delete.row$whole_weight, total.predict.1)

total.predict.2 <- predict(keep.model.2, newdata = delete.row)
cor(delete.row$whole_weight, total.predict.2)
plot(delete.row$whole_weight, total.predict.2)

total.predict.3 <- predict(keep.model.3, newdata = delete.row)
cor(delete.row$whole_weight, total.predict.3)
plot(delete.row$whole_weight, total.predict.3)