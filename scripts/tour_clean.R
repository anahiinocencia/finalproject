library(tidyverse)
library(readxl)


rm(list=ls())

tourism <- read_excel("./data/tourism.xlsx")

tourism$accommodation <-  factor( tourism$accommodation , 
                   labels =   c("*", "**","***","****","*****" )
                   )

tourism$price <- factor( tourism$price , 
                 labels =   c("Expensive", "Fair", "Cheap" ))

tourism$recommendation <-  factor( tourism$recommendation , 
              labels =   c("Yes", "Rather yes","Rather no","No" ))

tourism$skiholiday <- factor( tourism$skiholiday , 
                        labels = c("No", "Yes" ))

tourism$sex <- factor( tourism$sex , 
                              labels = c("Male", "Female" ))

tourism$country  <- factor( tourism$country , 
        labels = c("Switzerland", "Germany","Austria","Other" ))

tourism$education  <- factor( tourism$education , 
                  labels = c("Secondary", "A-level","Bachelor","Master" ))


table(as.factor(tourism$education))
table(x)


tourism |> ggplot(mapping = aes(x = satisfaction)) +
  geom_histogram(binwidth = 10) +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 8) +
  labs( title = "Satisfacción General") +
  xlab("Nivel de satisfacción") +
  ylab("Frecuencia")

 ggplot(mapping = aes(x = satisfaction)) +
  geom_density() +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 8) +
  labs( title = "Satisfacción General") +
  xlab("Nivel de satisfacción") +
  ylab("Frecuencia")

tourism |> ggplot( aes(x = sex, y = satisfaction )) +
  geom_boxplot() +
  scale_y_continuous(n.breaks = 10) +
  labs( title = "Satisfacción General", subtitle = "Comparación por genero") +
  xlab("Genero") +
  ylab("Satisfacción")

tourism |> ggplot( aes(x = sex, y = expenses )) +
  geom_boxplot() +
  scale_y_continuous(n.breaks = 10) +
  labs( title = "Gastos en la estadía", subtitle = "Comparación por genero") +
  xlab("Género") +
  ylab("Gastos (CHF)")

ggplot(data = tourism,
       mapping = aes(x = stay) ) + 
  geom_histogram(binwidth = 1) +
  scale_x_continuous(n.breaks = 12) +
  scale_y_continuous(n.breaks = 8) +
  labs( title = "Histograma de Frecuencia") +
  xlab("Dias hospedaje") +
  ylab("Frecuencia")


ggplot(data = tourism,
       mapping = aes(x = stay) ) + 
 geom_density() +
  scale_x_continuous(n.breaks = 12) +
  scale_y_continuous(n.breaks = 8) +
  labs( title = "Histograma de Frecuencia") +
  xlab("Dias hospedaje") +
  ylab("Frecuencia")


ggplot(data = tourism,
       mapping = aes(x = expenses, colour = sex) ) + 
  geom_density() +
  scale_x_continuous(n.breaks = 12) +
  scale_y_continuous(n.breaks = 8) +
  labs( title = "Costo de la estancia",
        subtitle = "Diferencias por género") +
  xlab("Gasto (CHF)") +
  ylab("Frecuencia")
  
  ggplot(data = tourism,
         mapping = aes(x = recommendation) ) + 
    geom_bar() +
    scale_y_continuous(n.breaks = 8) +
    labs( title = "Recomendaría el Resort") +
    xlab("Opinion") +
    ylab("Frecuencia")
  
ggplot(data = tourism,
         mapping = aes(x = recommendation, fill = sex) ) + 
    geom_bar( position = "stack") +
    scale_y_continuous(n.breaks = 10) +
    labs( title = "Recomendaría el Resort", subtitle = "Diferencias por género") +
    xlab("Opinion") +
    ylab("Frecuencia")

ggplot(data = tourism,
       mapping = aes(x = age, y = expenses) ) + 
  geom_point() +
  scale_y_continuous(n.breaks = 10) +
  labs( title = "Dinero invertido", subtitle = "Diferencias por edad") +
  xlab("Edad") +
  ylab("Inversión")

ggplot(data = tourism,
       mapping = aes(x = age, y = expenses) ) + 
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_continuous(n.breaks = 10) +
  labs( title = "Dinero invertido", subtitle = "Diferencias por edad") +
  xlab("Edad") +
  ylab("Inversión")

ggplot(data = tourism,
       mapping = aes(x = age, y = expenses) ) + 
  geom_point(aes(colour = sex, shape = country)) +
  geom_smooth(method = "lm") +
  scale_y_continuous(n.breaks = 10) +
  labs( title = "Dinero invertido", subtitle = "Diferencias por edad") +
  xlab("Edad") +
  ylab("Inversión")

ggplot(data = tourism,
       mapping = aes(x = age, y = expenses) ) + 
  geom_point(aes(colour = sex)) +
  geom_smooth(method = "lm") +
  scale_y_continuous(n.breaks = 10) +
  labs( title = "Dinero invertido", subtitle = "Diferencias por edad") +
  xlab("Edad") +
  ylab("Inversión") +
  facet_wrap(~country)


ggplot(data = tourism,
       mapping = aes(x = sex, y = expenses, fill = sex) ) + 
  geom_boxplot() +
  scale_y_continuous(n.breaks = 10) +
  labs( title = "Dinero invertido", 
        subtitle = "Diferencias por país y género") +
  xlab("Género") +
  ylab("Inversión") +
  facet_wrap(~country)




