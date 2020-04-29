# Pedro Concejero pedro.concejerocerezo@gmail.com
# para UTAD https://www.u-tad.com/
# 29 de abril de 2020

# Script para visualizar muertes por COVID-19 por edad y sexo
# Infinitas gracias al grandísimo trabajo de datadista para 
# coordinar y compartir los datos de esta pandemia
# https://datadista.com/
# https://github.com/datadista/datasets/tree/master/COVID%2019

library(tidyverse)

url <- "https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/nacional_covid19_rango_edad.csv"

data <- read_csv(url)

names(data)[1] <- "fecha"

summary(data)
head(data)
str(data)

View(data)

# se trata de las muertes *acumuladas* hasta el día de la fecha
# esto es importante porque entonces se tratará de hacer gráficos
# eligiendo la fecha para la cual se quiere mostar esa suma o acumulado de fallecidos

fecha <- max(data$fecha)
fecha

cond_ambos <- {data$fecha == fecha & 
    data$sexo == "ambos" & 
    data$rango_edad != "Total"}

data_para_plot <- data[cond_ambos, ]
View(data_para_plot)

barplot(data_para_plot$fallecidos)

# Empecemos a hacer gráficos bonitos con ggplot

p <- ggplot(data_para_plot,
            aes(rango_edad, fallecidos))

p + geom_col()

p + geom_bar(stat = "identity")

p + geom_bar(stat = "identity") + theme_classic()

p + geom_bar(stat = "identity", fill="steelblue") + 
  theme_minimal() + coord_flip()

# Hagamos ahora gráficos con facetas por sexo

cond <- {data$fecha == fecha & 
    data$rango_edad != "Total"}

data_para_plot <- data[cond, ]
View(data_para_plot)

p <- ggplot(data_para_plot,
            aes(rango_edad, fallecidos))

p + geom_col()

p + geom_col(fill="steelblue") + 
  theme_minimal() + 
  facet_grid(~ sexo)

p + geom_col(fill="steelblue") + 
  theme_minimal() + 
  facet_grid(rows = vars(sexo) )

p + geom_col(fill="steelblue") + 
  theme_minimal() + 
  facet_grid(cols = vars(sexo)) 

def <- p + geom_col(fill="steelblue") + 
  theme_minimal() + 
  facet_grid(cols = vars(sexo)) 

def + ggtitle("Muertes por COVID-19 en España")

title <- paste("Muertes por COVID-19 en España",
               "\n",
               "entre",
               min(data$fecha),
               "y",
               max(data$fecha))

print(title)

def + ggtitle(title)

# Otra alternativa dodged bar charts

p <- ggplot(data_para_plot[data_para_plot$sexo != "ambos", ],
            aes(rango_edad, fallecidos,
                fill = as.factor(sexo)))

p + geom_bar(position = "dodge",
             stat = "identity")

p + geom_bar(position = "dodge", 
             stat = "identity") + coord_flip()

def2 <- p + geom_bar(position = "dodge",
             stat = "identity") + coord_flip()

def2 + ggtitle(paste(title, "\n", "por género"))

               