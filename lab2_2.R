library(xlsx)
library(readxl)
library(arrangements)
library(corrplot)
library(plotly)
library(ggplot2)

#----------------------------------------
# Пороговое агрегирование
#----------------------------------------

df <- read_excel("answers_1.xlsx", col_names = FALSE) # чтение excel файла

K <- ((ncol(df) + 1) * (ncol(df) + 2)) / 2  # расчет кол-ва классов эквивалентности
comb <- combinations(3, 13, replace = TRUE) # создание шкалы эквивалентности

cor_df <- cor(df, df, method = 'spearman') # расчет корреляции оценок

I <- c()
for (i in 1:nrow(df)) {               # цикл по каждому эксперту
  avec <- as.vector(df[i,])           # сортировка значений
  avec <- sort(avec)                  # для возможности сравнения с comb
  
  for (j in 1:K) {                    # цикл по всем классам эквивалентности
    res <- (avec == comb[j,])     
    if (all(res)) {                   # если значения совпадают
      I <- c(I, j)                    # номер класса сохранияется
      break
    }
  }
}

df$I <- I                     # добавление номера класса к df
df$v <- (df$I - 1) / (K - 1)  # расчет агрегированной оценки


fig_c <- plot_ly(z = cor_df, colors = colorRamp(c("yellow", "blue")), type = "heatmap")
fig_c    # график корреляции



#----------------------------------------
# Линейное агрегирование
#----------------------------------------

max <- c(3,3,3,3,3,3,3,3,3,3,3,3,3)         # макс значение
weight <- c(1,1,1,1,1,1,1,1,1,1,1,1,1)      # веса
w_sum <- sum(weight)

lin_v <- c()

for (i in 1:nrow(df)) {
  sum <- 0
  
  for (j in 1:13) {
    sum <- sum + (df[i, j, ] / max[j]) * (weight[j] / w_sum)    # расчет линейной оценки
  }
  # print(sum)
  lin_v <- c(lin_v, as.numeric(sum))
}

df$lin_v <- lin_v
df$exp_n <- c(1:nrow(df))

fig2 <- plot_ly(data = df, x = ~exp_n) %>%
  add_trace(y = ~v, mode = 'lines', name = 'Пороговое', type = 'scatter', line = list(width = 4))%>%
  add_trace(y = ~lin_v, mode = 'lines', name = 'Линейное', type = 'scatter', line = list(width = 4))%>%
  layout(title = 'Сравнение способов агрегирования')
fig2 <- fig2 %>% layout(legend=list(title=list(text='<b> Rate </b>')),
                        xaxis = list(title = "Номер эксперта"),
                        yaxis = list(title = "Агрегированная оценка"))
fig2


z <- c()
for (i in 1:nrow(comb)) {
  z <- c(z, 0)
}
n <- 1:nrow(comb)

df2 <- data.frame(n, z)

for (i in 1:nrow(df)) {
  df2$z[df$I[i]] <- (df2$z[df$I[i]] + 1)
}

fig1 <- plot_ly(data = df2, x = ~n) %>%
  add_trace(y = ~z, mode = 'lines', name = '1', type = 'scatter', line = list(width = 4))%>%
  layout(title = 'Использование классов эквивалентности')
fig1 <- fig1 %>% layout(legend=list(title=list(text='')),
                        xaxis = list(title = "Номер класса эквивалентности"),
                        yaxis = list(title = "Количество использований"))
fig1

