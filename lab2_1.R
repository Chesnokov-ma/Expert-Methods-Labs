library(xlsx)
library(readxl)

#-----------------------------------------
# Чтение данных из эксель-таблиц
#-----------------------------------------

# Данные по оценкам учеников
df <- read_excel("school_1.xlsx", col_names = FALSE)  
colnames(df)[1] <- "Names"
colnames(df)[2] <- "T_1"
colnames(df)[3] <- "T_2"
colnames(df)[4] <- "T_3"
colnames(df)[5] <- "T_4"

as.integer(df$`T_1`)  # Первод всех оценок в int
as.integer(df$`T_2`)  # чтобы избежать ошибок
as.integer(df$`T_3`)

# Данные по максимальны баллам и весам
df1 <- read_excel("school_2.xlsx", col_names = FALSE) 
colnames(df1)[1] <- "Max_S"
colnames(df1)[2] <- "Weight"
as.integer(df1$'Max_S')
as.integer(df1$'Weight')

#-----------------------------------------
# Формирование столбцов с оценками
#-----------------------------------------

w_sum <- sum(df1$Weight)  # сумма всех весов

r1 <- c(df$T_1 / df1$Max_S[1] * df1$Weight[1] / w_sum)  # вектора с агрегированной оценкой
r2 <- c(df$T_2 / df1$Max_S[2] * df1$Weight[2] / w_sum)  # расчитанные по формуле
r3 <- c(df$T_3 / df1$Max_S[3] * df1$Weight[3] / w_sum)
r4 <- c(df$T_4 / df1$Max_S[4] * df1$Weight[4] / w_sum)

df$r1 <- r1   # добавление столбцов в df
df$r2 <- r2
df$r3 <- r3
df$r4 <- r4

rm(df1) # удаление ненужных переменных
rm(w_sum, r1, r2, r3, r4)

# итоговый рейтинг в десятичных дробях
df$Final_R <- df$r1 + df$r2 + df$r3 + df$r4
# итоговый рейтинг в процентах
df$Final_R_Percent <- df$Final_R * 100