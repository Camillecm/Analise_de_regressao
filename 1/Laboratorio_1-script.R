library(gridExtra)
library(GGally)
library(tidyverse)
dados <- read.csv("Dados_Lab01.csv",sep=";")
attach(dados)


ggpairs(dados[,1:8], diag = list(discrete = "barDiag"), title = "Gráficos de dispersão e correlação das variáveis acerca das mulheres de Pina da cidade de Phoenix no estado Arizona")


a <- ggplot(dados)+
  geom_histogram(aes(x=pregnant))+
  theme_minimal()

b <- ggplot(dados)+
  geom_histogram(aes(x=glucose))+
  theme_minimal()

c <- ggplot(dados)+
  geom_histogram(aes(x=diastolic))+
  theme_minimal()

d <- ggplot(dados)+
  geom_histogram(aes(x=triceps))+
  theme_minimal()

e <- ggplot(dados)+
  geom_histogram(aes(x=insulin))+
  theme_minimal()

f <- ggplot(dados)+
  geom_histogram(aes(x=bmi))+
  theme_minimal()

g <- ggplot(dados)+
  geom_histogram(aes(x=diabetes))+
  theme_minimal()

h <- ggplot(dados)+
  geom_histogram(aes(x=age))+
  theme_minimal()

i <- ggplot(dados)+
  geom_histogram(aes(x=test))+
  theme_minimal()

summary(dados[,1:8])

a <-subset(dados, dados$insulin > 100)
nrow(a)/nrow(dados)

a <-subset(dados, dados$bmi > 24.9)
nrow(a)/nrow(dados)

table(dados$test)
268/nrow(dados)

boxplot(dados[,1:8], main = "Boxplots das variáveis acerca das mulheres de Pina da cidade de Phoenix no estado Arizona")

grid.arrange(a,b,c,d,e,f,g,h,i,ncol=3, nrow=3)

# 2

df <- dados |>
  filter(glucose != 0, diastolic != 0, triceps != 0, insulin != 0, bmi != 0, insulin != 0, diabetes != 0)

a <-subset(df, df$insulin > 100)
nrow(a)/nrow(df)

ggpairs(df[,1:8], diag = list(discrete = "barDiag"), title = "Gráficos de dispersão e correlação das variáveis acerca das mulheres de Pina da cidade de Phoenix no estado Arizona")

a <- ggplot(df)+
  geom_histogram(aes(x=pregnant))+
  theme_minimal()

b <- ggplot(df)+
  geom_histogram(aes(x=glucose))+
  theme_minimal()

c <- ggplot(df)+
  geom_histogram(aes(x=diastolic))+
  theme_minimal()

d <- ggplot(df)+
  geom_histogram(aes(x=triceps))+
  theme_minimal()

e <- ggplot(df)+
  geom_histogram(aes(x=insulin))+
  theme_minimal()

f <- ggplot(df)+
  geom_histogram(aes(x=bmi))+
  theme_minimal()

g <- ggplot(df)+
  geom_histogram(aes(x=diabetes))+
  theme_minimal()

h <- ggplot(df)+
  geom_histogram(aes(x=age))+
  theme_minimal()

i <- ggplot(df)+
  geom_histogram(aes(x=test))+
  theme_minimal()

a <-subset(df, df$diastolic < 85)
nrow(a)/nrow(df)

summary(df[,1:8])

grid.arrange(a,b,c,d,e,f,g,h,i,ncol=3, nrow=3)

boxplot(df[,1:8], main = "Boxplots das variáveis acerca das mulheres de Pina da cidade de Phoenix no estado Arizona")

# 3

a <- ggplot(df)+
  geom_point(aes(x=pregnant,y=diabetes))+
  theme_minimal()

b <- ggplot(df)+
  geom_point(aes(x=glucose,y=diabetes))+
  theme_minimal()

c <- ggplot(df)+
  geom_point(aes(x=diastolic,y=diabetes))+
  theme_minimal()

d <- ggplot(df)+
  geom_point(aes(x=triceps,y=diabetes))+
  theme_minimal()

e <- ggplot(df)+
  geom_point(aes(x=insulin,y=diabetes))+
  theme_minimal()

f <- ggplot(df)+
  geom_point(aes(x=bmi,y=diabetes))+
  theme_minimal()

g <- ggplot(df)+
  geom_point(aes(x=diabetes,y=diabetes))+
  theme_minimal()

h <- ggplot(df)+
  geom_point(aes(x=age,y=diabetes))+
  theme_minimal()

i <- ggplot(df)+
  geom_point(aes(x=test,y=diabetes))+
  theme_minimal()

grid.arrange(a,b,c,d,e,f,g,h,i,ncol=3, nrow=3)

# 4

a <- ggplot(df)+
  geom_boxplot(aes(x = test, y = pregnant,group=test))+
  theme_minimal()

b <- ggplot(df)+
  geom_boxplot(aes(x = test, y = glucose,group=test))+
  theme_minimal()

c <- ggplot(df)+
  geom_boxplot(aes(x = test, y = diastolic,group=test))+
  theme_minimal()

d <- ggplot(df)+
  geom_boxplot(aes(x = test, y = triceps,group=test))+
  theme_minimal()

e <- ggplot(df)+
  geom_boxplot(aes(x = test, y = insulin,group=test))+
  theme_minimal()

f <- ggplot(df)+
  geom_boxplot(aes(x = test, y = bmi,group=test))+
  theme_minimal()

g <- ggplot(df)+
  geom_boxplot(aes(x = test, y = diabetes,group=test))+
  theme_minimal()

h <- ggplot(df)+
  geom_boxplot(aes(x = test, y = age,group=test))+
  theme_minimal()

grid.arrange(a,b,c,d,e,f,g,h,ncol=2, nrow=4)
summary(df)
