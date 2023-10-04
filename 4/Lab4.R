library(GGally)
library(tidyverse)
library(corrplot)
library(patchwork)
library(lmtest)
library(MASS)

df <- read.csv("dados_mundiais.csv")[,-1]
dfm <- df[,-length(df)]
dfm <- dfm[,-5]
`RendaMensal` <- df$RendaMensal
dfm <- data.frame(dfm,RendaMensal)
View(dfm)

# 1
var_df = c(121.39,1275571.96,65.22,4191636.98,43.57,41.65,50.28,11.68)
var_df/colMeans(df[2:9],na.rm = TRUE)
summary(df[2:9])
var(df[2:9],na.rm = TRUE)
var(df[2:9])


Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

apply(df[2:9], 2, Mode)

colnames(dfm)[2]<-'QI'

ggpairs(dfm[,2:length(dfm)])


boxp <- function(y,name){
  ggplot(df)+
    geom_boxplot(aes(y=df[,y]))+
    labs(y=name)+
    theme_minimal()
}

boxp(2,"QI") / boxp(3,"Despesas com educação") | boxp(4,"Temperatura máxima diária") /
  boxp(5,"Renda mensal") | boxp(6,"Expectativa de vida masculina") /
  boxp(7,"Expectativa de vida feminina") | boxp(8,"Taxa de natalidade") /
  boxp(9,"Taxa de mortalidade")

C <- cor(df[,3:length(df)-1],use='complete.obs')
corrplot(C, type = 'lower', cl.pos = 'n')


# 2

boxp <- function(y,name){
  ggplot(df)+
    geom_boxplot(aes(y=df[,y],x=Continente))+
    labs(y=name)+
    theme_minimal()
}

boxp(2,"QI") / boxp(3,"Despesas com educação") | boxp(4,"Temperatura máxima diária") /
  boxp(5,"Renda mensal") | boxp(6,"Expectativa de vida masculina") /
  boxp(7,"Expectativa de vida feminina") | boxp(8,"Taxa de natalidade") /
  boxp(9,"Taxa de mortalidade")



# 3




# 4

disp <- function(x,name){
  ggplot(df)+
    geom_point(aes(y=RendaMensal,x=df[,x]))+
    labs(y="Renda mensal",x=name)+
    theme_minimal()
}

disp(2,"QI") / disp(3,"Despesas com educação") | disp(4,"Temperatura máxima diária") /
  disp(5,"Renda mensal") | disp(6,"Expectativa de vida masculina") /
  disp(7,"Expectativa de vida feminina") | disp(8,"Taxa de natalidade") /
  disp(9,"Taxa de mortalidade")




# 5 e 6
#IQ
model1 <- lm(RendaMensal~IQ,data=df)
summary(model1)
plot(model1)

shapiro.test(model1$residuals)
bptest(model1)
dwtest(model1)


#educacao
model2 <- lm(RendaMensal~Educacao,data=df) 
summary(model2)
plot(model2)

shapiro.test(model2$residuals)
bptest(model2)
dwtest(model2)


#Temperatura
model3 <- lm(RendaMensal~TempMax,data=df) 
summary(model3)
plot(model3)

shapiro.test(model3$residuals)
bptest(model3)
dwtest(model3)

#expectativa masculina
model4 <- lm(RendaMensal~ExpectHomens,data=df) 
summary(model4)
plot(model4)

shapiro.test(model4$residuals)
bptest(model4)
dwtest(model4)

#expectatica feminina
model5 <- lm(RendaMensal~ExpectMulheres,data=df) 
summary(model5)
plot(model5)

shapiro.test(model5$residuals)
bptest(model5)
dwtest(model5)

#natalidade
model6 <- lm(RendaMensal~Natalidade,data=df) 
summary(model6)
plot(model6)

shapiro.test(model6$residuals)
bptest(model6)
dwtest(model6)

#mortalidade
model7 <- lm(RendaMensal~Mortalidade,data=df) 
summary(model7)
plot(model7)

shapiro.test(model7$residuals)
bptest(model7)
dwtest(model7)

#continente
model8 <- aov(RendaMensal~Continente,data=df) 
summary(model8)
plot(model8)

shapiro.test(model8$residuals)
bptest(model8)
dwtest(model8)



point <- function(y,name){
  ggplot(df, aes(x=df[,y],y=RendaMensal))+
    geom_point() +
    geom_smooth(method = "lm",se=TRUE)+
    labs(x=name,y="Renda mensal")+
    theme_minimal()
}


point(2,"QI") / point(3,"Despesas com educação") | point(4,"Temperatura máxima diária") / point(6,"Expectativa de vida masculina") |
  point(7,"Expectativa de vida feminina") / point(8,"Taxa de natalidade")



########################## boxcox ##############################################


library(MASS)
#QI
bc <- boxcox(model1,data=df)
plot(bc)
(lambda <- bc$x[which.max(bc$y)])

model1m <- lm(log(RendaMensal)~IQ,data=df)
summary(model1m)
plot(model1m)

shapiro.test(model1m$residuals)
bptest(model1m)
dwtest(model1m)

#educacao
bc2 = boxcox(model2,data=df)
plot(bc2)
bc2$x[which.max(bc2$y)]
bc2$x
model2m <- lm((RendaMensal)~Educacao,data=df) 
summary(model2m)
plot(model2m)

shapiro.test(model2m$residuals)
bptest(model2m)
dwtest(model2m)


#Temperatura
bc3 = boxcox(model3,data=df)
plot(bc3)
bc3$x[which.max(bc3$y)]

model3m <- lm(log(RendaMensal)~TempMax,data=df) 
summary(model3m)
plot(model3m)

shapiro.test(model3m$residuals)
bptest(model3m)
dwtest(model3m)


#expectativa masc
bc4 = boxcox(model4,data=df)
plot(bc4)
bc4$x[which.max(bc4$y)]

model4m <- lm(log(RendaMensal)~ExpectHomens,data=df) 
summary(model4m)
plot(model4m)

ggplot(df,aes(ExpectHomens,log(RendaMensal))) + 
  geom_point()+
  geom_smooth(method=lm)

shapiro.test(model4m$residuals)
bptest(model4m)
dwtest(model4m)


#expectativa fem.
bc5 = boxcox(model5,data=df)
plot(bc5)
bc5$x[which.max(bc5$y)]

model5m <- lm(log(RendaMensal)~ExpectMulheres,data=df) 
summary(model5m)
plot(model5m)

shapiro.test(model5m$residuals)
bptest(model5m)
dwtest(model5m)


#Natalidade
bc6 = boxcox(model6,data=df)
plot(bc6)
bc6$x[which.max(bc6$y)]

model6m <- lm(log(RendaMensal)~Natalidade,data=df) 
summary(model6m)
plot(model6m)

shapiro.test(model6m$residuals)
bptest(model6m)
dwtest(model6m)


#Mortalidade
bc7 = boxcox(model7,data=df)
plot(bc7)
bc7$x[which.max(bc7$y)]

model7m <- lm(log(RendaMensal)~Mortalidade,data=df) 
summary(model7m)
plot(model7m)

shapiro.test(model7m$residuals)
bptest(model7m)
dwtest(model7m)


#continente
bc7 = boxcox(model7,data=df)
plot(bc7)
bc7$x[which.max(bc7$y)]

model8m <- aov(log(RendaMensal)~Continente,data=df) 
summary(model8m)
plot(model8m)

shapiro.test(model8m$residuals)
bptest(model8m)
dwtest(model8m)






point <- function(y,name){
  ggplot(df, aes(x=df[,y],y=log(RendaMensal)))+
    geom_point() +
    geom_smooth(method = "lm",se=TRUE)+
    labs(x=name,y="Log. da renda mensal")+
    theme_minimal()
}


(point(2,"QI") |  point(4,"Temperatura máxima diária")) / (
  point(6,"Expectativa de vida masculina") |
  point(7,"Expectativa de vida feminina") | point(8,"Taxa de natalidade"))



# análise dos resíduos - expectativa de vida masculina

a <- ggplot(model4m)+
  geom_point(aes(x=.fitted,y=.resid))+
  labs(x="Valores preditos \n A",y="Resíduos")+
  theme_minimal()

b <- ggplot(model4m, aes(sample=studres(model4m)))+
  geom_qq()+
  geom_qq_line(color="red")+
  labs(x="Quantil teórico \n B", y="Resíduos studentizados")+
  theme_minimal()

c <- ggplot(model4m)+
  geom_histogram(aes(x=.resid),binwidth=0.4,color="black", fill="white")+
  labs(y="Frequência",x="Resíduos \n C")+
  theme_minimal()

a | b |c

shapiro.test(model4m$residuals)
lmtest::bptest(model4m, studentize = FALSE)
dwtest(model4m)
# ------------------------- Regressão Linear Múltiplo ----------------------------
df


modelmp = lm(log(RendaMensal) ~ IQ + Educacao  + TempMax +  ExpectHomens+ 
     ExpectMulheres + Natalidade + Mortalidade,df)
summary(modelmp)
plot(modelmp)
shapiro.test(modelmp$residuals)
bptest(modelmp)
dwtest(modelmp)


#boxcox(modelmp,data=df)
modelmp2 = lm(log(RendaMensal) ~  Educacao +   ExpectHomens ,df)
summary(modelmp2)
plot(modelmp2)
shapiro.test(modelmp2$residuals)
bptest(modelmp2)
dwtest(modelmp2)
