library(tidyverse)
library(GGally)
library(gridExtra)
library(broom)
library(gmodels)
library(car)
library(lmtest)
dados <- read.csv("desemprego.csv",sep=";")
colnames(dados) <- c("Ano","Desemprego","Suicídio")


ggpairs(dados)

summary(dados)
table(dados[,3])

a <- ggplot(dados)+
  geom_boxplot(aes(y=Desemprego))+
  theme_minimal()

b <- ggplot(dados)+
  geom_boxplot(aes(y=Suicídio))+
  theme_minimal()

grid.arrange(a,b,ncol=2)


a <- ggplot(dados)+
  geom_histogram(aes(x=Desemprego),binwidth=0.5,color="black", fill="white")+
  labs(y="Frequência")+
  theme_minimal()

b <- ggplot(dados)+
  geom_histogram(aes(x=Suicídio),binwidth=0.5,color="black", fill="white")+
  labs(y="Frequência")+
  theme_minimal()

grid.arrange(a,b,ncol=2)

## Com os "pontos atípicos"

model <- lm(Suicídio~Desemprego, data = dados)
summary(model)
confint(model)
#n = as.numeric(count(dados))
#0.16281 - 0.07889*qt(0.975,n-2)
#0.16281 + 0.07889*qt(0.975,n-2)

plot(model)

ggplot(dados, aes(x=Desemprego,y=Suicídio))+
  geom_point() +
  geom_smooth(method = "lm",se=TRUE)+
  theme_minimal()

a <- ggplot(model)+
  geom_point(aes(x=.fitted,y=.resid))+
  labs(x="Valores preditos \n A",y="Resíduos")+
  theme_minimal()


b <- ggplot(model, aes(sample=studres(model)))+
  geom_qq()+
  geom_qq_line(color="red")+
  labs(x="Quantil teórico \n B", y="Resíduos studentizados")+
  theme_minimal()

c <- ggplot(model)+
  geom_histogram(aes(x=.resid),binwidth=0.4,color="black", fill="white")+
  labs(y="Frequência",x="Resíduos \n C")+
  theme_minimal()

grid.arrange(a,b,c,ncol=3)


shapiro.test(model$residuals)
lmtest::bptest(model, studentize = FALSE)
durbinWatsonTest(model)



## Sem os "pontos atípicos"

df <- dados[-(52:54),]

model <- lm(Suicídio~Desemprego, data = df)
summary(model)

plot(model)


ggplot(df, aes(x=Desemprego,y=Suicídio))+
  geom_point() +
  geom_smooth(method = "lm",se=TRUE)+
  theme_minimal()



a <- ggplot(model)+
  geom_point(aes(x=.fitted,y=.resid))+
  labs(x="Valores preditos \n A",y="Resíduos")+
  theme_minimal()

b <- ggplot(model, aes(sample=studres(model)))+
  geom_qq()+
  geom_qq_line(color="red")+
  labs(x="Quantil teórico \n B", y="Resíduos studentizados")+
  theme_minimal()

c <- ggplot(model)+
  geom_histogram(aes(x=.resid),binwidth=0.4,color="black", fill="white")+
  labs(y="Frequência",x="Resíduos \n C")+
  theme_minimal()

grid.arrange(a,b,c,ncol=3)

shapiro.test(model$residuals)
lmtest::bptest(model, studentize = FALSE)
durbinWatsonTest(model)

plot(seq(1,length(model$residuals)),model$residuals)

ggplot(model,aes(y=model$residuals,x=seq(1,length(model$residuals))))+
  geom_point()+
  labs(y="Resíduos",x="Ordem de coleta")+
  theme_minimal()

summary(model)
kable(glance(model, conf.int = TRUE)[1:5])
