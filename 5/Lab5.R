library(GGally)
library(tidyverse)
library(patchwork)
library(lmtest)
library(MASS)
library(datasets)
library(broom)
library(lindia)
library(dplyr)

# ----------------------------- Atividade 1 -----------------------------------#
trees = trees[,2:3]

kable(summary(trees))

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

apply(trees,2,Mode)
apply(trees, 2, var)

ggpairs(trees)

a <- ggplot(trees)+
  geom_boxplot(aes(y = Height))+
  scale_y_continuous("Altura")+
  scale_x_discrete(breaks = NULL) +
  theme_minimal()

b <- ggplot(trees)+
  geom_boxplot(aes(y = Volume))+
  scale_x_discrete(breaks = NULL) +
  theme_minimal()

a+b

a <- ggplot(trees)+
  geom_histogram(aes(x=Height),bins = 7,color="black", fill="white")+
  labs(x="Altura",y="Frequência")+
  theme_minimal()

  b <- ggplot(trees)+
    geom_histogram(aes(x=Volume),bins=8,binwidth=12,color="black", fill="white")+
    labs(y="Frequência")+
    theme_minimal()

a+b

#1a)
modelo = lm(Volume~Height,data=trees)
anova(modelo)
summary(modelo)

ggplot(trees, aes(x=Height,y=Volume))+
  geom_point() +
  labs(x="Altura")+
  geom_smooth(method = "lm")+
  theme_minimal()

shapiro.test(modelo$residuals)
gqtest(modelo)
dwtest(modelo)

#1b)


ggplot(modelo, aes(x =.fitted, y = rstudent(modelo))) +
  geom_point() +
  labs(x="Valores preditos\n A", y='Resíduos Jackknife')+
  theme_minimal()+
ggplot(modelo, aes(x =.fitted, y = r_jack)) +
  geom_point() +
  labs(x="Valores preditos\n A", y='Resíduos Jackknife')+
  theme_minimal() 
  
  
ggplot(modelo, aes(sample=rstudent(modelo)))+
  geom_qq()+
  geom_qq_line(color="red")+
  labs(x="Quantil teórico \n B", y="Resíduos Jackknife")+
  theme_minimal()


#1c)
modeloT1 = lm(sqrt(Volume)~Height,data=trees)
modeloT2 = lm(log(Volume)~Height,data=trees)
modeloT3 = lm(Volume^2~Height,data=trees)

summary(modeloT1)
summary(modeloT2)
summary(modeloT3)

ggplot(trees, aes(x=Height,y=sqrt(Volume)))+
  geom_point() +
  labs(x="Altura",y="Raiz quadrada do volume")+
  geom_smooth(method = "lm")+
  theme_minimal() +

ggplot(trees, aes(x=Height,y=log(Volume)))+
  geom_point() +
  labs(x="Altura",y="Log. do volume")+
  geom_smooth(method = "lm")+
  theme_minimal() +

ggplot(trees, aes(x=Height,y=(Volume^2)))+
  geom_point() +
  labs(x="Altura",y="Volume ao quadrado")+
  geom_smooth(method = "lm")+
  theme_minimal()


a <- ggplot(modeloT1, aes(x =.fitted, y = rstudent(modeloT1))) +
  geom_point() +
  geom_hline(yintercept = 0,col="red")+
  labs(x="Valores preditos \
       √Y", y='Resíduos Jackknife')+
  theme_minimal()

b <- ggplot(modeloT2, aes(x =.fitted, y = rstudent(modeloT2))) +
  geom_point() +
  geom_hline(yintercept = 0,col="red")+
  labs(x="Valores preditos \
       log(Y)", y='Resíduos Jackknife')+
  theme_minimal()

c <- ggplot(modeloT3, aes(x =.fitted, y = rstudent(modeloT3))) +
  geom_point() +
  geom_hline(yintercept = 0,col="red")+
  labs(x="Valores preditos \
       Y²", y='Resíduos Jackknife')+
  theme_minimal()

a+b+c

ggplot(modeloT1, aes(sample=rstudent(modeloT1)))+
  geom_qq()+
  geom_qq_line(color="red")+
  labs(x="Quantil teórico \n √Y", y="Resíduos Jackknife")+
  theme_minimal() +

ggplot(modeloT2, aes(sample=rstudent(modeloT2)))+
  geom_qq()+
  geom_qq_line(color="red")+
  labs(x="Quantil teórico \n log(Y)", y="Resíduos Jackknife")+
  theme_minimal() +

ggplot(modeloT3, aes(sample=rstudent(modeloT3)))+
  geom_qq()+
  geom_qq_line(color="red")+
  labs(x="Quantil teórico \n Y²", y="Resíduos Jackknife")+
  theme_minimal()

shapiro.test(modeloT1$residuals)
gqtest(modeloT1)
dwtest(modeloT1)

shapiro.test(modeloT2$residuals)
gqtest(modeloT2)
dwtest(modeloT2)

shapiro.test(modeloT3$residuals)
gqtest(modeloT3)
dwtest(modeloT3)

#1d)

bc = boxcox(modelo)
gg_boxcox(modelo, showlambda = TRUE, lambdaSF = 3, scale.factor = 0.5)+
  labs(title="",y="Log-verossimilhança")+
  theme_minimal()
lambda = bc$x[bc$y == max(bc$y)];lambda

modelobc = lm((Volume^(lambda)-1)/lambda~Height,data=trees)
summary(modelobc)
tidy(anova(modelobc))

shapiro.test(modelobc$residuals)
gqtest(modelobc)
dwtest(modelobc)

ggplot(trees, aes(x=Height,y=(Volume^(lambda)-1)))+
  geom_point() +
  labs(x="Altura",y="Transf. BOx-Cox do volume")+
  geom_smooth(method = "lm")+
  theme_minimal()

ggplot(modelobc, aes(x =.fitted, y = studres(modelobc))) +
  geom_point() +
  labs(x="Valores preditos \n A", y="Resíduos Jackknife")+
  theme_minimal()+

ggplot(modelobc, aes(sample=studres(modelobc)))+
  geom_qq()+
  geom_qq_line(color="red")+
  labs(x="Quantil teórico \n B", y="Resíduos Jackknife")+
  theme_minimal()




# ----------------------- Atividade 2 -----------------------
# a e b
dados <-  read.csv('Dados2.csv',sep=';',dec=",")

dados <- dados %>% 
  mutate(height = height/39.3701,
         weight = weight/2.20462,
         waist = waist/39.3701,
         hip= hip/39.3701)

#RCQ = Relação Cintura Quadril

dados <- dados %>% 
  mutate(IMC = weight/(height^2),
         RCQ = waist/hip)

dados <- dados %>% 
  mutate(chol = NULL,
         hdl = NULL,
         bp.2s = NULL,
         bp.2d = NULL,
         height = NULL,
         weight = NULL,
         waist = NULL,
         hip = NULL,
         location = NULL,
         frame = NULL,
         gender = NULL,
         time.ppn = NULL
         )

nrow(dados)

dados <- dados %>% 
  na.omit()

nrow(dados)

# analise descritiva
ggpairs(dados)
summary(dados)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

apply(dados, 2, Mode)

boxp <- function(y,name){
  ggplot(dados)+
    geom_boxplot(aes(y=dados[,y]))+
    labs(y=name)+
    theme_minimal()
}

boxp(1, "Glicose estabilizada") / boxp(2, "Razão col. total e col. bom") |
boxp(3,"Hemoglobina glicada") / boxp(4, "Idade") | boxp(5, "Pressão sanguínea sistólica") /
  boxp(6, "Pressão sanguínea diastólica") | boxp(7,"IMC") / boxp(8,"RCQ")

histo <- function(y,name){
  ggplot(dados)+
    geom_histogram(aes(x=dados[,y]),binwidth=10,color="black", fill="white")+
    labs(x=name,y="Frequência")+
    theme_minimal()
}

histog <- function(y,name){
  ggplot(dados)+
    geom_histogram(aes(x=dados[,y]),binwidth=15,color="black", fill="white")+
    labs(x=name,y="Frequência")+
    theme_minimal()
}

histog(1, "Glicose estabilizada") / histo(2, "Razão col. total e col. bom") |
  histo(3,"Hemoglobina glicada") / histo(4, "Idade") | histo(5, "Pressão sanguínea sistólica") /
  histo(6, "Pressão sanguínea diastólica") | histo(7,"IMC") / histo(8,"RCQ")


disp <- function(x,name){
  ggplot(dados)+
    geom_point(aes(y=IMC,x=dados[,x]))+
    labs(x=name)+
    theme_minimal()
}

disp(1, "Glicose estabilizada") + disp(2, "Razão col. total e col. bom")+
  disp(3,"Hemoglobina glicada") + disp(4, "Idade") + disp(5, "Pressão sanguínea sistólica") +
  disp(6, "Pressão sanguínea diastólica") + disp(8,"RCQ")


# e

model <- lm(IMC~stab.glu+ratio+glyhb+age+bp.1s+bp.1d+RCQ, data=dados)
summary(model)
tab <- augment(model)


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
  geom_histogram(aes(x=.resid),binwidth=2.2,color="black", fill="white")+
  labs(y="Frequência",x="Resíduos \n C")+
  theme_minimal()

a | b | c

shapiro.test(model$residuals)
gqtest(model)
dwtest(model)

dados$RCQ
plot(model)
