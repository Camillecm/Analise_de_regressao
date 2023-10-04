# 1
df1 <- read.csv("possum_red.csv", sep = ";")
library(tidyverse)
library(gridExtra)
library(GGally)


# a
# descrevendo o comportamento de cada uma das variaveis
summary(df1)

prop.table(table(df1$sex))

ggpairs(df1[,2:4],diag = list(discrete = "barDiag"))

a <- c(rep("skullw",nrow(df1)),rep("totlngth",nrow(df1)))
b <- c(skullw,totlngth)
c <- data.frame(a,b)
ggplot(c)+
  geom_boxplot(aes(x = a,y = b, group=a))+
  scale_x_discrete("Medidas morfológicas")+
  scale_y_continuous("Comprimento")+
  theme_minimal()

var(df1$skullw)
var(df1$totlngth)

# Sumarizando as informaçoes de acordo com o sexo
a <- ggplot(df1)+
  geom_boxplot(aes(x = sex,y = skullw,group=sex))+
  theme_minimal()

b <- ggplot(df1)+
  geom_boxplot(aes(x = sex,y = totlngth,group=sex))+
  theme_minimal()

grid.arrange(a,b,ncol=2)


df_f <- df1 |>
  filter(df1$sex=="f")

df_m <- df1 |>
  filter(df1$sex=="m")

summary(df_f)
summary(df_m)
var(df_f$skullw)
var(df_m$skullw)
var(df_f$totlngth)
var(df_m$totlngth)


# b
ggplot(df1)+
  geom_bar(aes(x=sex),binwidth=1,color="black", fill="white")+
  theme_minimal()


# c
a <- ggplot(df1)+
  geom_histogram(aes(x=skullw),binwidth=1,color="black", fill="white")+
  theme_minimal()

b <- ggplot(df1)+
  geom_histogram(aes(x=totlngth),binwidth=1,color="black", fill="white")+
  theme_minimal()

grid.arrange(a,b,ncol=2)


# d
ggplot(df1)+
  geom_point(aes(x = totlngth,y =skullw))+
  theme_minimal()

cor(df1[3],df1[4])


# e
#usar boxplot no item a

df1_whtout_out <- df1 |>
  filter(df1$skullw < (58.10 + 1.5*(58.10-54.98)), df1$skullw > (54.98 - 1.5*(58.10-54.98)))

summary(df1_whtout_out)

ggplot(df1_whtout_out)+
  geom_point(aes(x = totlngth,y =skullw))+
  theme_minimal()

cor(df1_whtout_out[3],df1_whtout_out[4])


# f e g
model <- lm(df1_whtout_out$totlngth ~ df1_whtout_out$skullw) 
summary(model)
plot(df1_whtout_out$totlngth ~ df1_whtout_out$skullw)
abline(19.422,1.197)

res <- lm(df1_whtout_out$totlngth ~ df1_whtout_out$skullw)$residuals
fit <- lm(df1_whtout_out$totlngth ~ df1_whtout_out$skullw)$fitted.values
boxplot(res)
hist(res)
plot(fit,res)

mean(res)
sum(res)

# ooouuu
ggplot(df1_whtout_out, aes(x=skullw,y=totlngth))+
  geom_point() +
  geom_smooth(method = "lm",se=FALSE)+
  theme_minimal()

a <- ggplot(model, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)+
  labs(x='Fitted Values', y='Residuals')+
  theme_minimal()

b <- ggplot(model, aes(x = .resid)) +
  geom_histogram(binwidth=1,color="black", fill="white") +
  labs(y='Count', x ='Residuals')+
  theme_minimal()

grid.arrange(a,b,ncol=2)





# 2
df2 <- read.csv("florida.csv", sep = ";")

# a
ggpairs(df2[,4:5],diag = list(discrete = "barDiag"))


# b
model <- lm(df2$BUCHANAN ~ df2$BUSH)
summary(model)

ggplot(df2, aes(x=BUSH,y=BUCHANAN))+
  geom_point() +
  geom_smooth(method = "lm",se=FALSE)+
  theme_minimal()

a <- ggplot(model, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)+
  labs(x='Fitted Values', y='Residuals')+
  theme_minimal()

b <- ggplot(model, aes(x = .resid)) +
  geom_histogram(binwidth=1,color="black", fill="white") +
  labs(y='Count', x ='Residuals')+
  theme_minimal()

grid.arrange(a,b,ncol=2)


# c
df2_SPA <- df2 |>
  filter(BUCHANAN < 3000, BUSH < 250000)

ggpairs(df2_SPA[,4:5],diag = list(discrete = "barDiag"))

model <- lm(df2_SPA$BUCHANAN ~ df2_SPA$BUSH) 
summary(model)


# d
ggplot(df2_SPA, aes(x=BUSH,y=BUCHANAN))+
  geom_point() +
  geom_smooth(method = "lm",se=FALSE)+
  theme_minimal()

a <- ggplot(model, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)+
  labs(x='Fitted Values', y='Residuals')+
  theme_minimal()

b <- ggplot(model, aes(x = .resid)) +
  geom_histogram(binwidth=1,color="black", fill="white") +
  labs(y='Count', x ='Residuals')+
  theme_minimal()

grid.arrange(a,b,ncol=2)

round(38.536279+0.004404*152846)


# e
x <- df2_SPA$BUSH
y <- df2_SPA$BUCHANAN
n <- nrow(df2_SPA)

b1 <- (sum(x*y)-((sum(x)*as.numeric(sum(y))))/n) /
  (sum(x^2)-(sum(x)^2)/n); b1
  
b0 <- mean(df2_SPA$BUCHANAN)-b1*mean(df2_SPA$BUSH); b0
