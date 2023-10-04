library(tidyverse)
library(GGally)
library(corrplot)
library(patchwork)
library(olsrr)
library(gridExtra)
library(gmodels)
library(qqplotr)
library(dplyr)


df <- read.csv("dados_mundiais.csv")[,-1]

df <- df|>
mutate(Continente = as.factor(Continente))

df$Continente = relevel(df$Continente,"Europa")
#df <- df[,-length(df)]

#corrplot(cor(df[2:9]))

cor(df$ExpectHomens,log(df$RendaMensal))

ggplot(df)+
  aes(x=ExpectHomens,y=log(RendaMensal),color=Continente)+
  geom_smooth(method = lm, se = FALSE)+
  scale_x_continuous("Expectativa de vida masculina")+
  scale_y_continuous("Logaritmo da renda mensal")+
  geom_point()+
  theme_minimal()

a <- ggplot(df)+
  aes(x=Continente,y=log(RendaMensal),fill=Continente)+
  geom_boxplot()+
  scale_x_discrete("",breaks = NULL) +
  scale_y_continuous("Logaritmo da renda mensal")+
  theme_bw()+
  theme(legend.position="none")

b <- ggplot(df)+
  aes(x=Continente,y=ExpectHomens,fill=Continente)+
  geom_boxplot()+
  scale_x_discrete("",breaks = NULL) +
  scale_y_continuous("Expectativa de vida masculina")+
  theme_bw()

a | b

#modelos
fit1 <- lm(log(RendaMensal)~1, data = df)
fit2 <- lm(log(RendaMensal)~ExpectHomens, data = df)
fit3 <- lm(log(RendaMensal)~ExpectHomens+Continente, data = df)
fit4 <- lm(log(RendaMensal)~ExpectHomens*Continente, data = df)

exp(fit1$coefficients)
summary(fit1)
summary(fit2)
summary(fit3)

anova(fit3 ,fit2)
anova(fit)
summary(fit4)

anova(fit4,fit1)
anova(fit4,fit2)
anova(fit4,fit3)

model <- fit4

#Analise dos resíduos

shapiro.test(model$residuals)
gqtest(model)
dwtest(model)

a <- ggplot(model)+
  geom_point(aes(x=.fitted,y=studres(model),color=df$Continente))+
  scale_color_manual(values=c("#ff6961","#77dd77","#cbcd69","#84b6f4","#cd9cb2"))+
  geom_hline(yintercept = c(2,-2), colour = "black",linetype = "dashed")+
  geom_hline(yintercept = c(0), colour = "red",linetype = "dashed")+
  scale_color_discrete("Continente")+
  labs(x="Valores preditos \n B",y="Resíduo Studentizado")+
  theme_bw()


b <- ggplot(model, aes(sample=studres(model)))+
  geom_qq_line(color="black")+
  #geom_qq_band(fill="lightblue")+
  geom_qq(aes(color=df$Continente))+
  scale_color_manual("Continente",breaks=NULL,values=c("#ff6961","#77dd77","#cbcd69","#84b6f4","#cd9cb2"))+
  labs(x="Quantis teóricos \n A", y="Resíduo Studentizado")+
  theme_bw()


b | a




m_infl <- influence.measures(model)
metricas <- as.data.frame(m_infl$infmat)
names(metricas)

#leverage
g <- ols_prep_rstudlev_data(model)
d <- g$levrstud
d$txt <- ifelse(d$color == "normal", NA, d$obs)
f <- d[d$color == "outlier", c("obs", "leverage", "rstudent")]
colnames(f) <- c("observation", "leverage", "stud_resid")
a <- ggplot(d, aes(leverage, rstudent, label = txt)) +
  geom_point(aes(colour = fct_color)) +
  scale_color_manual(labels = c("normal","ponto de alavanca","outlier",
                                "outlier e ponto de alavanca"),
                     values = c("black","blue", "red", "green")) +
  xlim(g$minx, g$maxx) +
  ylim(g$miny, g$maxy) +
  labs(colour = "Observação", x = "Leverage \n B", y = "Resíduo Studentizado") +
  geom_hline(yintercept = c(2,-2), colour = "black",linetype = "dashed") +
  geom_vline(xintercept = g$lev_thrsh, colour = "black",linetype = "dashed") +
  geom_text(vjust = -1, size = 3, colour = "black") +
  theme_bw()



#dist^ancia de Cook
k <- ols_prep_cdplot_data(model)
d <- ols_prep_outlier_obs(k)
f <- ols_prep_cdplot_outliers(k)
b <- ggplot(d, aes(x = obs, y = cd, label = txt)) +
  geom_bar(width = 0.2, stat = "identity",
           aes(fill = fct_color)) +
  scale_fill_manual(values = c("black", "black"),breaks=NULL) +
  ylim(0, k$maxx) +
  labs(x = "Observação \n A", y = "Distância de Cook") +
  geom_hline(yintercept = 0, colour = "white") +
  geom_hline(yintercept = k$ts, colour = "red",linetype = "dashed") +
  geom_text(hjust = -0.2, nudge_x = 0.05, size = 3, na.rm = TRUE, color="red") +
  theme_bw()
  
b | a


ols_dfbetas <- function (model, seqs,n_row=2,n_col=2, print_plot = TRUE) 
{  obs <- NULL
txt <- NULL
dfb <- dfbetas(model)
n <- nrow(dfb)
#np <- ncol(dfb)
threshold <- 2/sqrt(n)
myplots <- list()
outliers <- list()
#colnames(dfb) <- c("Intercepto",
#                   "Corrente de ar refrigerado",
#                   "Temperatura de resfriamento",
#                   "Concentração de ácido")
for (i in seqs) {
  dbetas <- dfb[, i]
  df_data <- data.frame(obs = seq_len(n), dbetas = dbetas)
  d <- ols_prep_dfbeta_data(df_data, threshold)
  f <- ols_prep_dfbeta_outliers(d)
  p <- ggplot(d, aes(x = obs, y = dbetas, 
                     label = txt, ymin = 0, ymax = dbetas)) + 
    geom_linerange(colour = "black") + 
    geom_hline(yintercept = c(0, threshold, -threshold), 
               colour = "red") +
    labs(x="",y="") + 
    ggtitle(paste(colnames(dfb)[i])) + 
    theme_bw()+
    geom_text(hjust = -0.2, nudge_x = 0.15, size = 3, 
              colour = "black", na.rm = TRUE) 
  myplots[[i]] <- p
  outliers[[i]] <- f
}
if (print_plot) {
  marrangeGrob(myplots, nrow = n_row, ncol = n_col, top = quote(paste("")), left="DFBETAS",bottom="Observações")
}
}

ols_dfbetas(model,c(1,2,3,4))
ols_dfbetas(model,c(5,6,7,8))
ols_dfbetas(model,c(9,10),1,2)






#DFFITs 
dbetas <- NULL
obs <- NULL
txt <- NULL
dffitsm <- unlist(dffits(model))
k <- length(coef(model))
n <- nrow(df)
dffits_t <- sqrt(k/n) * 2
title <- names(model.frame(model))[1]
dfits_data <- data.frame(obs = seq_len(n), dbetas = dffitsm)
d <- ols_prep_dfbeta_data(dfits_data, dffits_t)
f <- ols_prep_dfbeta_outliers(d)
a <- ggplot(d, aes(x = obs, y = dbetas, label = txt, ymin = 0, 
                   ymax = dffitsm)) + 
  geom_linerange(colour = "black") + 
  geom_hline(yintercept = c(0, dffits_t, -dffits_t), colour = "red") + 
  labs(x = "Observação", y = "DFFITS") +
  theme_bw()+
  geom_text(hjust = -0.2, nudge_x = 0.15, size = 4, colour = "black", 
            na.rm = TRUE) + 
  annotate("text", x = Inf, y = Inf, hjust = 1.5, 
           vjust = 1.5, colour = "darkred", 
           label = paste("|Limite| =", round(dffits_t, 2)))

#COVRATIOs
cr = data.frame(covratio(model))
cr['obs'] = seq(1,nrow(df))
limite = (3*length(model$coefficients) ) / (nrow(df)-length(model$coefficients))

b <- ggplot(data=cr,aes(x = obs, y = (1-covratio.model.), label = obs))+
  geom_line() +
  geom_hline(yintercept = c(limite, -1*limite),colour = "red")+
  geom_text(aes(label=ifelse(abs(1-covratio.model.)>limite,as.character(obs),'')),hjust=-0.2,vjust=0) + 
  labs(x="Observação",y="COVRATIO")+
  theme_bw() +
  annotate("text", x = Inf, y = Inf, hjust = 1.5, 
           vjust = 1.5, colour = "darkred", label = paste("|Limite| =", round(limite, 3)))

a | b
