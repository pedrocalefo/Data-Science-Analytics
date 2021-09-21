###########################################
#
#Nome: Pedro Calefo Richena
#Data:20/09/2021
#Análise:Regressão Logística Binária
#Dados:Falhas do Coração
#
###########################################

#----- Carregando e limpando dados -------#

pacotes <- c('plotly','readr','jtools','caret','pROC')

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}
heart_failure <- read_csv("heart_failure.csv")
View(heart_failure)


summary(heart_failure)

heart_failure$time <- NULL
heart_failure$anaemia <- as.factor(heart_failure$anaemia)
heart_failure$diabetes<- as.factor(heart_failure$diabetes)
heart_failure$high_blood_pressure <- as.factor(heart_failure$high_blood_pressure)
heart_failure$sex <- as.factor(heart_failure$sex)
heart_failure$smoking <- as.factor(heart_failure$smoking)
heart_failure$DEATH_EVENT <- as.factor(heart_failure$DEATH_EVENT)

summary(heart_failure)

#----- Criando modelo e limpando váriaveis não relevantes -------#

modelo_heart_failure <- glm(formula = DEATH_EVENT ~ ., 
                           data = heart_failure, 
                           family = "binomial")

step_heart <- step(modelo_heart_failure, k = 3.841459)

summary(step_heart)

logLik(step_heart)

export_summs(step_heart, scale = F, digits = 6)

predict(object = step_heart, 
        data.frame(age=50,
                   ejection_fraction=80,
                   serum_creatinine=2.5),
        type = "response")


#----- Criando matriz de confusão -------#

confusionMatrix(table(predict(step_heart, type = "response") >= 0.40,
                      heart_failure$DEATH_EVENT == 1)[2:1, 2:1])


#----- Avaliando eficiência do modelo (ROC) -------#

ROC <- roc(response = heart_failure$DEATH_EVENT, 
           predictor = step_heart$fitted.values)

ggplotly(
  ggroc(ROC, color = "#440154FF", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1),
                 color="grey40",
                 size = 0.2) +
    labs(x = "Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:",
                       round(ROC$auc, 3),
                       "|",
                       "Coeficiente de Gini",
                       round((ROC$auc[1] - 0.5) / 0.5, 3))) +
    theme_bw()
)
