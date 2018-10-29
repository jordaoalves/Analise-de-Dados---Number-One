library(ggplot2)
library(rpart)
library(rpart.plot)
# Unindo os data frames
tabelaDados <- merge(mediaPontuacaoEscolas, indicadoresEducacionais, by="Estado")

#write.csv(tabelaDados ,file = "tabelaDados.csv")


# Modelagem media dos alunos do ensino fundamental por turma x pontuação
{
modeloRegressaoMediaAlunosFund <- lm(tabelaDados$MED_TURM_FUND ~ tabelaDados$Pontuação, 
                              data= tabelaDados)
summary(modeloRegressaoMediaAlunosFund)
anova(modeloRegressaoMediaAlunosFund)


ggplot(data=tabelaDados,aes(y=MED_TURM_FUND,x=Pontuação))+
  ggtitle("Média de alunos por turma x Pont. das escolas") +
              geom_point()+geom_smooth(method="lm")

arvoreMediaAlunosFund <- rpart(Pontuação ~ MED_TURM_FUND ,  data = tabelaDados)
prp(arvoreMediaAlunosFund, extra = 1)
}

# Modelagem taxa de aprovação dos alunos do ensino fundamental x pontuação
{
  modeloRegressaoTxAprovFund <- lm(tabelaDados$TX_APRV_FUND ~ tabelaDados$Pontuação, 
                                       data= tabelaDados)
  summary(modeloRegressaoTxAprovFund)
  anova(modeloRegressaoTxAprovFund)
  
  
  ggplot(data=tabelaDados,aes(y=TX_APRV_FUND,x=Pontuação))+
    ggtitle("Taxa de Aprovação x Pontuação das Escolas") +
    geom_point()+geom_smooth(method="lm")
  
  arvoreTxAprovFund <- rpart(Pontuação ~ TX_APRV_FUND ,  data = tabelaDados)
  prp(arvoreTxAprovFund, extra = 1)
}
















