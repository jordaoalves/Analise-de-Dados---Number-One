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
  ggtitle("Média de Alunos por Turma x Pontuação da \n Infraestrutura  das Escolas Públicas \n (Nível Fundamental)  em 2017 no Brasil") +
              geom_point()+geom_smooth(method="lm")+
theme(plot.title = element_text(hjust = 0.5))

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
    ggtitle("Taxa de Aprovação dos Alunos x Pontuação da \n Infraestrutura  das Escolas Públicas (Nível \n Fundamental)  em 2017 no Brasil") +
    geom_point()+geom_smooth(method="lm")+
  theme(plot.title = element_text(hjust = 0.5))
  
  arvoreTxAprovFund <- rpart(Pontuação ~ TX_APRV_FUND ,  data = tabelaDados)
  prp(arvoreTxAprovFund, extra = 1)
}
















