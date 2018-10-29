## converti o .csv para .rds. Fica mais rapido a importa??o e mais leve (compress = TRUE)

escolas <- readRDS("ESCOLAS.rds")
indicadoresEducacionais <- readRDS("INDICADORES EDUCACIONAIS.rds")


escolasPublicas <- subset(escolas, TP_DEPENDENCIA != 4) # Retirei as escolas privadas

{
## Saber a taxa de alunos que bebem agua filtrada
taxaAguaFiltradaEscolasPublicas <- escolasPublicas$IN_AGUA_FILTRADA
table(taxaAguaFiltradaEscolasPublicas)

## Saber abastecimentos de agua inexistentes
escolasSemAgua <- escolasPublicas$IN_AGUA_INEXISTENTE
table(escolasSemAgua)

## Saber abastecimentos de energia inexistentes
escolasSemEnergia <- escolasPublicas$IN_ENERGIA_INEXISTENTE
table(escolasSemEnergia)

## Saber serviço de esgoto inexistente
escolasSemEsgoto <- escolasPublicas$IN_ESGOTO_INEXISTENTE
table(escolasSemEsgoto)

## Saber a taxa de acesso a internet das escolas publicas
escolasSemInternet <- escolasPublicas$IN_INTERNET
table(escolasSemInternet)
}

## Criando um data frame com as variaveis que irei utilizar
## sobre a infraestrutura das escolas públicas brasileiras
estruturasDasEscolas <- cbind.data.frame(escolasPublicas$CO_UF
                                        ,escolasPublicas$IN_SALA_PROFESSOR
                                        ,escolasPublicas$IN_LABORATORIO_INFORMATICA
                                        ,escolasPublicas$IN_LABORATORIO_CIENCIAS
                                        ,escolasPublicas$IN_QUADRA_ESPORTES
                                        ,escolasPublicas$IN_BIBLIOTECA_SALA_LEITURA)

## Algumas linhas tiveram NA's, as eliminarei
estruturasDasEscolas <- na.omit(estruturasDasEscolas)

## Trocando o Código da UF por sua sigla
{
  estruturasDasEscolas[estruturasDasEscolas$`escolasPublicas$CO_UF` == 11,1] <- "RO"
  estruturasDasEscolas[estruturasDasEscolas$`escolasPublicas$CO_UF` == 12,1] <- "AC"
  estruturasDasEscolas[estruturasDasEscolas$`escolasPublicas$CO_UF` == 13,1] <- "AM"
  estruturasDasEscolas[estruturasDasEscolas$`escolasPublicas$CO_UF` == 14,1] <- "RR"
  estruturasDasEscolas[estruturasDasEscolas$`escolasPublicas$CO_UF` == 15,1] <- "PA"
  estruturasDasEscolas[estruturasDasEscolas$`escolasPublicas$CO_UF` == 16,1] <- "AP"
  estruturasDasEscolas[estruturasDasEscolas$`escolasPublicas$CO_UF` == 17,1] <- "TO"
  
  estruturasDasEscolas[estruturasDasEscolas$`escolasPublicas$CO_UF` == 21,1] <- "MA"
  estruturasDasEscolas[estruturasDasEscolas$`escolasPublicas$CO_UF` == 22,1] <- "PI"
  estruturasDasEscolas[estruturasDasEscolas$`escolasPublicas$CO_UF` == 23,1] <- "CE"
  estruturasDasEscolas[estruturasDasEscolas$`escolasPublicas$CO_UF` == 24,1] <- "RN"
  estruturasDasEscolas[estruturasDasEscolas$`escolasPublicas$CO_UF` == 25,1] <- "PB"
  estruturasDasEscolas[estruturasDasEscolas$`escolasPublicas$CO_UF` == 26,1] <- "PE"
  estruturasDasEscolas[estruturasDasEscolas$`escolasPublicas$CO_UF` == 27,1] <- "AL"
  estruturasDasEscolas[estruturasDasEscolas$`escolasPublicas$CO_UF` == 28,1] <- "SE"
  estruturasDasEscolas[estruturasDasEscolas$`escolasPublicas$CO_UF` == 29,1] <- "BA"
  
  estruturasDasEscolas[estruturasDasEscolas$`escolasPublicas$CO_UF` == 31,1] <- "MG"
  estruturasDasEscolas[estruturasDasEscolas$`escolasPublicas$CO_UF` == 32,1] <- "ES"
  estruturasDasEscolas[estruturasDasEscolas$`escolasPublicas$CO_UF` == 33,1] <- "RJ"
  estruturasDasEscolas[estruturasDasEscolas$`escolasPublicas$CO_UF` == 35,1] <- "SP"
  
  estruturasDasEscolas[estruturasDasEscolas$`escolasPublicas$CO_UF` == 41,1] <- "PR"
  estruturasDasEscolas[estruturasDasEscolas$`escolasPublicas$CO_UF` == 42,1] <- "SC"
  estruturasDasEscolas[estruturasDasEscolas$`escolasPublicas$CO_UF` == 43,1] <- "RS"
  
  estruturasDasEscolas[estruturasDasEscolas$`escolasPublicas$CO_UF` == 51,1] <- "MT"
  estruturasDasEscolas[estruturasDasEscolas$`escolasPublicas$CO_UF` == 52,1] <- "GO"
  estruturasDasEscolas[estruturasDasEscolas$`escolasPublicas$CO_UF` == 53,1] <- "DF"
  estruturasDasEscolas[estruturasDasEscolas$`escolasPublicas$CO_UF` == 50,1] <- "MS"
}


## Atribui as variaveis presentes a um valor que somado "pontua" a presença dos itens esse
## valor foi baseado na atribuição do trabalho da Adailda Gomes - "Desempenho e infraestru-
## tura: mapeamento das escolas públicas da região metropolitana do Rio de Janeiro"
estruturasDasEscolas[estruturasDasEscolas$`escolasPublicas$IN_SALA_PROFESSOR` == 1,2] <- 0.598
estruturasDasEscolas[estruturasDasEscolas$`escolasPublicas$IN_LABORATORIO_INFORMATICA` == 1,3] <- 0.703
estruturasDasEscolas[estruturasDasEscolas$`escolasPublicas$IN_LABORATORIO_CIENCIAS` == 1,4] <- 0.510
estruturasDasEscolas[estruturasDasEscolas$`escolasPublicas$IN_QUADRA_ESPORTES` == 1,5] <- 0.651
estruturasDasEscolas[estruturasDasEscolas$`escolasPublicas$IN_BIBLIOTECA_SALA_LEITURA` == 1,6] <- 0.606

pontosDosEstados <- cbind.data.frame(estruturasDasEscolas[1],(estruturasDasEscolas[2]+
                                                              estruturasDasEscolas[3]+
                                                              estruturasDasEscolas[4]+
                                                              estruturasDasEscolas[5]+
                                                              estruturasDasEscolas[6]))
names(pontosDosEstados)[1:2] <- c("Estado", "Pontuação")

mediaPontuacaoEscolas <- aggregate(Pontuação ~ Estado, data = pontosDosEstados, FUN = mean)















