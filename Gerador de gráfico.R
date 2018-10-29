#instala o pacote (Não tem no CRAN DO Rstudio)
devtools::install_github('abjur/abjData')
library(dplyr)
library(abjData)
library(ggplot2)

dados <- data.frame(mediaPontuacaoEscolas)
names(dados)[1:2] <- c("id", "variavel")

constroi_mapa_tematico <- function(dataset){
  dataset %>% 
    inner_join(abjData::br_uf_map) %>% {
      ggplot(.) +
        geom_map(aes(x = long, y = lat,
                     map_id = id, fill = variavel),
                 color = 'gray30', map = ., data = .) + 
        theme_void() +
        coord_equal()
    }
}

constroi_mapa_tematico(dados) +
  ggtitle(" Pontuação da Infraestrutura das Escolas Públicas em 2017 no Brasil") +
  scale_fill_continuous(name = "Pontuação", low = 'white', high = 'red',
                        na.value = 'white')




