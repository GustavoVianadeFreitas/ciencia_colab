require(tidyr)
require(dplyr)

data.frame(
  participante = seq(1, 10, 1),
  fichas = c(read.csv("~/github/cursodados/data/iris_mod.csv", header = T) %>% 
               distinct(amostra) %>% 
               pull()%>% 
                 sample()),
  n = 'amostras'
  ) %>% 
  pivot_wider(
    names_from = "n", 
    values_from = "fichas"
  ) %>% 
  knitr::kable()
?pull
