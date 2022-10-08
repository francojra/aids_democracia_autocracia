
# HIV/AIDS em países capitalistas e comunistas ---------------------------------------------------------------------------------------------
# Atoria do script: Jeanne Franco ----------------------------------------------------------------------------------------------------------
# Data: 07/10/22 ---------------------------------------------------------------------------------------------------------------------------
# Referência: https://ourworldindata.org/hiv-aids ------------------------------------------------------------------------------------------

# Sobre os dados ---------------------------------------------------------------------------------------------------------------------------

### Uma infecção com HIV (human immunodeficiency virus) pode levar a AIDS (acquired immunodeficiency syndrome)
### AIDS resulta em um gradual e persistente declínio e falha do sistema imunológico,
### resultando em um aumentado risco de infecções e cancers que ameaçam a vida.

### Na maioria dos casos, HIV é uma infecção transmitida sexualmente. Entretanto, o HIV
### pode ser transmitido da mãe para o filho através da gravidez ou parto, ou através
### da amamentação. Transmissão não sexual também pode ocorrer através do compartilhamento
### dos equipamento de injeções como agulhas contaminadas.

# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(cols4all)
library(hrbrthemes)

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

aids <- read.csv("share-deaths-aids.csv")
view(aids)
names(aids)

# Manipular dados --------------------------------------------------------------------------------------------------------------------------

aids <- aids %>%
  select(-Code) %>%
  rename(morte_hiv = Deaths...HIV.AIDS...Sex..Both...Age..All.Ages..Percent.) %>%
  view()

aids1 <- aids %>%
  filter(Entity %in% c("United States", "Germany", "Japan",
                       "China", "Cuba", "North Korea")) %>%
  group_by(Entity) %>%
  summarise(media = mean(morte_hiv),
            sd = sd(morte_hiv), n = n(),
            se = sd/sqrt(n)) %>%
  view()

aids2 <- aids %>%
  filter(Entity %in% c("United States", "Germany", "Japan",
                       "China", "Cuba", "North Korea")) %>%
  view()

# Gráficos ---------------------------------------------------------------------------------------------------------------------------------

c4a("safe", 6)

ggplot(aids1, aes(x = fct_reorder(Entity, media), 
                  y = media, fill = Entity)) +
  geom_col() +
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                size = 0.8, width = 0.2) +
  scale_fill_manual(values = c("#88CCEE", "#CC6677",
                               "#DDCC77", "#117733",
                               "#332288", "#AA4499")) +
  scale_y_continuous(expand = expansion(mult = c(0,0))) +
  scale_x_discrete(labels = c("Japão", "Alemanha", "Coreia do Norte",
                              "China", "Cuba", "Estados Unidos")) +
  labs(x = "Países", y = "Porcentagem mortes por HIV") +
  theme_ipsum(axis_title_size = 16,
              axis_text_size = 14) +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"))

ggplot(aids2, aes(x = Year, y = morte_hiv, 
                  group = Entity,
                  color = Entity)) +
  geom_point(shape = 15, size = 2.5) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("#88CCEE", "#CC6677",
                               "#DDCC77", "#117733",
                               "#332288", "#AA4499"),
                     labels = c("China", "Cuba", "Alemanha",
                                "Japão", "Coreia do Norte", "Estados Unidos")) +
  labs(x = "Tempo (anos)", y = "Porcentagem mortes por HIV",
       color = "Países") +
  theme_ipsum(axis_title_size = 16,
              axis_text_size = 14) +
  theme(axis.text = element_text(color = "black"))

