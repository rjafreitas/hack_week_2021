# Carregando pacotes

library(tidyverse)
library(patchwork)

# Abrindo a base

hw21 <- readr::read_rds("data/piloto_hack.rds")

# view(hw21)

# 1. Formação em dados abertos. -------------------------------------------

hw21 %>%
  ggplot(aes(x = formacao_dados, fill = formacao_dados)) +
  geom_bar() +
  theme_minimal() +
  labs(x = "",
       y = "Contagem") +
  theme(legend.position = "none") +
  scale_fill_hue(c = 40) +
  stat_count(
    geom = "text",
    aes(label = stat(count)),
    position = position_stack(vjust = 0.5),
    colour = "black"
  )


# 2. Utilizo dados produzidos por terceiros. ----------------------------

d_terceiros <- hw21 %>%
  ggplot(aes(x = dados_terceiros, fill = dados_terceiros)) +
  geom_bar(width = 0.6) +
  theme_minimal() +
  stat_count(
    geom = "text",
    aes(label = stat(count)),
    position = position_stack(vjust = 0.5),
    colour = "#e9ebf0"
  ) +
  labs(title = "Dados de terceiros",
       x = "",
       y = "Contagem") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    text = element_text(size = 16),
    axis.title.y = element_text(size = 12)
  ) +
  scale_fill_manual(
    "Legenda",
    values = c(
      "Nunca" = "#9eb3c2",
      "Pouco frequente" = "#1c7293",
      "Com alguma frequência" = "#065a82",
      "Muito frequente" = "#1b3b6f",
      "Sempre" = "#21295c"
    )
  ) +
  guides(fill = guide_legend())


# 2.1 Meu grupo de pesquisa disponibiliza os dados. ------------------------

d_grupo <- hw21 %>%
  ggplot(aes(x = grupo_disp_dados, fill = grupo_disp_dados)) +
  geom_bar(width = 0.6) +
  theme_minimal() +
  stat_count(
    geom = "text",
    aes(label = stat(count)),
    position = position_stack(vjust = 0.5),
    colour = "#e9ebf0"
  ) +
  labs(title = "Disponibilização pelo grupo",
       x = "",
       y = "") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none"
  ) +
  scale_fill_manual(
    values = c(
      "Nunca" = "#9eb3c2",
      "Pouco frequente" = "#1c7293",
      "Com alguma frequência" = "#065a82",
      "Muito frequente" = "#1b3b6f",
      "Sempre" = "#21295c"
    )
  )

# 2.2 Trabalho com dados públicos. -----------------------------------------

d_publicos <- hw21 %>%
  ggplot(aes(x = dados_publicos, fill = dados_publicos)) +
  geom_bar(width = 0.6) +
  theme_minimal() +
  stat_count(
    geom = "text",
    aes(label = stat(count)),
    position = position_stack(vjust = 0.5),
    colour = "#e9ebf0"
  ) +
  labs(title = "Dados públicos",
    x = "",
       y = "") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none"
  ) +
  scale_fill_manual(
    values = c(
      "Nunca" = "#9eb3c2",
      "Pouco frequente" = "#1c7293",
      "Com alguma frequência" = "#065a82",
      "Muito frequente" = "#1b3b6f",
      "Sempre" = "#21295c"
    )
  )

# 2.3 Disponibilizo meu banco de dados em alguma plataforma. ---------------

d_banco <- hw21 %>%
  ggplot(aes(x = disponibilizo_banco, fill = disponibilizo_banco)) +
  geom_bar(width = 0.6) +
  theme_minimal() +
  stat_count(
    geom = "text",
    aes(label = stat(count)),
    position = position_stack(vjust = 0.5),
    colour = "#e9ebf0"
  ) +
  labs(title = "Disponibilização em plataformas",
    x = "",
       y = "") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none"
  ) +
  scale_fill_manual(
    values = c(
      "Nunca" = "#9eb3c2",
      "Pouco frequente" = "#1c7293",
      "Com alguma frequência" = "#065a82",
      "Muito frequente" = "#1b3b6f",
      "Sempre" = "#21295c"
    )
  )

# 2.4 Na minha área de pesquisa é comum disponibilizar dados. --------------

d_area <- hw21 %>%
  ggplot(aes(x = disponibilizacao_area, fill = disponibilizacao_area)) +
  geom_bar(width = 0.6) +
  theme_minimal() +
  stat_count(
    geom = "text",
    aes(label = stat(count)),
    position = position_stack(vjust = 0.5),
    colour = "#e9ebf0"
  ) +
  labs(title = "Comum a disponibilização na área",
    x = "",
       y = "") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none"
  ) +
  scale_fill_manual(
    values = c(
      "Nunca" = "#9eb3c2",
      "Pouco frequente" = "#1c7293",
      "Com alguma frequência" = "#065a82",
      "Muito frequente" = "#1b3b6f",
      "Sempre" = "#21295c"
    )
  )


# Juntando gráficos dos itens 2 -------------------------------------------

d_terceiros / (d_area + d_banco + d_grupo + d_publicos)

# Salvando:

# ggsave(
#   "graficos/uso_dados.jpeg",
#   device = "jpeg",
#   dpi = 600,
#   width = 9,
#   height = 7
# )

# 3. Incentivo sobre a disponibilização dos dados
# 3.1 Orientador/Supervisor -----------------------------------------------

inc_orient <- hw21 %>%
  ggplot(aes(x = incentivo_orientador, fill = incentivo_orientador)) +
  geom_bar(width = 0.6) +
  theme_minimal() +
  stat_count(
    geom = "text",
    aes(label = stat(count)),
    position = position_stack(vjust = 0.5),
    colour = "#e9ebf0"
  ) +
  labs(title = "Orientador/Supervisor",
       x = "",
       y = "Contagem") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    text = element_text(size = 16),
    axis.title.y = element_text(size = 12)
  ) +
  scale_fill_manual(
    "Legenda",
    values = c(
      "Sim, de forma obrigatória" = "#9eb3c2",
      "Sim, sem obrigatoriedade" = "#1c7293",
      "Não" = "#065a82",
      "Sou desincentivado" = "#1b3b6f",
      "Não sei" = "#21295c"
    )
  ) +
  guides(fill = guide_legend())

# 3.2 Programa de pós graduação/instituição -------------------------------

inc_prog <- hw21 %>%
  ggplot(aes(x = incentivo_ppg, fill = incentivo_ppg)) +
  geom_bar(width = 0.6) +
  theme_minimal() +
  stat_count(
    geom = "text",
    aes(label = stat(count)),
    position = position_stack(vjust = 0.5),
    colour = "#e9ebf0"
  ) +
  labs(title = "PPG/instituição",
       x = "",
       y = "") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    text = element_text(size = 16),
    axis.title.y = element_text(size = 12),
    legend.position = "none"
  ) +
  scale_fill_manual(
    values = c(
      "Sim, de forma obrigatória" = "#9eb3c2",
      "Sim, sem obrigatoriedade" = "#1c7293",
      "Não" = "#065a82",
      "Sou desincentivado" = "#1b3b6f",
      "Não sei" = "#21295c"
    )
  )

# 3.3 Fonte de fomento (ex: CAPES) ----------------------------------------

inc_fom <- hw21 %>%
  ggplot(aes(x = incentivo_fomento, fill = incentivo_fomento)) +
  geom_bar(width = 0.6) +
  theme_minimal() +
  stat_count(
    geom = "text",
    aes(label = stat(count)),
    position = position_stack(vjust = 0.5),
    colour = "#e9ebf0"
  ) +
  labs(title = "Fonte de fomento",
       x = "",
       y = "") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    text = element_text(size = 16),
    axis.title.y = element_text(size = 12),
    legend.position = "none"
  ) +
  scale_fill_manual(
    values = c(
      "Sim, de forma obrigatória" = "#9eb3c2",
      "Sim, sem obrigatoriedade" = "#1c7293",
      "Não" = "#065a82",
      "Sou desincentivado" = "#1b3b6f",
      "Não sei" = "#21295c"
    )
  )

# Juntando os itens 3
inc_orient | inc_fom | inc_prog

# Salvando:

# ggsave(
#   "graficos/incentivo.jpeg",
#   device = "jpeg",
#   dpi = 600,
#   width = 12,
#   height = 3
# )

# exemplo de gráfico

# 4. Recursos ou conhecimentos na área de dados

# 4.1 Zenodo --------------------------------------------------------------

zenodo <- hw21 %>%
  ggplot(aes(x = recursos_zenodo, fill = recursos_zenodo)) +
  geom_bar(width = 0.6) +
  theme_minimal() +
  stat_count(
    geom = "text",
    aes(label = stat(count)),
    position = position_stack(vjust = 0.5),
    colour = "#e9ebf0"
  ) +
  labs(title = "Zenodo",
       x = "",
       y = "") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    text = element_text(size = 16),
    axis.title.y = element_text(size = 12),
    legend.position = "none"
  ) +
  scale_fill_manual(
    values = c(
      "Nunca ouvi falar" = "#9eb3c2",
      "O nome não é estranho" = "#1c7293",
      "Conheço, mas nunca usei/apliquei" = "#065a82",
      "Já usei/apliquei, mas pouco" = "#1b3b6f",
      "Uso/aplico no meu dia-a-dia" = "#21295c"
    )
  )

# 4.2 Dryad ---------------------------------------------------------------

dryad <- hw21 %>%
  ggplot(aes(x = recursos_dryad, fill = recursos_dryad)) +
  geom_bar(width = 0.6) +
  theme_minimal() +
  stat_count(
    geom = "text",
    aes(label = stat(count)),
    position = position_stack(vjust = 0.5),
    colour = "#e9ebf0"
  ) +
  labs(title = "Dryad",
       x = "",
       y = "") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    text = element_text(size = 16),
    axis.title.y = element_text(size = 12),
  ) +
  scale_fill_manual("Legenda",
    values = c(
      "Nunca ouvi falar" = "#9eb3c2",
      "O nome não é estranho" = "#1c7293",
      "Conheço, mas nunca usei/apliquei" = "#065a82",
      "Já usei/apliquei, mas pouco" = "#1b3b6f",
      "Uso/aplico no meu dia-a-dia" = "#21295c"
    )
  )

# 4.3 Figshare ------------------------------------------------------------

figshare <- hw21 %>%
  ggplot(aes(x = recursos_figshare, fill = recursos_figshare)) +
  geom_bar(width = 0.6) +
  theme_minimal() +
  stat_count(
    geom = "text",
    aes(label = stat(count)),
    position = position_stack(vjust = 0.5),
    colour = "#e9ebf0"
  ) +
  labs(title = "Figshare",
       x = "",
       y = "") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    text = element_text(size = 16),
    axis.title.y = element_text(size = 12),
    legend.position = "none"
  ) +
  scale_fill_manual(
    values = c(
      "Nunca ouvi falar" = "#9eb3c2",
      "O nome não é estranho" = "#1c7293",
      "Conheço, mas nunca usei/apliquei" = "#065a82",
      "Já usei/apliquei, mas pouco" = "#1b3b6f",
      "Uso/aplico no meu dia-a-dia" = "#21295c"
    )
  )

# 4.4 Dataverse -----------------------------------------------------------

dataverse <- hw21 %>%
  ggplot(aes(x = recursos_dataverse, fill = recursos_dataverse)) +
  geom_bar(width = 0.6) +
  theme_minimal() +
  stat_count(
    geom = "text",
    aes(label = stat(count)),
    position = position_stack(vjust = 0.5),
    colour = "#e9ebf0"
  ) +
  labs(title = "Dataverse",
       x = "",
       y = "") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    text = element_text(size = 16),
    axis.title.y = element_text(size = 12),
    legend.position = "none"
  ) +
  scale_fill_manual(
    values = c(
      "Nunca ouvi falar" = "#9eb3c2",
      "O nome não é estranho" = "#1c7293",
      "Conheço, mas nunca usei/apliquei" = "#065a82",
      "Já usei/apliquei, mas pouco" = "#1b3b6f",
      "Uso/aplico no meu dia-a-dia" = "#21295c"
    )
  )

# 4.5 Principios FAIR -----------------------------------------------------

fair <- hw21 %>%
  ggplot(aes(x = recursos_fair, fill = recursos_fair)) +
  geom_bar(width = 0.6) +
  theme_minimal() +
  stat_count(
    geom = "text",
    aes(label = stat(count)),
    position = position_stack(vjust = 0.5),
    colour = "#e9ebf0"
  ) +
  labs(title = "Princípios FAIR",
       x = "",
       y = "") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    text = element_text(size = 16),
    axis.title.y = element_text(size = 12),
    
  ) +
  scale_fill_manual("Legenda",
    values = c(
      "Nunca ouvi falar" = "#9eb3c2",
      "O nome não é estranho" = "#1c7293",
      "Conheço, mas nunca usei/apliquei" = "#065a82",
      "Já usei/apliquei, mas pouco" = "#1b3b6f",
      "Uso/aplico no meu dia-a-dia" = "#21295c"
    )
  )

# 4.6 Licença -------------------------------------------------------------

licenca <- hw21 %>%
  ggplot(aes(x = recursos_licenca, fill = recursos_licenca)) +
  geom_bar(width = 0.6) +
  theme_minimal() +
  stat_count(
    geom = "text",
    aes(label = stat(count)),
    position = position_stack(vjust = 0.5),
    colour = "#e9ebf0"
  ) +
  labs(title = "Licença",
       x = "",
       y = "") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    text = element_text(size = 16),
    axis.title.y = element_text(size = 12),
    legend.position = "none"
  ) +
  scale_fill_manual(
    values = c(
      "Nunca ouvi falar" = "#9eb3c2",
      "O nome não é estranho" = "#1c7293",
      "Conheço, mas nunca usei/apliquei" = "#065a82",
      "Já usei/apliquei, mas pouco" = "#1b3b6f",
      "Uso/aplico no meu dia-a-dia" = "#21295c"
    )
  )

# 4.7 CSV -----------------------------------------------------------------

csv <- hw21 %>%
  ggplot(aes(x = recursos_csv, fill = recursos_csv)) +
  geom_bar(width = 0.6) +
  theme_minimal() +
  stat_count(
    geom = "text",
    aes(label = stat(count)),
    position = position_stack(vjust = 0.5),
    colour = "#e9ebf0"
  ) +
  labs(title = ".csv",
       x = "",
       y = "") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    text = element_text(size = 16),
    axis.title.y = element_text(size = 12),
    legend.position = "none"
  ) +
  scale_fill_manual(
    values = c(
      "Nunca ouvi falar" = "#9eb3c2",
      "O nome não é estranho" = "#1c7293",
      "Conheço, mas nunca usei/apliquei" = "#065a82",
      "Já usei/apliquei, mas pouco" = "#1b3b6f",
      "Uso/aplico no meu dia-a-dia" = "#21295c"
    )
  )

# 4.8 Dicionário ----------------------------------------------------------

dicionario <- hw21 %>%
  ggplot(aes(x = recursos_dicionario, fill = recursos_dicionario)) +
  geom_bar(width = 0.6) +
  theme_minimal() +
  stat_count(
    geom = "text",
    aes(label = stat(count)),
    position = position_stack(vjust = 0.5),
    colour = "#e9ebf0"
  ) +
  labs(title = "Dicionário de dados",
       x = "",
       y = "") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    text = element_text(size = 16),
    axis.title.y = element_text(size = 12),
    legend.position = "none"
  ) +
  scale_fill_manual(
    values = c(
      "Nunca ouvi falar" = "#9eb3c2",
      "O nome não é estranho" = "#1c7293",
      "Conheço, mas nunca usei/apliquei" = "#065a82",
      "Já usei/apliquei, mas pouco" = "#1b3b6f",
      "Uso/aplico no meu dia-a-dia" = "#21295c"
    )
  )

# 4.9 Ontologias ----------------------------------------------------------

ontologias <- hw21 %>%
  ggplot(aes(x = recursos_ontologias, fill = recursos_ontologias)) +
  geom_bar(width = 0.6) +
  theme_minimal() +
  stat_count(
    geom = "text",
    aes(label = stat(count)),
    position = position_stack(vjust = 0.5),
    colour = "#e9ebf0"
  ) +
  labs(title = "Ontologias",
       x = "",
       y = "") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(size = 12),
    text = element_text(size = 16),
    legend.position = "none"
  ) +
  scale_fill_manual(
    values = c(
      "Nunca ouvi falar" = "#9eb3c2",
      "O nome não é estranho" = "#1c7293",
      "Conheço, mas nunca usei/apliquei" = "#065a82",
      "Já usei/apliquei, mas pouco" = "#1b3b6f",
      "Uso/aplico no meu dia-a-dia" = "#21295c"
    )
  )

# 4.10 Identificadores persistentes ---------------------------------------

persistentes <- hw21 %>%
  ggplot(aes(x = recursos_identi_persistentes, 
             fill = recursos_identi_persistentes)) +
  geom_bar(width = 0.6) +
  theme_minimal() +
  stat_count(
    geom = "text",
    aes(label = stat(count)),
    position = position_stack(vjust = 0.5),
    colour = "#e9ebf0"
  ) +
  labs(title = "Identificadores persistentes",
       x = "",
       y = "") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    text = element_text(size = 16),
    legend.position = "none"
  ) +
  scale_fill_manual(
    values = c(
      "Nunca ouvi falar" = "#9eb3c2",
      "O nome não é estranho" = "#1c7293",
      "Conheço, mas nunca usei/apliquei" = "#065a82",
      "Já usei/apliquei, mas pouco" = "#1b3b6f",
      "Uso/aplico no meu dia-a-dia" = "#21295c"
    )
  )

# 4.11 Research Data Framework --------------------------------------------

rdf <- hw21 %>%
  ggplot(aes(x = recursos_rdf, fill = recursos_rdf)) +
  geom_bar(width = 0.6) +
  theme_minimal() +
  stat_count(
    geom = "text",
    aes(label = stat(count)),
    position = position_stack(vjust = 0.5),
    colour = "#e9ebf0"
  ) +
  labs(title = "Research Data Framework (RDF)",
       x = "",
       y = "") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    text = element_text(size = 16),
    axis.title.y = element_text(size = 12),
    legend.position = "none"
  ) +
  scale_fill_manual(
    values = c(
      "Nunca ouvi falar" = "#9eb3c2",
      "O nome não é estranho" = "#1c7293",
      "Conheço, mas nunca usei/apliquei" = "#065a82",
      "Já usei/apliquei, mas pouco" = "#1b3b6f",
      "Uso/aplico no meu dia-a-dia" = "#21295c"
    )
  )


# 4.12 Gestão de dados ----------------------------------------------------

gestao <- hw21 %>%
  ggplot(aes(x = recursos_gestao_dados, fill = recursos_gestao_dados)) +
  geom_bar(width = 0.6) +
  theme_minimal() +
  stat_count(
    geom = "text",
    aes(label = stat(count)),
    position = position_stack(vjust = 0.5),
    colour = "#e9ebf0"
  ) +
  labs(title = "Gestão de dados",
       x = "",
       y = "") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    text = element_text(size = 16),
    axis.title.y = element_text(size = 12),
    legend.position = "none"
  ) +
  scale_fill_manual(
    values = c(
      "Nunca ouvi falar" = "#9eb3c2",
      "O nome não é estranho" = "#1c7293",
      "Conheço, mas nunca usei/apliquei" = "#065a82",
      "Já usei/apliquei, mas pouco" = "#1b3b6f",
      "Uso/aplico no meu dia-a-dia" = "#21295c"
    )
  )


# Juntando conceitos ------------------------------------------------------

gestao + dicionario + fair + licenca + persistentes + ontologias 

# Salvando

ggsave(
  "graficos/conceitos.jpeg",
  device = "jpeg",
  dpi = 600,
  width = 16,
  height = 8
)

# Juntando ferramentas ----------------------------------------------------

rdf + zenodo + dryad + dataverse + figshare + csv

# Salvando

ggsave(
  "graficos/ferramentas.jpeg",
  device = "jpeg",
  dpi = 600,
  width = 18,
  height = 9
)

