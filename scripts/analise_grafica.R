# Carregando pacotes

library(tidyverse)
library(patchwork)

# Abrindo a base

hw21 <- readr::read_rds("data/piloto_hack.rds")

# view(hw21)

# 1. Conhecimento sobre utilização de dados -------------------------------
# Criando função para os gráficos do item 1.
graficos_dados <- function(x) {
  hw21 %>%
    ggplot(aes(x = {{x}}, fill = {{x}})) +
    geom_bar(width = 0.6) +
    stat_count(
      geom = "text",
      aes(label = stat(count)),
      position = position_stack(vjust = 0.5),
      colour = "#e9ebf0"
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
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.text = element_text(size = 12),
      axis.title.y = element_text(size = 12)
    ) +
    guides(fill = guide_legend())
}

# 1.1 Utilizo dados produzidos por terceiros.
d_terceiros <- graficos_dados(dados_terceiros) +
  labs(title = "Dados de terceiros",
       y = "Contagem") +
  theme(text = element_text(size = 16))

# 1.2 Meu grupo de pesquisa disponibiliza os dados.
d_grupo <- graficos_dados(grupo_disp_dados) +
  labs(title = "Disponibilização pelo grupo",
       y = "") +
  theme(legend.position = "none")

# 1.3 Trabalho com dados públicos.
d_publicos <- graficos_dados(dados_publicos) +
  labs(title = "Dados públicos",
       y = "") +
  theme(legend.position = "none")

# 1.4 Disponibilizo meu banco de dados em alguma plataforma.
d_banco <- graficos_dados(disponibilizo_banco) +
  labs(title = "Disponibilização em plataformas",
       y = "") +
  theme(legend.position = "none")

# 1.5 Na minha área de pesquisa é comum disponibilizar dados.
d_area <- graficos_dados(disponibilizacao_area) +
  labs(title = "Comum a disponibilização na área",
       y = "") +
  theme(legend.position = "none")

# 1.6 Juntando
d_terceiros / (d_area + d_banco + d_grupo + d_publicos)

# Salvando:

# ggsave(
#   "graficos/uso_dados.jpeg",
#   device = "jpeg",
#   dpi = 600,
#   width = 9,
#   height = 7
# )

# 2. Incentivo a disponibilização de dados --------------------------------
# Criando função para os gráficos do item 2.
graficos_incentivo <- function(x) {
  hw21 %>%
    ggplot(aes(x = {{x}}, fill = {{x}})) +
    geom_bar(width = 0.6) +
    stat_count(
      geom = "text",
      aes(label = stat(count)),
      position = position_stack(vjust = 0.5),
      colour = "#e9ebf0"
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
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      text = element_text(size = 16),
      legend.text = element_text(size = 12),
      axis.title.y = element_text(size = 12)
    ) +
    guides(fill = guide_legend())
}

# 2.1 Orientador/Supervisor.
inc_orient <- graficos_incentivo(incentivo_orientador) +
  labs(title = "Orientador/Supervisor",
       y = "Contagem")

# 2.2 Programa de pós graduação/instituição.
inc_prog <- graficos_incentivo(incentivo_ppg) +
  labs(title = "PPG/instituição",
       y = "") +
  theme(legend.position = "none")

# 2.3 Fonte de fomento (ex: CAPES)
inc_fom <- graficos_incentivo(incentivo_fomento) +
  labs(title = "Fonte de fomento",
       y = "") +
  theme(legend.position = "none")

# 2.4 Juntando
inc_orient | inc_fom | inc_prog

# Salvando:

# ggsave(
#   "graficos/incentivo.jpeg",
#   device = "jpeg",
#   dpi = 600,
#   width = 12,
#   height = 3
# )


# 3. Recursos familiares --------------------------------------------------
# Função para gráficos de recursos e conceitos familiares

graficos_rc <- function(x) {
  hw21 %>%
    ggplot(aes(x = {{x}}, fill = {{x}})) +
    geom_bar(width = 0.6) +
    stat_count(
      geom = "text",
      aes(label = stat(count)),
      position = position_stack(vjust = 0.5),
      colour = "#e9ebf0"
    ) + 
    scale_fill_manual(
      "Legenda",
      values = c(
        "Nunca ouvi falar" = "#9eb3c2",
        "O nome não é estranho" = "#1c7293",
        "Conheço, mas nunca usei/apliquei" = "#065a82",
        "Já usei/apliquei, mas pouco" = "#1b3b6f",
        "Uso/aplico no meu dia-a-dia" = "#21295c"
      )
    ) + 
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      text = element_text(size = 16),
      legend.text = element_text(size = 12),
      axis.title.y = element_text(size = 12)
    ) +
    guides(fill = guide_legend())
}

# 3.1 Research data framework (RDF)
rdf <- graficos_rc(recursos_rdf) +
  labs(title = "Research Data Framework (RDF)",
       y = "") +
  theme(legend.position = "none")

# 3.2 Zenodo
zenodo <- graficos_rc(recursos_zenodo) +
  labs(title = "Zenodo",
       y = "") +
  theme(legend.position = "none")

# 3.3 Dryad (colocar legenda aqui)
dryad <- graficos_rc(recursos_dryad) +
  labs(title = "Dryad",
       y = "")

# 3.4 Dataverse
dataverse <- graficos_rc(recursos_dataverse) +
  labs(title = "Dataverse",
       y = "") +
  theme(legend.position = "none")

# 3.5 Figshare
figshare <- graficos_rc(recursos_figshare) +
  labs(title = "Figshare",
       y = "") +
  theme(legend.position = "none")

# 3.6 .csv
csv <- graficos_rc(recursos_csv) +
  labs(title = ".csv",
       y = "") +
  theme(legend.position = "none")

# 3.7 Juntando
rdf + zenodo + dryad + dataverse + figshare + csv

# Salvando

# ggsave(
#   "graficos/ferramentas.jpeg",
#   device = "jpeg",
#   dpi = 600,
#   width = 18,
#   height = 9
# )

# 4. Conceitos familiares -------------------------------------------------
# 4.1 Gestão de dados
gestao <- graficos_rc(recursos_gestao_dados) +
  labs(title = "Gestão de dados",
       y = "") +
  theme(legend.position = "none")

# 4.2 Dicionário de dados
dicionario <- graficos_rc(recursos_dicionario) +
  labs(title = "Gestão de dados",
       y = "") +
  theme(legend.position = "none")

# 4.3 Princípio FAIR
fair <- graficos_rc(recursos_fair) +
  labs(title = "Princípios FAIR",
       y = "")

# 4. Licença
licenca <- graficos_rc(recursos_licenca) +
  labs(title = "Licença",
       y = "") +
  theme(legend.position = "none")

# 4.5 Identificadores persistentes
persistentes <- graficos_rc(recursos_identi_persistentes) +
  labs(title = "Identificadores persistentes",
       y = "") +
  theme(legend.position = "none")

# 4.6 Ontologias
ontologias <- graficos_rc(recursos_ontologias) +
  labs(title = "Ontologias",
       y = "") +
  theme(legend.position = "none")

# 4.7 Juntando
gestao + dicionario + fair + licenca + persistentes + ontologias 

# Salvando

# ggsave(
#   "graficos/teste.jpeg",
#   device = "jpeg",
#   dpi = 600,
#   width = 16,
#   height = 8
# )
