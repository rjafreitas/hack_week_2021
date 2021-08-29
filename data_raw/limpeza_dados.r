# carregando pacotes
library(tidyverse)

bb <- readr::read_delim(
  "data_raw/form_inicial.csv",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE
)


banco <- bb %>% mutate(
  educacao = factor(
    educacao,
    levels = c(
      "Graduação (incompleta)",
      "Graduação (completa)",
      "Mestrado (incompleto)",
      "Mestrado (completo)",
      "Doutorado (incompleto)",
      "Doutorado (completo)",
      "Pesquisa de Pós-Doutorado (atual)",
      "Docência (atual)"
    )
  ),
  formacao_dados = factor(
    formacao_dados,
    levels = c("Sim (institucionalmente)",
               "Sim (curso online)",
               "Não")
  ),
  dados_terceiros = factor(
    dados_terceiros,
    levels = c(
      "Nunca",
      "Pouco frequente",
      "Com alguma frequência",
      "Muito frequente",
      "Sempre"
    )
  ),
  grupo_disp_dados = factor(
    grupo_disp_dados,
    levels = c(
      "Nunca",
      "Pouco frequente",
      "Com alguma frequência",
      "Muito frequente",
      "Sempre"
    )
  ),
  dados_publicos = factor(
    dados_publicos,
    levels = c(
      "Nunca",
      "Pouco frequente",
      "Com alguma frequência",
      "Muito frequente",
      "Sempre"
    )
  ),
  disponibilizo_banco = factor(
    disponibilizo_banco,
    levels = c(
      "Nunca",
      "Pouco frequente",
      "Com alguma frequência",
      "Muito frequente",
      "Sempre"
    )
  ),
  disponibilizacao_area = factor(
    disponibilizacao_area,
    levels = c(
      "Nunca",
      "Pouco frequente",
      "Com alguma frequência",
      "Muito frequente",
      "Sempre"
    )
  ),
  incentivo_orientador = factor(
    incentivo_orientador,
    levels = c(
      "Sim, de forma obrigatória",
      "Sim, sem obrigatoriedade",
      "Não",
      "Sou desincentivado",
      "Não sei",
      "Não se aplica"
    )
  ),
  incentivo_fomento = factor(
    incentivo_fomento,
    levels = c(
      "Sim, de forma obrigatória",
      "Sim, sem obrigatoriedade",
      "Não",
      "Sou desincentivado",
      "Não sei",
      "Não se aplica"
    )
  ),
  incentivo_ppg = factor(
    incentivo_ppg,
    levels = c(
      "Sim, de forma obrigatória",
      "Sim, sem obrigatoriedade",
      "Não",
      "Sou desincentivado",
      "Não sei",
      "Não se aplica"
    )
  ),
  recursos_zenodo = factor(
    recursos_zenodo,
    levels = c(
      "Nunca ouvi falar",
      "O nome não é estranho",
      "Conheço, mas nunca usei/apliquei",
      "Já usei/apliquei, mas pouco",
      "Uso/aplico no meu dia-a-dia"
    )
  ),
  recursos_dryad = factor(
    recursos_dryad,
    levels = c(
      "Nunca ouvi falar",
      "O nome não é estranho",
      "Conheço, mas nunca usei/apliquei",
      "Já usei/apliquei, mas pouco",
      "Uso/aplico no meu dia-a-dia"
    )
  ),
  recursos_figshare = factor(
    recursos_figshare,
    levels = c(
      "Nunca ouvi falar",
      "O nome não é estranho",
      "Conheço, mas nunca usei/apliquei",
      "Já usei/apliquei, mas pouco",
      "Uso/aplico no meu dia-a-dia"
    )
  ),
  recursos_dataverse = factor(
    recursos_dataverse,
    levels = c(
      "Nunca ouvi falar",
      "O nome não é estranho",
      "Conheço, mas nunca usei/apliquei",
      "Já usei/apliquei, mas pouco",
      "Uso/aplico no meu dia-a-dia"
    )
  ),
  recursos_fair = factor(
    recursos_fair,
    levels = c(
      "Nunca ouvi falar",
      "O nome não é estranho",
      "Conheço, mas nunca usei/apliquei",
      "Já usei/apliquei, mas pouco",
      "Uso/aplico no meu dia-a-dia"
    )
  ),
  recursos_licenca = factor(
    recursos_licenca,
    levels = c(
      "Nunca ouvi falar",
      "O nome não é estranho",
      "Conheço, mas nunca usei/apliquei",
      "Já usei/apliquei, mas pouco",
      "Uso/aplico no meu dia-a-dia"
    )
  ),
  recursos_csv = factor(
    recursos_csv,
    levels = c(
      "Nunca ouvi falar",
      "O nome não é estranho",
      "Conheço, mas nunca usei/apliquei",
      "Já usei/apliquei, mas pouco",
      "Uso/aplico no meu dia-a-dia"
    )
  ),
  recursos_dicionario = factor(
    recursos_dicionario,
    levels = c(
      "Nunca ouvi falar",
      "O nome não é estranho",
      "Conheço, mas nunca usei/apliquei",
      "Já usei/apliquei, mas pouco",
      "Uso/aplico no meu dia-a-dia"
    )
  ),
  recursos_ontologias = factor(
    recursos_ontologias,
    levels = c(
      "Nunca ouvi falar",
      "O nome não é estranho",
      "Conheço, mas nunca usei/apliquei",
      "Já usei/apliquei, mas pouco",
      "Uso/aplico no meu dia-a-dia"
    )
  ),
  recursos_identi_persistentes = factor(
    recursos_identi_persistentes,
    levels = c(
      "Nunca ouvi falar",
      "O nome não é estranho",
      "Conheço, mas nunca usei/apliquei",
      "Já usei/apliquei, mas pouco",
      "Uso/aplico no meu dia-a-dia"
    )
  ),
  recursos_rdf = factor(
    recursos_rdf,
    levels = c(
      "Nunca ouvi falar",
      "O nome não é estranho",
      "Conheço, mas nunca usei/apliquei",
      "Já usei/apliquei, mas pouco",
      "Uso/aplico no meu dia-a-dia"
    )
  ),
  recursos_gestao_dados = factor(
    recursos_gestao_dados,
    levels = c(
      "Nunca ouvi falar",
      "O nome não é estranho",
      "Conheço, mas nunca usei/apliquei",
      "Já usei/apliquei, mas pouco",
      "Uso/aplico no meu dia-a-dia"
    )
  )
)

write_rds(banco, "data/piloto_hack.rds")
