library(tidyverse)
library(here)

survey <- read_csv(here("data/survey.csv"))

initial_length <- nrow(survey)

# Changing column names

colnames(survey) <- c(
    'data_hora', 
    'idade',
    'genero',
    'periodo',
    'cursadas',
    
    'prob_tec-mat',
    'prob_tec-comp',
    'prob_tec-prat',
    
    'comunic_prej-mat',
    'comunic_prej-comp',
    'comunic_prej-prat',
    
    'prob_ger_temp-mat',
    'prob_ger_temp-comp',
    'prob_ger_temp-prat',
    
    'falta_contato-mat',
    'falta_contato-comp',
    'falta_contato-prat',
    
    'amb_nao_prod-mat',
    'amb_nao_prod-comp',
    'amb_nao_prod-prat',
    
    'flex_horario-mat',
    'flex_horario-comp',
    'flex_horario-prat',
    
    'estrut_clara-mat',
    'estrut_clara-comp',
    'estrut_clara-prat',
    
    'ritmo_proprio-mat',
    'ritmo_proprio-comp',
    'ritmo_proprio-prat',
    
    'dificuldades_n_citadas',
    'positivos_n_citados',
    
    'indice_aprendizado'
) 

aspectos <- c(
    'prob_tec-mat',
    'prob_tec-comp',
    'prob_tec-prat',
    
    'comunic_prej-mat',
    'comunic_prej-comp',
    'comunic_prej-prat',
    
    'prob_ger_temp-mat',
    'prob_ger_temp-comp',
    'prob_ger_temp-prat',
    
    'falta_contato-mat',
    'falta_contato-comp',
    'falta_contato-prat',
    
    'amb_nao_prod-mat',
    'amb_nao_prod-comp',
    'amb_nao_prod-prat',
    
    'flex_horario-mat',
    'flex_horario-comp',
    'flex_horario-prat',
    
    'estrut_clara-mat',
    'estrut_clara-comp',
    'estrut_clara-prat',
    
    'ritmo_proprio-mat',
    'ritmo_proprio-comp',
    'ritmo_proprio-prat'
)

aspect_type <- function(aspecto) {
    dificuldades <- c(
        'prob_tec',
        'comunic_prej',
        'prob_ger_temp',
        'falta_contato',
        'amb_nao_prod'
    )
    
    ifelse(aspecto %in% dificuldades, "dificuldade", "positivo")
}

survey <- survey %>%
        pivot_longer(aspectos, names_to = "aspectos", values_to = "avaliacao") %>%
        separate(aspectos, into = c("aspecto", "disciplinas"), sep = "-") %>%
        mutate(tipo = aspect_type(aspecto)) %>%
        mutate(comp_cursadas = ifelse(grepl("Teóricas de computação/gerais", cursadas, fixed=TRUE), 1, 0),
               mat_cursadas = ifelse(grepl("Teóricas da base matemática/estatística do curso", cursadas, fixed=TRUE), 1, 0),
               prat_cursadas = ifelse(grepl( "Práticas/Laboratórios", cursadas, fixed=TRUE), 1, 0)) %>%
        select(-c("cursadas", "positivos_n_citados", "dificuldades_n_citadas")) %>%
    slice(n = 25:(initial_length*24))

View(survey)

# Saving CSV
write.csv(survey, here("data/modified_survey.csv"), row.names = FALSE)

