#TAULES ANNEXES
##1
library(dplyr)

taula_annex_poblacio_icb <- dataset %>%
  mutate(
    Mes = factor(
      month,
      levels = 1:12,
      labels = c("Gener", "Febrer", "Març", "Abril", "Maig", "Juny",
                 "Juliol", "Agost", "Setembre", "Octubre", "Novembre", "Desembre")
    )
  ) %>%
  select(
    Any = year,
    Mes,
    `Codi ICB` = icb_code,
    `Nom ICB` = icb_name,
    `Població registrada` = patients_registered
  ) %>%
  arrange(Any, `Codi ICB`, Mes)
View(taula_annex_poblacio_icb) 

##2
taula_annex_activitat_ap_icb <- dataset %>%
  mutate(
    Mes = factor(
      month,
      levels = 1:12,
      labels = c("Gener", "Febrer", "Març", "Abril", "Maig", "Juny",
                 "Juliol", "Agost", "Setembre", "Octubre", "Novembre", "Desembre")
    )
  ) %>%
  select(
    Any = year,
    Mes,
    `Codi ICB` = icb_code,
    `Nom ICB` = icb_name,
    `Consultes AP totals` = gp_appointments_total,
    `Consultes AP per 1.000 persones registrades` = gp_appointments_per_1000
  ) %>%
  arrange(Any, `Codi ICB`, Mes)
View(taula_annex_activitat_ap_icb)

##3
taula_annex_modes_icb <- dataset %>%
  mutate(
    Mes = factor(
      month,
      levels = 1:12,
      labels = c("Gener", "Febrer", "Març", "Abril", "Maig", "Juny",
                 "Juliol", "Agost", "Setembre", "Octubre", "Novembre", "Desembre")
    ),
    pct_presencial = (gp_appointments_face_to_face / gp_appointments_total) * 100,
    pct_home_visit = (gp_appointments_home_visit / gp_appointments_total) * 100,
    pct_telefonica = (gp_appointments_telephone / gp_appointments_total) * 100,
    pct_video_online = (gp_appointments_video_online / gp_appointments_total) * 100,
    pct_remota_total = gp_pct_remote,
    pct_unknown_mode = gp_pct_unknown_mode
  ) %>%
  select(
    Any = year,
    Mes,
    `Codi ICB` = icb_code,
    `Nom ICB` = icb_name,
    `Consultes totals AP` = gp_appointments_total,
    `Presencial (%)` = pct_presencial,
    `Visita domiciliària (%)` = pct_home_visit,
    `Telefònica (%)` = pct_telefonica,
    `Vídeo online (%)` = pct_video_online,
    `Remota total (%)` = pct_remota_total,
    `Mode desconegut (%)` = pct_unknown_mode
  ) %>%
  arrange(Any, `Codi ICB`, Mes)
View(taula_annex_modes_icb)

install.packages("writexl")
library(writexl)
write_xlsx(
  list(
    "Poblacio per ICB" = taula_annex_poblacio_icb,
    "Activitat AP per ICB" = taula_annex_activitat_ap_icb,
    "Modes consulta per ICB" = taula_annex_modes_icb
  ),
  "taules_annexos_per_ICB.xlsx"
)
getwd()


##taules annexes models bi i multi variat
library(broom)
library(writexl)

taula_model_bivariat <- tidy(model_bivariat, conf.int = TRUE)

taula_model_interaccio <- tidy(model_interaccio, conf.int = TRUE)

taula_model_multivariat <- tidy(model_multivariat, conf.int = TRUE)

taula_model_multivariat_interaccio <- tidy(model_multivariat_interaccio, conf.int = TRUE)

write_xlsx(
  list(
    "Model bivariat" = taula_model_bivariat,
    "Bivariat interaccio" = taula_model_interaccio,
    "Model multivariat" = taula_model_multivariat,
    "Multivariat interaccio" = taula_model_multivariat_interaccio
  ),
  "taules_models_regressio.xlsx"
)




##ANNEX TIPUS D'URGÈNCIES
library(dplyr)
library(tidyr)
library(writexl)

resum_mediana_riq <- function(x, digits = 2) {
  mediana <- median(x, na.rm = TRUE)
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  
  paste0(
    round(mediana, digits),
    " [",
    round(q1, digits),
    "–",
    round(q3, digits),
    "]"
  )
}

dataset <- dataset %>%
  mutate(
    ae_pct_type1 = dataset$A.E_attendances_Type.1 / dataset$sum_ae_attendances_total * 100,
    ae_pct_type2 = dataset$A.E_attendances_Type.2 / dataset$sum_ae_attendances_total * 100,
    ae_pct_other = dataset$A.E_attendances_Other_Department / dataset$sum_ae_attendances_total * 100,
    
    emerg_pct_type1 = dataset$Emergency._Type.1 / dataset$sum_ae_emergencies_total * 100,
    emerg_pct_type2 = dataset$Emergency_Type.2 / dataset$sum_ae_emergencies_total * 100,
    emerg_pct_other = dataset$Emergency._Other_department / dataset$sum_ae_emergencies_total * 100
  )


taula_pes_ae <- bind_rows(
  
  dataset %>%
    summarise(
      Any = "Total",
      `% A&E attendances Type 1` = resum_mediana_riq(ae_pct_type1, 2),
      `% A&E attendances Type 2` = resum_mediana_riq(ae_pct_type2, 2),
      `% A&E attendances Other` = resum_mediana_riq(ae_pct_other, 2),
      `% Emergency admissions Type 1` = resum_mediana_riq(emerg_pct_type1, 2),
      `% Emergency admissions Type 2` = resum_mediana_riq(emerg_pct_type2, 2),
      `% Emergency admissions Other` = resum_mediana_riq(emerg_pct_other, 2)
    ),
  
  dataset %>%
    group_by(year) %>%
    summarise(
      Any = as.character(first(year)),
      `% A&E attendances Type 1` = resum_mediana_riq(ae_pct_type1, 2),
      `% A&E attendances Type 2` = resum_mediana_riq(ae_pct_type2, 2),
      `% A&E attendances Other` = resum_mediana_riq(ae_pct_other, 2),
      `% Emergency admissions Type 1` = resum_mediana_riq(emerg_pct_type1, 2),
      `% Emergency admissions Type 2` = resum_mediana_riq(emerg_pct_type2, 2),
      `% Emergency admissions Other` = resum_mediana_riq(emerg_pct_other, 2),
      .groups = "drop"
    )
)

taula_pes_ae_final <- taula_pes_ae %>%
  select(-year) %>% 
  pivot_longer(
    cols = -Any,
    names_to = "Variable",
    values_to = "Valor"
  ) %>%
  pivot_wider(
    names_from = Any,
    values_from = Valor
  ) %>%
  select(Variable, Total, `2022`, `2025`)

View(taula_pes_ae_final)

write_xlsx(
  taula_pes_ae_final,
  "taula_pes_relatiu_ae_tipus.xlsx"
)

