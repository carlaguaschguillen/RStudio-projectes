#CANVI NOM
library(dplyr)

dataset <- dataset %>%
  rename(
    sum_ae_attendances_type1 = A.E_attendances_Type.1,
    sum_ae_attendances_type2 = A.E_attendances_Type.2,
    sum_ae_attendances_type3 = A.E_attendances_Other_Department,
    
    sum_ae_emergencies_type1 = Emergency._Type.1,
    sum_ae_emergencies_type2 = Emergency_Type.2,
    sum_ae_emergencies_type3 = Emergency._Other_department
  )

#CREACIÓ TAXES
dataset <- dataset %>%
  mutate(
    ae_attendances_type1_per_1000 = sum_ae_attendances_type1 / patients_registered * 1000,
    ae_attendances_type2_per_1000 = sum_ae_attendances_type2 / patients_registered * 1000,
    ae_attendances_type3_per_1000 = sum_ae_attendances_type3 / patients_registered * 1000,
    
    ae_emergencies_type1_per_1000 = sum_ae_emergencies_type1 / patients_registered * 1000,
    ae_emergencies_type2_per_1000 = sum_ae_emergencies_type2 / patients_registered * 1000,
    ae_emergencies_type3_per_1000 = sum_ae_emergencies_type3 / patients_registered * 1000
  )

#ATTENDANCES
##Bivariat simple
model_att_type1_biv <- lm(
  ae_attendances_type1_per_1000 ~ gp_pct_remote,
  data = dataset
)

model_att_type2_biv <- lm(
  ae_attendances_type2_per_1000 ~ gp_pct_remote,
  data = dataset
)

model_att_type3_biv <- lm(
  ae_attendances_type3_per_1000 ~ gp_pct_remote,
  data = dataset
)

summary(model_att_type1_biv)
summary(model_att_type2_biv)
summary(model_att_type3_biv)

##Bivariat interacció
model_att_type1_biv_interaccio <- lm(
  ae_attendances_type1_per_1000 ~ gp_pct_remote * year,
  data = dataset
)

model_att_type2_biv_interaccio <- lm(
  ae_attendances_type2_per_1000 ~ gp_pct_remote * year,
  data = dataset
)

model_att_type3_biv_interaccio <- lm(
  ae_attendances_type3_per_1000 ~ gp_pct_remote * year,
  data = dataset
)

summary(model_att_type1_biv_interaccio)
summary(model_att_type2_biv_interaccio)
summary(model_att_type3_biv_interaccio)

##Multivariat simple
model_att_type1_multi <- lm(
  ae_attendances_type1_per_1000 ~ gp_pct_remote + gp_appointments_per_1000 + year + month + icb_code,
  data = dataset
)

model_att_type2_multi <- lm(
  ae_attendances_type2_per_1000 ~ gp_pct_remote + gp_appointments_per_1000 + year + month + icb_code,
  data = dataset
)

model_att_type3_multi <- lm(
  ae_attendances_type3_per_1000 ~ gp_pct_remote + gp_appointments_per_1000 + year + month + icb_code,
  data = dataset
)

summary(model_att_type1_multi)
summary(model_att_type2_multi)
summary(model_att_type3_multi)

##Multivariat interacció
model_att_type1_multi_interaccio <- lm(
  ae_attendances_type1_per_1000 ~ gp_pct_remote * year + gp_appointments_per_1000 + month + icb_code,
  data = dataset
)

model_att_type2_multi_interaccio <- lm(
  ae_attendances_type2_per_1000 ~ gp_pct_remote * year + gp_appointments_per_1000 + month + icb_code,
  data = dataset
)

model_att_type3_multi_interaccio <- lm(
  ae_attendances_type3_per_1000 ~ gp_pct_remote * year + gp_appointments_per_1000 + month + icb_code,
  data = dataset
)

summary(model_att_type1_multi_interaccio)
summary(model_att_type2_multi_interaccio)
summary(model_att_type3_multi_interaccio)


#EMERGENCIES
##Bivariat simple
model_emerg_type1_biv <- lm(
  ae_emergencies_type1_per_1000 ~ gp_pct_remote,
  data = dataset
)

model_emerg_type2_biv <- lm(
  ae_emergencies_type2_per_1000 ~ gp_pct_remote,
  data = dataset
)

model_emerg_type3_biv <- lm(
  ae_emergencies_type3_per_1000 ~ gp_pct_remote,
  data = dataset
)

summary(model_emerg_type1_biv)
summary(model_emerg_type2_biv)
summary(model_emerg_type3_biv)

##Bivariat interacció
model_emerg_type1_biv_interaccio <- lm(
  ae_emergencies_type1_per_1000 ~ gp_pct_remote * year,
  data = dataset
)

model_emerg_type2_biv_interaccio <- lm(
  ae_emergencies_type2_per_1000 ~ gp_pct_remote * year,
  data = dataset
)

model_emerg_type3_biv_interaccio <- lm(
  ae_emergencies_type3_per_1000 ~ gp_pct_remote * year,
  data = dataset
)

summary(model_emerg_type1_biv_interaccio)
summary(model_emerg_type2_biv_interaccio)
summary(model_emerg_type3_biv_interaccio)

##Multivariat simple
model_emerg_type1_multi <- lm(
  ae_emergencies_type1_per_1000 ~ gp_pct_remote + gp_appointments_per_1000 + year + month + icb_code,
  data = dataset
)

model_emerg_type2_multi <- lm(
  ae_emergencies_type2_per_1000 ~ gp_pct_remote + gp_appointments_per_1000 + year + month + icb_code,
  data = dataset
)

model_emerg_type3_multi <- lm(
  ae_emergencies_type3_per_1000 ~ gp_pct_remote + gp_appointments_per_1000 + year + month + icb_code,
  data = dataset
)

summary(model_emerg_type1_multi)
summary(model_emerg_type2_multi)
summary(model_emerg_type3_multi)

##Multivariat interacció
model_emerg_type1_multi_interaccio <- lm(
  ae_emergencies_type1_per_1000 ~ gp_pct_remote * year + gp_appointments_per_1000 + month + icb_code,
  data = dataset
)

model_emerg_type2_multi_interaccio <- lm(
  ae_emergencies_type2_per_1000 ~ gp_pct_remote * year + gp_appointments_per_1000 + month + icb_code,
  data = dataset
)

model_emerg_type3_multi_interaccio <- lm(
  ae_emergencies_type3_per_1000 ~ gp_pct_remote * year + gp_appointments_per_1000 + month + icb_code,
  data = dataset
)

summary(model_emerg_type1_multi_interaccio)
summary(model_emerg_type2_multi_interaccio)
summary(model_emerg_type3_multi_interaccio)



#RECONTRAPEDAZO TAULA
library(dplyr)
library(broom)
library(writexl)

dataset <- dataset %>%
  mutate(
    year = factor(year),
    month = factor(month),
    icb_code = factor(icb_code)
  )

###1º codi, pas 3
extreure_valors_model <- function(model, nom_outcome, tipus_model) {
  
  coeficients <- tidy(model)
  ajust <- glance(model)
  
  beta_remote <- coeficients %>%
    filter(term == "gp_pct_remote") %>%
    pull(estimate)
  
  p_remote <- coeficients %>%
    filter(term == "gp_pct_remote") %>%
    pull(p.value)
  
  beta_interaccio <- coeficients %>%
    filter(term == "gp_pct_remote:year2025") %>%
    pull(estimate)
  
  p_interaccio <- coeficients %>%
    filter(term == "gp_pct_remote:year2025") %>%
    pull(p.value)
  
  beta_gp_appointments <- coeficients %>%
    filter(term == "gp_appointments_per_1000") %>%
    pull(estimate)
  
  p_gp_appointments <- coeficients %>%
    filter(term == "gp_appointments_per_1000") %>%
    pull(p.value)
  
  beta_year2025 <- coeficients %>%
    filter(term == "year2025") %>%
    pull(estimate)
  
  p_year2025 <- coeficients %>%
    filter(term == "year2025") %>%
    pull(p.value)
  
  # Si el model no té interacció, aquests valors quedaran com NA
  if (length(beta_interaccio) == 0) beta_interaccio <- NA
  if (length(p_interaccio) == 0) p_interaccio <- NA
  
  if (length(beta_gp_appointments) == 0) beta_gp_appointments <- NA
  if (length(p_gp_appointments) == 0) p_gp_appointments <- NA
  
  if (length(beta_year2025) == 0) beta_year2025 <- NA
  if (length(p_year2025) == 0) p_year2025 <- NA
  
  # En models amb interacció: beta 2025 = beta 2022 + beta interacció
  # En models sense interacció: beta 2025 no aplica
  beta_remote_2025 <- ifelse(
    is.na(beta_interaccio),
    NA,
    beta_remote + beta_interaccio
  )
  
  interpretacio <- case_when(
    is.na(beta_interaccio) & beta_remote > 0 & p_remote < 0.05 ~
      "Associació positiva significativa entre consulta remota i outcome.",
    
    is.na(beta_interaccio) & beta_remote < 0 & p_remote < 0.05 ~
      "Associació negativa significativa entre consulta remota i outcome.",
    
    is.na(beta_interaccio) & p_remote >= 0.05 ~
      "No s'observa associació significativa entre consulta remota i outcome.",
    
    !is.na(beta_interaccio) & p_interaccio < 0.05 ~
      "La interacció és significativa: l'associació difereix entre 2022 i 2025.",
    
    !is.na(beta_interaccio) & p_interaccio >= 0.05 ~
      "La interacció no és significativa: no hi ha evidència clara de diferències entre anys.",
    
    TRUE ~
      "Interpretació no disponible."
  )
  
  tibble(
    outcome = nom_outcome,
    model = tipus_model,
    beta_remote_2022 = beta_remote,
    p_remote = p_remote,
    beta_interaccio_2025 = beta_interaccio,
    p_interaccio = p_interaccio,
    beta_remote_2025_estimat = beta_remote_2025,
    beta_gp_appointments = beta_gp_appointments,
    p_gp_appointments = p_gp_appointments,
    beta_year2025 = beta_year2025,
    p_year2025 = p_year2025,
    r2_ajustat = ajust$adj.r.squared,
    interpretacio_breu = interpretacio
  )
}

###2º codi, pas 4
# Type 1
att_type1_biv <- lm(ae_attendances_type1_per_1000 ~ gp_pct_remote, data = dataset)

att_type1_biv_interaccio <- lm(
  ae_attendances_type1_per_1000 ~ gp_pct_remote * year,
  data = dataset
)

att_type1_multi <- lm(
  ae_attendances_type1_per_1000 ~ gp_pct_remote + gp_appointments_per_1000 + year + month + icb_code,
  data = dataset
)

att_type1_multi_interaccio <- lm(
  ae_attendances_type1_per_1000 ~ gp_pct_remote * year + gp_appointments_per_1000 + month + icb_code,
  data = dataset
)


# Type 2
att_type2_biv <- lm(ae_attendances_type2_per_1000 ~ gp_pct_remote, data = dataset)

att_type2_biv_interaccio <- lm(
  ae_attendances_type2_per_1000 ~ gp_pct_remote * year,
  data = dataset
)

att_type2_multi <- lm(
  ae_attendances_type2_per_1000 ~ gp_pct_remote + gp_appointments_per_1000 + year + month + icb_code,
  data = dataset
)

att_type2_multi_interaccio <- lm(
  ae_attendances_type2_per_1000 ~ gp_pct_remote * year + gp_appointments_per_1000 + month + icb_code,
  data = dataset
)


# Type 3 / Other Department
att_type3_biv <- lm(ae_attendances_type3_per_1000 ~ gp_pct_remote, data = dataset)

att_type3_biv_interaccio <- lm(
  ae_attendances_type3_per_1000 ~ gp_pct_remote * year,
  data = dataset
)

att_type3_multi <- lm(
  ae_attendances_type3_per_1000 ~ gp_pct_remote + gp_appointments_per_1000 + year + month + icb_code,
  data = dataset
)

att_type3_multi_interaccio <- lm(
  ae_attendances_type3_per_1000 ~ gp_pct_remote * year + gp_appointments_per_1000 + month + icb_code,
  data = dataset
)

###3º codi, pas 5
taula_attendances_tipus <- bind_rows(
  extreure_valors_model(att_type1_biv, "A&E attendances Type 1", "Bivariat simple"),
  extreure_valors_model(att_type1_biv_interaccio, "A&E attendances Type 1", "Bivariat amb interacció"),
  extreure_valors_model(att_type1_multi, "A&E attendances Type 1", "Multivariable sense interacció"),
  extreure_valors_model(att_type1_multi_interaccio, "A&E attendances Type 1", "Multivariable amb interacció"),
  
  extreure_valors_model(att_type2_biv, "A&E attendances Type 2", "Bivariat simple"),
  extreure_valors_model(att_type2_biv_interaccio, "A&E attendances Type 2", "Bivariat amb interacció"),
  extreure_valors_model(att_type2_multi, "A&E attendances Type 2", "Multivariable sense interacció"),
  extreure_valors_model(att_type2_multi_interaccio, "A&E attendances Type 2", "Multivariable amb interacció"),
  
  extreure_valors_model(att_type3_biv, "A&E attendances Other Department", "Bivariat simple"),
  extreure_valors_model(att_type3_biv_interaccio, "A&E attendances Other Department", "Bivariat amb interacció"),
  extreure_valors_model(att_type3_multi, "A&E attendances Other Department", "Multivariable sense interacció"),
  extreure_valors_model(att_type3_multi_interaccio, "A&E attendances Other Department", "Multivariable amb interacció")
)

taula_attendances_tipus

###4º codi, pas 6
# Type 1
emerg_type1_biv <- lm(ae_emergencies_type1_per_1000 ~ gp_pct_remote, data = dataset)

emerg_type1_biv_interaccio <- lm(
  ae_emergencies_type1_per_1000 ~ gp_pct_remote * year,
  data = dataset
)

emerg_type1_multi <- lm(
  ae_emergencies_type1_per_1000 ~ gp_pct_remote + gp_appointments_per_1000 + year + month + icb_code,
  data = dataset
)

emerg_type1_multi_interaccio <- lm(
  ae_emergencies_type1_per_1000 ~ gp_pct_remote * year + gp_appointments_per_1000 + month + icb_code,
  data = dataset
)


# Type 2
emerg_type2_biv <- lm(ae_emergencies_type2_per_1000 ~ gp_pct_remote, data = dataset)

emerg_type2_biv_interaccio <- lm(
  ae_emergencies_type2_per_1000 ~ gp_pct_remote * year,
  data = dataset
)

emerg_type2_multi <- lm(
  ae_emergencies_type2_per_1000 ~ gp_pct_remote + gp_appointments_per_1000 + year + month + icb_code,
  data = dataset
)

emerg_type2_multi_interaccio <- lm(
  ae_emergencies_type2_per_1000 ~ gp_pct_remote * year + gp_appointments_per_1000 + month + icb_code,
  data = dataset
)


# Type 3 / Other Department
emerg_type3_biv <- lm(ae_emergencies_type3_per_1000 ~ gp_pct_remote, data = dataset)

emerg_type3_biv_interaccio <- lm(
  ae_emergencies_type3_per_1000 ~ gp_pct_remote * year,
  data = dataset
)

emerg_type3_multi <- lm(
  ae_emergencies_type3_per_1000 ~ gp_pct_remote + gp_appointments_per_1000 + year + month + icb_code,
  data = dataset
)

emerg_type3_multi_interaccio <- lm(
  ae_emergencies_type3_per_1000 ~ gp_pct_remote * year + gp_appointments_per_1000 + month + icb_code,
  data = dataset
)

###5º codi, pas 7
taula_emergencies_tipus <- bind_rows(
  extreure_valors_model(emerg_type1_biv, "Emergency admissions Type 1", "Bivariat simple"),
  extreure_valors_model(emerg_type1_biv_interaccio, "Emergency admissions Type 1", "Bivariat amb interacció"),
  extreure_valors_model(emerg_type1_multi, "Emergency admissions Type 1", "Multivariable sense interacció"),
  extreure_valors_model(emerg_type1_multi_interaccio, "Emergency admissions Type 1", "Multivariable amb interacció"),
  
  extreure_valors_model(emerg_type2_biv, "Emergency admissions Type 2", "Bivariat simple"),
  extreure_valors_model(emerg_type2_biv_interaccio, "Emergency admissions Type 2", "Bivariat amb interacció"),
  extreure_valors_model(emerg_type2_multi, "Emergency admissions Type 2", "Multivariable sense interacció"),
  extreure_valors_model(emerg_type2_multi_interaccio, "Emergency admissions Type 2", "Multivariable amb interacció"),
  
  extreure_valors_model(emerg_type3_biv, "Emergency admissions Other Department", "Bivariat simple"),
  extreure_valors_model(emerg_type3_biv_interaccio, "Emergency admissions Other Department", "Bivariat amb interacció"),
  extreure_valors_model(emerg_type3_multi, "Emergency admissions Other Department", "Multivariable sense interacció"),
  extreure_valors_model(emerg_type3_multi_interaccio, "Emergency admissions Other Department", "Multivariable amb interacció")
)

taula_emergencies_tipus

#EXCEL
write_xlsx(
  list(
    "Attendances per tipus" = taula_attendances_tipus,
    "Emergencies per tipus" = taula_emergencies_tipus
  ),
  "taules_models_per_tipus_attendances_emergencies.xlsx"
)

############################################################################################
#Annex 6 graficat
##1 creacio taula
install.packages("tidyverse")
library(tidyverse)

taula_tipus <- tribble(
  ~variable, ~servei, ~tipus, ~any, ~mediana, ~q1, ~q3,
  "% A&E attendances Type 1", "A&E attendances", "Type 1", "2022", 65.31, 57.67, 71.78,
  "% A&E attendances Type 1", "A&E attendances", "Type 1", "2025", 59.67, 54.91, 67.74,
  
  "% A&E attendances Type 2", "A&E attendances", "Type 2", "2022", 0.50, 0.00, 2.69,
  "% A&E attendances Type 2", "A&E attendances", "Type 2", "2025", 0.98, 0.00, 3.08,
  
  "% A&E attendances Other", "A&E attendances", "Other", "2022", 30.78, 22.05, 37.13,
  "% A&E attendances Other", "A&E attendances", "Other", "2025", 35.02, 25.12, 41.46,
  
  "% Emergency admissions Type 1", "Emergency admissions", "Type 1", "2022", 72.09, 66.70, 78.61,
  "% Emergency admissions Type 1", "Emergency admissions", "Type 1", "2025", 73.80, 66.64, 81.81,
  
  "% Emergency admissions Type 2", "Emergency admissions", "Type 2", "2022", 0.00, 0.00, 0.04,
  "% Emergency admissions Type 2", "Emergency admissions", "Type 2", "2025", 0.00, 0.00, 0.02,
  
  "% Emergency admissions Other", "Emergency admissions", "Other", "2022", 0.00, 0.00, 1.38,
  "% Emergency admissions Other", "Emergency admissions", "Other", "2025", 0.05, 0.00, 1.16
)

#2 grafic punts
ggplot(taula_tipus, aes(x = tipus, y = mediana, color = any)) +
  geom_point(
    position = position_dodge(width = 0.5),
    size = 3
  ) +
  geom_errorbar(
    aes(ymin = q1, ymax = q3),
    position = position_dodge(width = 0.5),
    width = 0.2
  ) +
  facet_wrap(~ servei) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20)
  ) +
  scale_color_manual(values = c("2022" = "#2E86AB", "2025" = "#2E7D32")) +
  labs(
    title = "Distribució percentual de les assistències a A&E i admissions urgents segons tipus de servei",
    x = "Tipus de servei",
    y = "Percentatge mensual per ICB, mediana [RIQ]",
    color = "Any"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold")
  )

