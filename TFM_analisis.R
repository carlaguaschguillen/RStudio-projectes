#INSTAL·LACIÓ
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
library(dplyr)
library(readr)
library(tidyr)

#CANVI DE VARIABLES CARACTER A NUMERIC
dataset <- dataset_operatiuTFM %>%
  mutate(
    gp_appointments_total = parse_number(
      gp_appointments_total,
      locale = locale(decimal_mark = ",", grouping_mark = ".")
    ),
    gp_appointments_face_to_face = parse_number(
      gp_appointments_face_to_face,
      locale = locale(decimal_mark = ",", grouping_mark = ".")
    ),
    gp_pct_remote = parse_number(
      gp_pct_remote,
      locale = locale(decimal_mark = ",", grouping_mark = ".")
    ),
    gp_pct_unknown_mode = parse_number(
      gp_pct_unknown_mode,
      locale = locale(decimal_mark = ",", grouping_mark = ".")
    ),
    patients_registered = parse_number(
      patients_registered,
      locale = locale(decimal_mark = ",", grouping_mark = ".")
    )
  )
str(dataset)
summary(dataset)

#CREACIÓ DE VARIABLES PER TAXES
dataset <- dataset %>%
  mutate(
    ae_attendances_per_1000 = (sum_ae_attendances_total / patients_registered) * 1000,
    ae_emergencies_per_1000 = (sum_ae_emergencies_total / patients_registered) * 1000,
    gp_appointments_per_1000 = (gp_appointments_total / patients_registered) * 1000
  )

#GENERACIÓ DESCRIPTIUS MEDIANA I RIQ
resum_mediana_riq <- function(x, decimals = 2) {
  paste0(
    round(median(x, na.rm = TRUE), decimals),
    " [",
    round(quantile(x, 0.25, na.rm = TRUE), decimals),
    "-",
    round(quantile(x, 0.75, na.rm = TRUE), decimals),
    "]"
  )
}

#RESUM DESCRIPTIUS MEDIANA I RIQ
descriptius_generals <- dataset %>%
  summarise(
    # Variables temporals
    `Any` = resum_mediana_riq(year, 0),
    `Mes` = resum_mediana_riq(month, 0),
    
    # Consultes GP
    `Cites GP totals` = resum_mediana_riq(gp_appointments_total, 0),
    `Cites GP presencials` = resum_mediana_riq(gp_appointments_face_to_face, 0),
    `Cites GP a domicili` = resum_mediana_riq(gp_appointments_home_visit, 2),
    `Cites GP telefòniques` = resum_mediana_riq(gp_appointments_telephone, 2),
    `Cites GP per vídeo/online` = resum_mediana_riq(gp_appointments_video_online, 2),
    `Cites GP amb mode desconegut` = resum_mediana_riq(gp_appointments_unknown_mode, 2),
    `Cites GP remotes` = resum_mediana_riq(gp_appointments_remote, 2),
    
    # Percentatges GP
    `Consultes remotes (%)` = resum_mediana_riq(gp_pct_remote, 2),
    `Mode desconegut (%)` = resum_mediana_riq(gp_pct_unknown_mode, 2),
    
    # Població
    `Pacients registrats` = resum_mediana_riq(patients_registered, 0),
    
    # A&E - emergències/admissions
    `Emergency - Other department` = resum_mediana_riq(`Emergency._Other_department`, 0),
    `Emergency - Type 2` = resum_mediana_riq(`Emergency_Type.2`, 0),
    `Emergency - Type 1` = resum_mediana_riq(`Emergency._Type.1`, 0),
    
    # A&E - attendances
    `A&E attendances - Other department` = resum_mediana_riq(`A.E_attendances_Other_Department`, 0),
    `A&E attendances - Type 2` = resum_mediana_riq(`A.E_attendances_Type.2`, 0),
    `A&E attendances - Type 1` = resum_mediana_riq(`A.E_attendances_Type.1`, 0),
    
    # Totals A&E
    `A&E attendances totals` = resum_mediana_riq(sum_ae_attendances_total, 0),
    `Emergency admissions totals` = resum_mediana_riq(sum_ae_emergencies_total, 0),
    
    # Taxes per 1.000
    `A&E attendances per 1.000` = resum_mediana_riq(ae_attendances_per_1000, 2),
    `Emergency admissions per 1.000` = resum_mediana_riq(ae_emergencies_per_1000, 2),
    `GP appointments per 1.000` = resum_mediana_riq(gp_appointments_per_1000, 2)
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Mediana_RIQ"
  )

descriptius_generals
View(descriptius_generals)

##mitjana i ds %desconegut
dataset %>%
  filter(year %in% c(2022,2025))%>%
  group_by(year) %>%
  summarise(
    mitjana = mean(gp_pct_unknown_mode, na.rm=TRUE),
    Desviacio_estandar = sd(gp_pct_unknown_mode, na.rm=TRUE)
  )
##mediana i riq pacients
dataset %>%
  group_by(year) %>%
  summarise(
    resum = paste0(
      round(median(patients_registered, na.rm = TRUE), 2),
      " [",
      round(quantile(patients_registered, 0.25, na.rm = TRUE), 2),
      "-",
      round(quantile(patients_registered, 0.75, na.rm = TRUE), 2),
      "]"
    )
  )
##6.1.3. Població registrada i activitat d’atenció primària

library(ggplot2)
pop_mensual <- dataset %>%
  group_by(year, month) %>%
  summarise(
    mediana = median(patients_registered, na.rm = TRUE),
    p25 = quantile(patients_registered, 0.25, na.rm = TRUE),
    p75 = quantile(patients_registered, 0.75, na.rm = TRUE)
  )


ggplot(pop_mensual, aes(x = month, y = mediana, color = factor(year))) +
  
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  
  scale_color_manual(values = c("2022" = "#2E86AB", "2025" = "#2E7D32")) +
  
  scale_x_continuous(
    breaks = 1:12,
    labels = c("Gen", "Feb", "Mar", "Abr", "Mai", "Jun",
               "Jul", "Ago", "Set", "Oct", "Nov", "Des"),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0.05, 0.1))
  ) +
  
  labs(
    x = "Mes",
    y = "Població registrada",
    color = "Any",
    title = "Evolució mensual de la població registrada"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0),
    legend.position = "right",
    plot.margin = margin(35, 40, 35, 40),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15))
  )

###taula informacio
library(dplyr)

taula_annex_poblacio <- dataset %>%
  group_by(year, month) %>%
  summarise(
    mediana_poblacio = median(patients_registered, na.rm = TRUE),
    q1 = quantile(patients_registered, 0.25, na.rm = TRUE),
    q3 = quantile(patients_registered, 0.75, na.rm = TRUE),
    riq = IQR(patients_registered, na.rm = TRUE),
    n_icb = n(),
    .groups = "drop"
  ) %>%
  mutate(
    mes = factor(
      month,
      levels = 1:12,
      labels = c("Gener", "Febrer", "Març", "Abril", "Maig", "Juny",
                 "Juliol", "Agost", "Setembre", "Octubre", "Novembre", "Desembre")
    )
  ) %>%
  select(
    Any = year,
    Mes = mes,
    `Mediana població registrada` = mediana_poblacio,
    Q1 = q1,
    Q3 = q3,
    RIQ = riq,
    `Nombre d'ICB` = n_icb
  )
View(taula_annex_poblacio)



## Consultes totals mensuals per ICB a AP (mediana + RIQ)
dataset %>%
  filter(year %in% c(2022, 2025)) %>%
  group_by(year) %>%
  summarise(
    consultes_totals = paste0(
      round(median(gp_appointments_total, na.rm = TRUE), 0),
      " [",
      round(quantile(gp_appointments_total, 0.25, na.rm = TRUE), 0),
      "-",
      round(quantile(gp_appointments_total, 0.75, na.rm = TRUE), 0),
      "]"
    ),
    consultes_per_1000 = paste0(
      round(median(gp_appointments_per_1000, na.rm = TRUE), 2),
      " [",
      round(quantile(gp_appointments_per_1000, 0.25, na.rm = TRUE), 2),
      "-",
      round(quantile(gp_appointments_per_1000, 0.75, na.rm = TRUE), 2),
      "]"
    )
  )

gp_mensual <- dataset %>%
  filter(year %in% c(2022, 2025)) %>%
  group_by(year, month) %>%
  summarise(
    mediana = median(gp_appointments_per_1000, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(gp_mensual, aes(x = month, y = mediana, color = factor(year))) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("2022" = "#2E86AB", "2025" = "#2E7D32")) +
  scale_x_continuous(
    breaks = 1:12,
    labels = c("Gen", "Feb", "Mar", "Abr", "Mai", "Jun",
               "Jul", "Ago", "Set", "Oct", "Nov", "Des"),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.05, 0.1))
  ) +
  labs(
    x = "Mes",
    y = "Consultes d’AP per 1.000 persones registrades",
    color = "Any",
    title = "Evolució mensual de l’activitat d’AP"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(35, 40, 35, 40),
    legend.position = "right",
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15))
  )

library(dplyr)

taula_annex_activitat_ap <- dataset %>%
  group_by(year, month) %>%
  summarise(
    mediana_consultes_ap_per_1000 = median(gp_appointments_per_1000, na.rm = TRUE),
    q1 = quantile(gp_appointments_per_1000, 0.25, na.rm = TRUE),
    q3 = quantile(gp_appointments_per_1000, 0.75, na.rm = TRUE),
    riq = IQR(gp_appointments_per_1000, na.rm = TRUE),
    n_icb = n(),
    .groups = "drop"
  ) %>%
  mutate(
    mes = factor(
      month,
      levels = 1:12,
      labels = c("Gener", "Febrer", "Març", "Abril", "Maig", "Juny",
                 "Juliol", "Agost", "Setembre", "Octubre", "Novembre", "Desembre")
    )
  ) %>%
  select(
    Any = year,
    Mes = mes,
    `Mediana consultes AP per 1.000` = mediana_consultes_ap_per_1000,
    Q1 = q1,
    Q3 = q3,
    RIQ = riq,
    `Nombre d'ICB` = n_icb
  ) %>%
  arrange(Any, Mes)
View(taula_annex_activitat_ap)

#CONSULTES PER TIPUS 
mode_consulta_pct <- dataset %>%
  filter(year %in% c(2022, 2025)) %>%
  mutate(
    pct_face_to_face = (gp_appointments_face_to_face / gp_appointments_total) * 100,
    pct_home_visit = (gp_appointments_home_visit / gp_appointments_total) * 100,
    pct_telephone = (gp_appointments_telephone / gp_appointments_total) * 100,
    pct_video_online = (gp_appointments_video_online / gp_appointments_total) * 100,
    pct_unknown = gp_pct_unknown_mode,
    pct_remote = gp_pct_remote
  ) %>%
  group_by(year, month) %>%
  summarise(
    Presencial = median(pct_face_to_face, na.rm = TRUE),
    Home_visit = median(pct_home_visit, na.rm = TRUE),
    Telefonica = median(pct_telephone, na.rm = TRUE),
    Video_online = median(pct_video_online, na.rm = TRUE),
    Remota_total = median(pct_remote, na.rm = TRUE),
    Mode_desconegut = median(pct_unknown, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(Presencial, Home_visit, Telefonica,
             Video_online, Remota_total, Mode_desconegut),
    names_to = "tipus_consulta",
    values_to = "percentatge"
  )

mode_principal <- mode_consulta_pct %>%
  filter(tipus_consulta %in% c(
    "Presencial",
    "Telefonica",
    "Video_online",
    "Remota_total"
  ))

ggplot(mode_principal, aes(x = month, y = percentatge, color = factor(year))) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  facet_wrap(~ tipus_consulta, scales = "free_y", ncol = 2) +
  scale_color_manual(values = c("2022" = "#2E86AB", "2025" = "#2E7D32")) +
  scale_x_continuous(
    breaks = 1:12,
    labels = c("Gen", "Feb", "Mar", "Abr", "Mai", "Jun",
               "Jul", "Ago", "Set", "Oct", "Nov", "Des")
  ) +
  labs(
    x = "Mes",
    y = "% de consultes",
    color = "Any",
    title = "Evolució mensual dels principals modes de consulta"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    plot.margin = margin(35, 40, 35, 40),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15))
  )

#GRAFIC US URGENCIES
library(dplyr)
library(ggplot2)
###A&E
ae_mensual <- dataset %>%
  filter(year %in% c(2022, 2025)) %>%
  group_by(year, month) %>%
  summarise(
    mediana = median(ae_attendances_per_1000, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(ae_mensual, aes(x = month, y = mediana, color = factor(year))) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("2022" = "#2E86AB", "2025" = "#2E7D32")) +
  scale_x_continuous(
    breaks = 1:12,
    labels = c("Gen", "Feb", "Mar", "Abr", "Mai", "Jun",
               "Jul", "Ago", "Set", "Oct", "Nov", "Des"),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.05, 0.1))
  ) +
  labs(
    x = "Mes",
    y = "Assistències a A&E per 1.000 persones registrades",
    color = "Any",
    title = "Evolució mensual de la taxa d’assistències a A&E"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.position = "right",
    plot.margin = margin(35, 40, 35, 40),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15))
  )

emergencies_mensual <- dataset %>%
  filter(year %in% c(2022, 2025)) %>%
  group_by(year, month) %>%
  summarise(
    mediana = median(ae_emergencies_per_1000, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(emergencies_mensual, aes(x = month, y = mediana, color = factor(year))) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("2022" = "#2E86AB", "2025" = "#2E7D32")) +
  scale_x_continuous(
    breaks = 1:12,
    labels = c("Gen", "Feb", "Mar", "Abr", "Mai", "Jun",
               "Jul", "Ago", "Set", "Oct", "Nov", "Des"),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.05, 0.1))
  ) +
  labs(
    x = "Mes",
    y = "Emergències urgents per 1.000 persones registrades",
    color = "Any",
    title = "Evolució mensual de la taxa d'emergències"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.position = "right",
    plot.margin = margin(35, 40, 35, 40),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15))
  )



##A&E attendances per tipus
ae_type1_per_1000 = dataset$A.E_attendances_Type.1 / dataset$patients_registered * 1000
ae_type2_per_1000 = dataset$A.E_attendances_Type.2 / dataset$patients_registered * 1000
ae_other_per_1000 = dataset$A.E_attendances_Other_Department / dataset$patients_registered * 1000

ae_pct_type1 = dataset$A.E_attendances_Type.1 / dataset$sum_ae_attendances_total * 100
ae_pct_type2 = dataset$A.E_attendances_Type.2 / dataset$sum_ae_attendances_total * 100
ae_pct_other = dataset$A.E_attendances_Other_Department / dataset$sum_ae_attendances_total * 100

`ae_type1_per_1000` = resum_mediana_riq(ae_type1_per_1000, 2)
`ae_type2_per_1000` = resum_mediana_riq(ae_type2_per_1000, 2)
`ae_other_per_1000` = resum_mediana_riq(ae_other_per_1000, 2)

`ae_pct_type1` = resum_mediana_riq(ae_pct_type1, 2)
`ae_pct_type2` = resum_mediana_riq(ae_pct_type2, 2)
`ae_pct_other` = resum_mediana_riq(ae_pct_other, 2)


#2022VS2025
library(dplyr)

dataset <- dataset %>%
  mutate(
    gp_pct_face_to_face = (gp_appointments_face_to_face / gp_appointments_total) * 100,
    gp_pct_telephone = (gp_appointments_telephone / gp_appointments_total) * 100,
    gp_pct_video_online = (gp_appointments_video_online / gp_appointments_total) * 100
  )
taula_comparativa <- dataset %>%
  filter(year %in% c(2022, 2025)) %>%
  group_by(year) %>%
  summarise(
    `Població registrada` = paste0(
      round(median(patients_registered, na.rm = TRUE), 0),
      " [",
      round(quantile(patients_registered, 0.25, na.rm = TRUE), 0),
      "-",
      round(quantile(patients_registered, 0.75, na.rm = TRUE), 0),
      "]"
    ),
    
    `Consultes AP totals` = paste0(
      round(median(gp_appointments_total, na.rm = TRUE), 0),
      " [",
      round(quantile(gp_appointments_total, 0.25, na.rm = TRUE), 0),
      "-",
      round(quantile(gp_appointments_total, 0.75, na.rm = TRUE), 0),
      "]"
    ),
    
    `Consultes AP per 1.000` = paste0(
      round(median(gp_appointments_per_1000, na.rm = TRUE), 2),
      " [",
      round(quantile(gp_appointments_per_1000, 0.25, na.rm = TRUE), 2),
      "-",
      round(quantile(gp_appointments_per_1000, 0.75, na.rm = TRUE), 2),
      "]"
    ),
    
    `% presencials` = paste0(
      round(median(gp_pct_face_to_face, na.rm = TRUE), 2),
      " [",
      round(quantile(gp_pct_face_to_face, 0.25, na.rm = TRUE), 2),
      "-",
      round(quantile(gp_pct_face_to_face, 0.75, na.rm = TRUE), 2),
      "]"
    ),
    
    `% remotes` = paste0(
      round(median(gp_pct_remote, na.rm = TRUE), 2),
      " [",
      round(quantile(gp_pct_remote, 0.25, na.rm = TRUE), 2),
      "-",
      round(quantile(gp_pct_remote, 0.75, na.rm = TRUE), 2),
      "]"
    ),
    
    `% mode desconegut` = paste0(
      round(median(gp_pct_unknown_mode, na.rm = TRUE), 2),
      " [",
      round(quantile(gp_pct_unknown_mode, 0.25, na.rm = TRUE), 2),
      "-",
      round(quantile(gp_pct_unknown_mode, 0.75, na.rm = TRUE), 2),
      "]"
    ),
    
    `A&E totals` = paste0(
      round(median(sum_ae_attendances_total, na.rm = TRUE), 0),
      " [",
      round(quantile(sum_ae_attendances_total, 0.25, na.rm = TRUE), 0),
      "-",
      round(quantile(sum_ae_attendances_total, 0.75, na.rm = TRUE), 0),
      "]"
    ),
    
    `A&E per 1.000` = paste0(
      round(median(ae_attendances_per_1000, na.rm = TRUE), 2),
      " [",
      round(quantile(ae_attendances_per_1000, 0.25, na.rm = TRUE), 2),
      "-",
      round(quantile(ae_attendances_per_1000, 0.75, na.rm = TRUE), 2),
      "]"
    ),
    
    `A&E totals` = paste0(
      round(median(sum_ae_emergencies_total, na.rm = TRUE), 0),
      " [",
      round(quantile(sum_ae_emergencies_total, 0.25, na.rm = TRUE), 0),
      "-",
      round(quantile(sum_ae_emergencies_total, 0.75, na.rm = TRUE), 0),
      "]"
    ),
    
    `A&E per 1.000` = paste0(
      round(median(ae_emergencies_per_1000, na.rm = TRUE), 2),
      " [",
      round(quantile(ae_emergencies_per_1000, 0.25, na.rm = TRUE), 2),
      "-",
      round(quantile(ae_emergencies_per_1000, 0.75, na.rm = TRUE), 2),
      "]"
    ),
    .groups = "drop"
  )

taula_comparativa

comparativa_numeric <- dataset %>%
  filter(year %in% c(2022, 2025)) %>%
  group_by(year) %>%
  summarise(
    patients_registered = median(patients_registered, na.rm = TRUE),
    gp_appointments_total = median(gp_appointments_total, na.rm = TRUE),
    gp_appointments_per_1000 = median(gp_appointments_per_1000, na.rm = TRUE),
    gp_pct_face_to_face = median(gp_pct_face_to_face, na.rm = TRUE),
    gp_pct_remote = median(gp_pct_remote, na.rm = TRUE),
    gp_pct_unknown_mode = median(gp_pct_unknown_mode, na.rm = TRUE),
    sum_ae_attendances_total = median(sum_ae_attendances_total, na.rm = TRUE),
    ae_attendances_per_1000 = median(ae_attendances_per_1000, na.rm = TRUE),
    sum_ae_emergencies_total = median(sum_ae_emergencies_total, na.rm = TRUE),
    ae_emergenciess_per_1000 = median(ae_emergencies_per_1000, na.rm = TRUE),
    .groups = "drop"
  )

comparativa_diferencies <- comparativa_numeric %>%
  tidyr::pivot_longer(
    cols = -year,
    names_to = "variable",
    values_to = "mediana"
  ) %>%
  tidyr::pivot_wider(
    names_from = year,
    values_from = mediana
  ) %>%
  mutate(
    diferencia_2025_2022 = `2025` - `2022`,
    canvi_percentual = ((`2025` - `2022`) / `2022`) * 100
  )

comparativa_diferencies


#VARIABILITAT TERRITORIAL
library(dplyr)

variabilitat_territorial <- dataset %>%
  filter(year %in% c(2022, 2025)) %>%
  group_by(year) %>%
  summarise(
    gp_ap_mediana = median(gp_appointments_per_1000, na.rm = TRUE),
    gp_ap_q1 = quantile(gp_appointments_per_1000, 0.25, na.rm = TRUE),
    gp_ap_q3 = quantile(gp_appointments_per_1000, 0.75, na.rm = TRUE),
    gp_ap_min = min(gp_appointments_per_1000, na.rm = TRUE),
    gp_ap_max = max(gp_appointments_per_1000, na.rm = TRUE),
    
    remote_mediana = median(gp_pct_remote, na.rm = TRUE),
    remote_q1 = quantile(gp_pct_remote, 0.25, na.rm = TRUE),
    remote_q3 = quantile(gp_pct_remote, 0.75, na.rm = TRUE),
    remote_min = min(gp_pct_remote, na.rm = TRUE),
    remote_max = max(gp_pct_remote, na.rm = TRUE),
    
    ae_mediana = median(ae_attendances_per_1000, na.rm = TRUE),
    ae_q1 = quantile(ae_attendances_per_1000, 0.25, na.rm = TRUE),
    ae_q3 = quantile(ae_attendances_per_1000, 0.75, na.rm = TRUE),
    ae_min = min(ae_attendances_per_1000, na.rm = TRUE),
    ae_max = max(ae_attendances_per_1000, na.rm = TRUE),
    
    .groups = "drop"
  )

variabilitat_territorial

##boxplot variabilitat territorial mode de consulta i a&e
library(dplyr)
library(tidyr)
library(ggplot2)

variabilitat_figura <- dataset %>%
  filter(year %in% c(2022, 2025)) %>%
  select(year, gp_pct_remote, ae_attendances_per_1000) %>%
  pivot_longer(
    cols = c(gp_pct_remote, ae_attendances_per_1000),
    names_to = "variable",
    values_to = "valor"
  ) %>%
  mutate(
    variable = recode(
      variable,
      gp_pct_remote = "% de consultes remotes",
      ae_attendances_per_1000 = "A&E per 1.000 persones registrades"
    )
  )

ggplot(variabilitat_figura,
       aes(x = factor(year), y = valor, fill = factor(year))) +
  geom_boxplot(alpha = 0.65, outlier.shape = NA) +
  geom_jitter(width = 0.18, alpha = 0.25, size = 1) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = c("2022" = "#2E86AB", "2025" = "#2E7D32")) +
  labs(
    x = "Any",
    y = "",
    fill = "Any",
    title = "Variabilitat territorial del mode de consulta i de l’ús d’A&E"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  )

##variabilitat mensual
ggplot(dataset %>% filter(year %in% c(2022, 2025)),
       aes(x = factor(month), y = ae_attendances_per_1000, fill = factor(year))) +
  geom_boxplot(alpha = 0.65, outlier.shape = NA) +
  scale_fill_manual(values = c("2022" = "#2E86AB", "2025" = "#2E7D32")) +
  scale_x_discrete(
    labels = c("Gen", "Feb", "Mar", "Abr", "Mai", "Jun",
               "Jul", "Ago", "Set", "Oct", "Nov", "Des")
  ) +
  labs(
    x = "Mes",
    y = "Assistències a A&E per 1.000 persones registrades",
    fill = "Any",
    title = "Variabilitat mensual de la taxa d’assistències a A&E"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggplot(dataset %>% filter(year %in% c(2022, 2025)),
       aes(x = factor(month), y = gp_pct_remote, fill = factor(year))) +
  geom_boxplot(alpha = 0.65, outlier.shape = NA) +
  scale_fill_manual(values = c("2022" = "#2E86AB", "2025" = "#2E7D32")) +
  scale_x_discrete(
    labels = c("Gen", "Feb", "Mar", "Abr", "Mai", "Jun",
               "Jul", "Ago", "Set", "Oct", "Nov", "Des")
  ) +
  labs(
    x = "Mes",
    y = "% de consultes remotes",
    fill = "Any",
    title = "Variabilitat mensual del percentatge de consultes remotes"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )


##linies mes visuals
library(dplyr)
library(tidyr)
library(ggplot2)

variabilitat_mes <- dataset %>%
  filter(year %in% c(2022, 2025)) %>%
  group_by(year, month) %>%
  summarise(
    remote_mediana = median(gp_pct_remote, na.rm = TRUE),
    remote_q1 = quantile(gp_pct_remote, 0.25, na.rm = TRUE),
    remote_q3 = quantile(gp_pct_remote, 0.75, na.rm = TRUE),
    
    ae_mediana = median(ae_attendances_per_1000, na.rm = TRUE),
    ae_q1 = quantile(ae_attendances_per_1000, 0.25, na.rm = TRUE),
    ae_q3 = quantile(ae_attendances_per_1000, 0.75, na.rm = TRUE),
    .groups = "drop"
  )
variabilitat_plot <- bind_rows(
  variabilitat_mes %>%
    transmute(
      year,
      month,
      variable = "% de consultes remotes",
      mediana = remote_mediana,
      q1 = remote_q1,
      q3 = remote_q3
    ),
  variabilitat_mes %>%
    transmute(
      year,
      month,
      variable = "A&E per 1.000 persones registrades",
      mediana = ae_mediana,
      q1 = ae_q1,
      q3 = ae_q3
    )
)
ggplot(variabilitat_plot,
       aes(x = month, y = mediana, color = factor(year), fill = factor(year))) +
  
  geom_ribbon(aes(ymin = q1, ymax = q3), alpha = 0.15, color = NA) +
  geom_line(size = 1.2) +
  geom_point(size = 2.2) +
  
  facet_wrap(~ variable, scales = "free_y", ncol = 1) +
  
  scale_color_manual(values = c("2022" = "#2E86AB", "2025" = "#2E7D32")) +
  scale_fill_manual(values = c("2022" = "#2E86AB", "2025" = "#2E7D32")) +
  
  scale_x_continuous(
    breaks = 1:12,
    labels = c("Gen", "Feb", "Mar", "Abr", "Mai", "Jun",
               "Jul", "Ago", "Set", "Oct", "Nov", "Des"),
    expand = expansion(mult = c(0.03, 0.03))
  ) +
  
  labs(
    x = "Mes",
    y = "",
    color = "Any",
    fill = "Any",
    title = "Variabilitat temporal i territorial de les principals variables de l’estudi"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "right"
  )

#############################################################################################

library(dplyr)
resum_unknown_any <- dataset %>%
  group_by(year) %>%
  summarise(
    mediana = median(gp_pct_unknown_mode, na.rm = TRUE),
    q1 = quantile(gp_pct_unknown_mode, 0.25, na.rm = TRUE),
    q3 = quantile(gp_pct_unknown_mode, 0.75, na.rm = TRUE),
    mediana_riq = resum_mediana_riq(gp_pct_unknown_mode),
    .groups = "drop"
  )

resum_unknown_any


#############################################################################################
install.packages("patchwork")   
library(patchwork)
graf_poblacio + graf_ap

