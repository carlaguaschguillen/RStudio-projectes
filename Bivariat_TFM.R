#MODELITZACIO ESTADISTICA
##MODEL BIVARIAT PRINCIPAL
model1 <- lm(dataset$ae_attendances_per_1000 ~ dataset$gp_pct_remote + dataset$year + dataset$month, data = dataset)
summary(model1)


model2 <- lm(dataset$ae_attendances_per_1000 ~ dataset$gp_pct_remote + dataset$gp_appointments_per_1000 + dataset$year + dataset$month, data = dataset)
summary(model2)


model3 <- lm(dataset$ae_attendances_per_1000 ~ dataset$gp_pct_remote + dataset$gp_appointments_per_1000 + dataset$year + factor(dataset$month) + factor(dataset$icb_code), data = dataset)
summary(model3)


#Mirar la relació entre consultes remotes i A&E
library(ggplot2)

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
  facet_wrap(~ servei, scales = "free_y") +
  scale_color_manual(values = c("2022" = "#2E86AB", "2025" = "#2E7D32")) +
  labs(
    title = "Distribució percentual de les assistències a A&E i admissions urgents segons tipus de servei",
    x = "Tipus de servei",
    y = "Percentatge mensual per ICB, mediana [RIQ]",
    color = "Any"
  ) +
  theme_minimal(base_size = 13)

##ASSOCIACIO SEPARAT PER ANYS
ggplot(dataset, aes(x = gp_pct_remote, y = ae_attendances_per_1000)) +
  geom_point(aes(color = factor(year)), alpha = 0.40, size = 1.8) +
  geom_smooth(
    aes(color = factor(year), fill = factor(year)),
    method = "lm",
    se = TRUE,
    linewidth = 1.2,
    alpha = 0.18
  ) +
  facet_wrap(~ year, nrow = 1) +
  scale_color_manual(values = c("2022" = "#2E86AB", "2025" = "#2E7D32")) +
  scale_fill_manual(values = c("2022" = "#2E86AB", "2025" = "#2E7D32")) +
  coord_cartesian(
    xlim = c(15, 48),
    ylim = c(15, 67)
  ) +
  labs(
    x = "Proporció de consultes remotes en atenció primària (%)",
    y = "Assistències a A&E per 1.000 persones registrades",
    title = "Associació bivariada entre consulta remota i ús d’A&E segons any"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    panel.spacing = unit(1.2, "lines")
  )

###estadistica
cor.test(dataset$gp_pct_remote, dataset$ae_attendances_per_1000, method = "spearman",  use = "complete.obs")


##correlacions per any
library(dplyr)

dataset %>%
  group_by(year) %>%
  summarise(
    correlacio_spearman = cor(gp_pct_remote, ae_attendances_per_1000, method = "spearman", use = "complete.obs"),
    n = n()
  )
library(dplyr)
install.packages("broom")
library(broom)

cor_any <- dataset %>%
  group_by(year) %>%
  summarise(
    test = list(cor.test(gp_pct_remote, ae_attendances_per_1000, method = "spearman"))
  ) %>%
  mutate(resultats = lapply(test, tidy)) %>%
  tidyr::unnest(resultats)

cor_any


#Regressió lineal simple
model_bivariat <- lm(dataset$ae_attendances_per_1000 ~ dataset$gp_pct_remote, data = dataset)

summary(model_bivariat)

###bivariat per any
model_interaccio <- lm(
  ae_attendances_per_1000 ~ gp_pct_remote * factor(year),
  data = dataset
)

summary(model_interaccio)

library(broom)

tidy(model_bivariat, conf.int = TRUE)

resultats_bivariat <- tidy(model_bivariat, conf.int = TRUE)

resultats_bivariat

#taula resum dels models
library(broom)
library(dplyr)
library(writexl)

taula_model_attendances_bivariat <- tidy(model_bivariat) %>%
  mutate(
    Model = "Model bivariant simple"
  )

taula_model_attendances_interaccio <- tidy(model_interaccio) %>%
  mutate(
    Model = "Model amb interacció consulta remota × any"
  )

taula_models_attendances <- bind_rows(
  taula_model_attendances_bivariat,
  taula_model_attendances_interaccio
) %>%
  select(Model, term, estimate, std.error, statistic, p.value) %>%
  mutate(
    estimate = round(estimate, 3),
    std.error = round(std.error, 3),
    statistic = round(statistic, 3),
    p.value = round(p.value, 4)
  )

View(taula_models_attendances)


################################################################################cucu
#BIVARIAT PER EMERGENCY

library(ggplot2)

ggplot(dataset, aes(x = gp_pct_remote, 
                    y = ae_emergencies_per_1000,
                    color = factor(year),
                    fill = factor(year))) +
  geom_point(alpha = 0.4, size = 2) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.18) +
  facet_wrap(~ year, nrow = 1) +
  scale_color_manual(
    values = c("2022" = "#2E86AB", "2025" = "#2E7D32")
  ) +
  scale_fill_manual(
    values = c("2022" = "#2E86AB", "2025" = "#2E7D32")
  ) +
  labs(
    x = "Consultes remotes en atenció primària (%)",
    y = "Admissions urgents per 1.000 persones registrades",
    color = "Any",
    fill = "Any",
    title = "Relació entre consultes remotes en AP i admissions urgents segons any"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 12)
  )

#spearman
cor.test(
  dataset$gp_pct_remote,
  dataset$ae_emergencies_per_1000,
  method = "spearman",
  use = "complete.obs"
)

library(dplyr)
library(purrr)
library(tibble)

taula_spearman_emergencies <- dataset %>%
  group_by(year) %>%
  summarise(
    rho = cor(gp_pct_remote, ae_emergencies_per_1000,
              method = "spearman",
              use = "complete.obs"),
    p_value = cor.test(gp_pct_remote,
                       ae_emergencies_per_1000,
                       method = "spearman",
                       use = "complete.obs")$p.value,
    .groups = "drop"
  )

View(taula_spearman_emergencies)

#regressio lineal bivariada simple
model_emerg_bivariat <- lm(
  ae_emergencies_per_1000 ~ gp_pct_remote,
  data = dataset
)

summary(model_emerg_bivariat)

#regressio lineal bivariada interaccio remot-any
dataset <- dataset %>%
  mutate(
    year = factor(year)
  )

model_emerg_interaccio <- lm(
  ae_emergencies_per_1000 ~ gp_pct_remote * year,
  data = dataset
)

summary(model_emerg_interaccio)

#taula resum dels models
library(broom)
library(dplyr)
library(writexl)

taula_model_emerg_bivariat <- tidy(model_emerg_bivariat) %>%
  mutate(
    Model = "Model bivariant simple"
  )

taula_model_emerg_interaccio <- tidy(model_emerg_interaccio) %>%
  mutate(
    Model = "Model amb interacció consulta remota × any"
  )

taula_models_emergency <- bind_rows(
  taula_model_emerg_bivariat,
  taula_model_emerg_interaccio
) %>%
  select(Model, term, estimate, std.error, statistic, p.value) %>%
  mutate(
    estimate = round(estimate, 3),
    std.error = round(std.error, 3),
    statistic = round(statistic, 3),
    p.value = round(p.value, 4)
  )

View(taula_models_emergency)

#afegeixc R2
taula_r2_emergency <- tibble(
  Model = c("Model bivariant simple",
            "Model amb interacció consulta remota × any"),
  R2 = c(
    summary(model_emerg_bivariat)$r.squared,
    summary(model_emerg_interaccio)$r.squared
  ),
  R2_ajustat = c(
    summary(model_emerg_bivariat)$adj.r.squared,
    summary(model_emerg_interaccio)$adj.r.squared
  )
) %>%
  mutate(
    R2 = round(R2, 3),
    R2_ajustat = round(R2_ajustat, 3)
  )

View(taula_r2_emergency)

##PROPOSTA 2 XAT
library(ggplot2)
#grafic 1
ggplot(dataset, aes(x = gp_pct_remote, 
                    y = ae_attendances_per_1000, 
                    color = factor(year))) +
  geom_point(alpha = 0.4, size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual(values = c("2022" = "#2E86AB", "2025" = "#2E7D32")) +
  labs(
    x = "Consultes remotes en atenció primària (%)",
    y = "Assistències a A&E per 1.000 persones registrades",
    color = "Any",
    title = "Relació entre consultes remotes en AP i assistències a A&E"
  ) +
  theme_minimal()

#grafic 2
ggplot(dataset, aes(x = gp_pct_remote, 
                    y = ae_emergencies_per_1000, 
                    color = factor(year))) +
  geom_point(alpha = 0.4, size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual(values = c("2022" = "#2E86AB", "2025" = "#2E7D32")) +
  labs(
    x = "Consultes remotes en atenció primària (%)",
    y = "Admissions urgents per 1.000 persones registrades",
    color = "Any",
    title = "Relació entre consultes remotes en AP i admissions urgents"
  ) +
  theme_minimal()

#separar per any
library(ggplot2)

ggplot(dataset, aes(x = gp_pct_remote, 
                    y = ae_emergencies_per_1000,
                    color = factor(year),
                    fill = factor(year))) +
  geom_point(alpha = 0.4, size = 2) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.18) +
  facet_wrap(~ year, nrow = 1) +
  scale_color_manual(
    values = c("2022" = "#2E86AB", "2025" = "#2E7D32")
  ) +
  scale_fill_manual(
    values = c("2022" = "#2E86AB", "2025" = "#2E7D32")
  ) +
  labs(
    x = "Consultes remotes en atenció primària (%)",
    y = "Admissions urgents per 1.000 persones registrades",
    color = "Any",
    fill = "Any",
    title = "Relació entre consultes remotes en AP i admissions urgents segons any"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 12)
  )
#COMPARACIO PER GRUPS DE CONSULTA REMOTA
dataset <- dataset %>%
  mutate(
    grup_remote = ntile(gp_pct_remote, 3),
    grup_remote = case_when(
      grup_remote == 1 ~ "Baix % remot",
      grup_remote == 2 ~ "Mitjà % remot",
      grup_remote == 3 ~ "Alt % remot"
    )
  )
taula_bivariat_remote <- dataset %>%
  group_by(grup_remote) %>%
  summarise(
    n = n(),
    `Consultes remotes (%)` = resum_mediana_riq(gp_pct_remote, 2),
    `A&E per 1.000` = resum_mediana_riq(ae_attendances_per_1000, 2),
    `Admissions urgents per 1.000` = resum_mediana_riq(ae_emergencies_per_1000, 2),
    `Activitat AP per 1.000` = resum_mediana_riq(gp_appointments_per_1000, 2),
    .groups = "drop"
  )

View(taula_bivariat_remote)

kruskal.test(ae_attendances_per_1000 ~ grup_remote, data = dataset)
kruskal.test(ae_emergencies_per_1000 ~ grup_remote, data = dataset)









##resum modelització estadística
library(dplyr)
library(tibble)
library(writexl)

dataset <- dataset %>%
  mutate(
    year = factor(year),
    month = factor(month),
    icb_code = factor(icb_code)
  )

# 1. Model bivariant simple
model_emerg_bivariat <- lm(
  ae_emergencies_per_1000 ~ gp_pct_remote,
  data = dataset
)

# 2. Model bivariant amb interacció
model_emerg_interaccio <- lm(
  ae_emergencies_per_1000 ~ gp_pct_remote * year,
  data = dataset
)

# 3. Model multivariable sense interacció
model_emerg_multi <- lm(
  ae_emergencies_per_1000 ~ gp_pct_remote + gp_appointments_per_1000 + year + month + icb_code,
  data = dataset
)

# 4. Model multivariable amb interacció
model_emerg_multi_interaccio <- lm(
  ae_emergencies_per_1000 ~ gp_pct_remote * year + gp_appointments_per_1000 + month + icb_code,
  data = dataset
)

