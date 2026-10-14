#MULTIVARIAT ATTENDANCES
##Sense interacció
model_multivariat <- lm(
  ae_attendances_per_1000 ~ gp_pct_remote + 
    gp_appointments_per_1000 +
    factor(year) +
    factor(month) +
    factor(icb_code),
  data = dataset
)

summary(model_multivariat)

##Amb interacció
model_multivariat_interaccio <- lm(
  ae_attendances_per_1000 ~ gp_pct_remote * factor(year) + 
    gp_appointments_per_1000 +
    factor(month) +
    factor(icb_code),
  data = dataset
)

summary(model_multivariat_interaccio)


anova(model_multivariat, model_multivariat_interaccio)


#prediccions del model
install.packages("ggeffects")
library(ggeffects)
library(ggplot2)
library(dplyr)

dataset <- dataset %>%
  mutate(
    year_f = factor(year),
    month_f = factor(month),
    icb_f = factor(icb_code))

model_multivariat_interaccio <- lm(
  ae_attendances_per_1000 ~ gp_pct_remote * year_f +
    gp_appointments_per_1000 +
    month_f +
    icb_f,
  data = dataset
)

summary(model_multivariat_interaccio)

library(ggeffects)

pred <- ggpredict(
  model_multivariat_interaccio,
  terms = c("gp_pct_remote", "year_f")
)

pred

library(ggplot2)

ggplot(pred, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high),
    alpha = 0.15,
    color = NA
  ) +
  scale_color_manual(values = c("2022" = "#2E86AB", "2025" = "#2E7D32")) +
  scale_fill_manual(values = c("2022" = "#2E86AB", "2025" = "#2E7D32")) +
  labs(
    x = "Consultes remotes en atenció primària (%)",
    y = "Taxa ajustada d'assistències a A&E per 1.000 persones registrades",
    color = "Any",
    fill = "Any",
    title = "Associació ajustada entre consulta remota i ús d’A&E segons any"
  ) +
  theme_minimal(base_size = 12) 






##########################CANVI
#MULTIVARIAT EMERGENCIES
##Sense interacció
dataset <- dataset %>%
  mutate(
    year = factor(year),
    month = factor(month),
    icb_code = factor(icb_code)
  )

model_emerg_multi <- lm(
  ae_emergencies_per_1000 ~ gp_pct_remote + gp_appointments_per_1000 + year + month + icb_code,
  data = dataset
)

summary(model_emerg_multi)

##Amb interacció
model_emerg_multi_interaccio <- lm(
  ae_emergencies_per_1000 ~ gp_pct_remote * year + gp_appointments_per_1000 + month + icb_code,
  data = dataset
)

summary(model_emerg_multi_interaccio)

anova(model_emerg_multi, model_emerg_multi_interaccio)


#prediccions
library(ggeffects)

pred_emerg <- ggpredict(
  model_emerg_multi_interaccio,
  terms = c("gp_pct_remote", "year")
)

pred_emerg

library(ggplot2)

ggplot(pred_emerg, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high),
    alpha = 0.15,
    color = NA
  ) +
  scale_color_manual(values = c("2022" = "#2E86AB", "2025" = "#2E7D32")) +
  scale_fill_manual(values = c("2022" = "#2E86AB", "2025" = "#2E7D32")) +
  labs(
    x = "Consultes remotes en atenció primària (%)",
    y = "Taxa ajustada d'emergències a A&E per 1.000 persones registrades",
    color = "Any",
    fill = "Any",
    title = "Associació ajustada entre consulta remota i emergències a A&E segons any"
  ) +
  theme_minimal(base_size = 12)
