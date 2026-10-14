library(ggplot2)
library(dplyr)

taula_attendances_plot <- taula_attendances_tipus %>%
  filter(model %in% c("Bivariat simple", "Multivariable sense interacció")) %>%
  mutate(
    outcome = factor(
      outcome,
      levels = c(
        "A&E attendances Type 1",
        "A&E attendances Type 2",
        "A&E attendances Other Department"
      )
    )
  )

ggplot(
  taula_attendances_plot,
  aes(x = beta_remote_2022, y = outcome, color = model)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  labs(
    title = "Associació entre consulta remota i assistències a A&E per tipus",
    x = "Coeficient β de consulta remota",
    y = "Tipus d'assistència",
    color = "Model"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom"
  )


taula_emergencies_plot <- taula_emergencies_tipus%>%
  filter(model %in% c("Bivariat simple", "Multivariable sense interacció")) %>%
  mutate(
    outcome = factor(
      outcome,
      levels = c(
        "Emergency admissions Type 1",
        "Emergency admissions Type 2",
        "Emergency admissions Other Department"
      )
    )
  )

ggplot(
  taula_emergencies_plot,
  aes(x = beta_remote_2022, y = outcome, color = model)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  labs(
    title = "Associació entre consulta remota i admissions a emergències per tipus",
    x = "Coeficient β de consulta remota",
    y = "Tipus d'admissió",
    color = "Model"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom"
  )














taula_att_interaccio_plot <- taula_attendances_tipus %>%
  filter(model %in% c("Bivariat amb interacció", "Multivariable amb interacció")) %>%
  select(outcome, model, beta_remote_2022, beta_remote_2025_estimat) %>%
  tidyr::pivot_longer(
    cols = c(beta_remote_2022, beta_remote_2025_estimat),
    names_to = "any",
    values_to = "beta"
  ) %>%
  mutate(
    any = recode(
      any,
      beta_remote_2022 = "2022",
      beta_remote_2025_estimat = "2025"
    )
  )

ggplot(
  taula_att_interaccio_plot,
  aes(x = any, y = beta, group = outcome, color = outcome)
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  facet_wrap(~ model) +
  labs(
    title = "Coeficient estimat de consulta remota segons any i tipus d'assistència",
    x = "Any",
    y = "Coeficient β estimat",
    color = "Tipus d'assistència"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom"
  )



taula_emerg_interaccio_plot <- taula_emergencies_tipus %>%
  filter(model %in% c("Bivariat amb interacció", "Multivariable amb interacció")) %>%
  select(outcome, model, beta_remote_2022, beta_remote_2025_estimat) %>%
  tidyr::pivot_longer(
    cols = c(beta_remote_2022, beta_remote_2025_estimat),
    names_to = "any",
    values_to = "beta"
  ) %>%
  mutate(
    any = recode(
      any,
      beta_remote_2022 = "2022",
      beta_remote_2025_estimat = "2025"
    )
  )

ggplot(
  taula_emerg_interaccio_plot,
  aes(x = any, y = beta, group = outcome, color = outcome)
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  facet_wrap(~ model) +
  labs(
    title = "Coeficient estimat de consulta remota segons any i tipus d'admissió",
    x = "Any",
    y = "Coeficient β estimat",
    color = "Tipus d'admissió"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom"
  )






library(ggeffects)
library(ggplot2)
library(dplyr)


pred_att_type1 <- ggpredict(
  att_type1_multi_interaccio,
  terms = c("gp_pct_remote", "year")
)

ggplot(pred_att_type1, aes(x = x, y = predicted, color = group)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high, fill = group),
    alpha = 0.15,
    color = NA
  ) +
  labs(
    title = "Prediccions ajustades: A&E attendances Type 1",
    x = "Consultes remotes (%)",
    y = "Assistències Type 1 per 1.000 persones registrades",
    color = "Any",
    fill = "Any"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom"
  )

pred_att_type2 <- ggpredict(
  att_type2_multi_interaccio,
  terms = c("gp_pct_remote", "year")
)

ggplot(pred_att_type2, aes(x = x, y = predicted, color = group)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high, fill = group),
    alpha = 0.15,
    color = NA
  ) +
  labs(
    title = "Prediccions ajustades: A&E attendances Type 2",
    x = "Consultes remotes (%)",
    y = "Assistències Type 2 per 1.000 persones registrades",
    color = "Any",
    fill = "Any"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom"
  )

pred_att_type3 <- ggpredict(
  att_type3_multi_interaccio,
  terms = c("gp_pct_remote", "year")
)

ggplot(pred_att_type3, aes(x = x, y = predicted, color = group)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high, fill = group),
    alpha = 0.15,
    color = NA
  ) +
  labs(
    title = "Prediccions ajustades: A&E attendances Other Department",
    x = "Consultes remotes (%)",
    y = "Assistències Other Department per 1.000 persones registrades",
    color = "Any",
    fill = "Any"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom"
  )
















pred_att_type1 <- ggpredict(att_type1_multi_interaccio, terms = c("gp_pct_remote", "year")) %>%
  as.data.frame() %>%
  mutate(outcome = "A&E attendances Type 1")

pred_att_type2 <- ggpredict(att_type2_multi_interaccio, terms = c("gp_pct_remote", "year")) %>%
  as.data.frame() %>%
  mutate(outcome = "A&E attendances Type 2")

pred_att_type3 <- ggpredict(att_type3_multi_interaccio, terms = c("gp_pct_remote", "year")) %>%
  as.data.frame() %>%
  mutate(outcome = "A&E attendances Other Department")

pred_att_all <- bind_rows(
  pred_att_type1,
  pred_att_type2,
  pred_att_type3
)

ggplot(pred_att_all, aes(x = x, y = predicted, color = group)) +
  geom_line(linewidth = 1.1) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high, fill = group),
    alpha = 0.15,
    color = NA
  ) +
  facet_wrap(~ outcome, scales = "free_y") +
  labs(
    title = "Prediccions ajustades de les assistències a A&E per tipus",
    x = "Consultes remotes (%)",
    y = "Taxa estimada per 1.000 persones registrades",
    color = "Any",
    fill = "Any"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )



pred_emerg_type1 <- ggpredict(emerg_type1_multi_interaccio, terms = c("gp_pct_remote", "year")) %>%
  as.data.frame() %>%
  mutate(outcome = "Emergency admissions Type 1")

pred_emerg_type2 <- ggpredict(emerg_type2_multi_interaccio, terms = c("gp_pct_remote", "year")) %>%
  as.data.frame() %>%
  mutate(outcome = "Emergency admissions Type 2")

pred_emerg_type3 <- ggpredict(emerg_type3_multi_interaccio, terms = c("gp_pct_remote", "year")) %>%
  as.data.frame() %>%
  mutate(outcome = "Emergency admissions Other Department")

pred_emerg_all <- bind_rows(
  pred_emerg_type1,
  pred_emerg_type2,
  pred_emerg_type3
)

ggplot(pred_emerg_all, aes(x = x, y = predicted, color = group)) +
  geom_line(linewidth = 1.1) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high, fill = group),
    alpha = 0.15,
    color = NA
  ) +
  facet_wrap(~ outcome, scales = "free_y") +
  labs(
    title = "Prediccions ajustades de les admissions a emergències per tipus",
    x = "Consultes remotes (%)",
    y = "Taxa estimada per 1.000 persones registrades",
    color = "Any",
    fill = "Any"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )






