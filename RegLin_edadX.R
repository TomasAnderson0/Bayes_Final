library(rstan)
library(readr)
library(dplyr)
library(ggplot2)

cerebros <- read_csv("cerebros.csv")
cerebros$diag = factor(cerebros$diag, levels = c("HC", "MCI", "AD"))
cerebros = cerebros %>% mutate(diag = ifelse(diag == "HC", "HC", "MCI&AD"))
cerebros <- cerebros %>% mutate(res1 = ifelse(resonador_fab == "GE", 1, 0)) %>% 
  mutate(res2 = ifelse(resonador_fab == "Philips", 1, 0))

lista <- list(N = nrow(cerebros),
              edad = cerebros$edad,
              ver = cerebros$lh_subcx_hippocampus_volume/1000
)

reglin_edadxVHI <- stan(
  file = "RegLin_edadXvar.stan",
  data = lista,
  chains = 4,
  warmup = 150,
  iter = 1500,
  control = list(adapt_delta = 0.99),
  seed = 1997
)

reglin_edadxVHI

posteriorVHI <- extract(reglin_edadxVHI, c("beta0", "beta1", "sigma"))

# ------------- grafico

x_grid <- seq(20, 90, length.out = 100)
mu_matrix <- matrix(nrow = 5400, ncol = 100)

for (i in seq_along(x_grid)) {
  mu_matrix[, i] <- posteriorVHI$beta0 + posteriorVHI$beta1 * x_grid[i]
}

mu_mean <- apply(mu_matrix, 2, mean)
mu_qts <- t(apply(mu_matrix, 2, function(x) quantile(x, c(0.025, 0.975))))
mu_qts2 <- t(apply(mu_matrix, 2, function(x) quantile(x, c(0.25, 0.75))))

data_mu <- data.frame(
  x = x_grid, 
  y = mu_mean,
  lower_95 = mu_qts[, 1],
  upper_95 = mu_qts[, 2],
  lower_50 = mu_qts2[, 1],
  upper_50 = mu_qts2[, 2]
)

edadxvhi = ggplot(cerebros) +
  geom_ribbon(
    aes(x, ymin = lower_95, ymax = upper_95),
    fill = "grey50",
    alpha = 0.8,
    data = data_mu
  ) +
  geom_ribbon(
    aes(x, ymin = lower_50, ymax = upper_50),
    fill = "grey35",
    alpha = 0.6,
    data = data_mu
  ) +
  geom_point(aes(x = edad, y = lh_subcx_hippocampus_volume/1000, color = diag),
             alpha = 0.6, size = 2) +
  scale_colour_manual(values = c("#1A9C5A", "#DE4627")) +
  geom_line(
    aes(x, y), 
    color = "firebrick",
    data = data_mu
  ) +
  labs(x = "Edad", y = expression(paste("VHI⠀", (cm^{3}))),
       color = "Diagnóstico") + theme_minimal()

edadxvhi2 = ggplot(cerebros) +
  geom_ribbon(
    aes(x, ymin = lower_95, ymax = upper_95),
    fill = "grey50",
    alpha = 0.8,
    data = data_mu
  ) +
  geom_ribbon(
    aes(x, ymin = lower_50, ymax = upper_50),
    fill = "grey35",
    alpha = 0.6,
    data = data_mu
  ) +
  geom_point(aes(x = edad, y = lh_subcx_hippocampus_volume/1000), color = "#232323", size = 2) +
  scale_colour_manual(values = c("#1A9C5A", "#DE4627")) +
  geom_line(
    aes(x, y), 
    color = "firebrick",
    data = data_mu
  ) +
  labs(x = "Edad", y = expression(paste("VHI⠀", (cm^{3}))),
       color = "Diagnóstico") + theme_minimal()


# ------------------------------ VI ---------------------------------------
lista <- list(N = nrow(cerebros),
              edad = cerebros$edad,
              ver = cerebros$xh_general_etiv_volume/100000
)

reglin_edadxVI <- stan(
  file = "RegLin_edadXvar.stan",
  data = lista,
  chains = 4,
  warmup = 150,
  iter = 1500,
  control = list(adapt_delta = 0.99),
  seed = 1997
)

posteriorVI <- extract(reglin_edadxVI, c("beta0", "beta1", "sigma"))

# ------------- grafico

x_grid <- seq(20, 90, length.out = 100)
mu_matrix <- matrix(nrow = 5400, ncol = 100)

for (i in seq_along(x_grid)) {
  mu_matrix[, i] <- posteriorVI$beta0 + posteriorVI$beta1 * x_grid[i]
}

mu_mean <- apply(mu_matrix, 2, mean)
mu_qts <- t(apply(mu_matrix, 2, function(x) quantile(x, c(0.025, 0.975))))
mu_qts2 <- t(apply(mu_matrix, 2, function(x) quantile(x, c(0.25, 0.75))))


data_mu <- data.frame(
  x = x_grid, 
  y = mu_mean,
  lower_95 = mu_qts[, 1],
  upper_95 = mu_qts[, 2],
  lower_50 = mu_qts2[, 1],
  upper_50 = mu_qts2[, 2]
)

edadxvi = ggplot(cerebros) +
  geom_ribbon(
    aes(x, ymin = lower_95, ymax = upper_95),
    fill = "grey50",
    alpha = 0.6,
    data = data_mu
  ) +
  geom_ribbon(
    aes(x, ymin = lower_50, ymax = upper_50),
    fill = "grey35",
    alpha = 0.6,
    data = data_mu
  ) +
  geom_point(aes(x = edad, y = xh_general_etiv_volume/100000, color = diag),
alpha = 0.6, size = 2) +
  scale_colour_manual(values = c("#1A9C5A", "#DE4627")) +
  geom_line(
    aes(x, y), 
    color = "firebrick",
    data = data_mu
  ) +
  labs(x = "Edad", y = expression(paste("VI (100 ",cm^{3}, ")")),
       color = "Diagnóstico") + theme_minimal()


edadxvi2 = ggplot(cerebros) +
  geom_ribbon(
    aes(x, ymin = lower_95, ymax = upper_95),
    fill = "grey50",
    alpha = 0.6,
    data = data_mu
  ) +
  geom_ribbon(
    aes(x, ymin = lower_50, ymax = upper_50),
    fill = "grey35",
    alpha = 0.6,
    data = data_mu
  ) +
  geom_point(aes(x = edad, y = xh_general_etiv_volume/100000), color = "#232323", size = 2) +
  scale_colour_manual(values = c("#1A9C5A", "#DE4627")) +
  geom_line(
    aes(x, y), 
    color = "firebrick",
    data = data_mu
  ) +
  labs(x = "Edad", y = expression(paste("VI (100 ",cm^{3}, ")")),
       color = "Diagnóstico") + theme_minimal()


# ------------------------------- ECSF ----------------------------

lista <- list(N = nrow(cerebros),
              edad = cerebros$edad,
              ver = cerebros$lh_cortex_superiorfrontal_thickness
)

reglin_edadxECSF <- stan(
  file = "RegLin_edadXvar.stan",
  data = lista,
  chains = 4,
  warmup = 150,
  iter = 1500,
  control = list(adapt_delta = 0.99),
  seed = 1997
)

posteriorECSF <- extract(reglin_edadxECSF, c("beta0", "beta1", "sigma"))

# ------------- grafico

x_grid <- seq(20, 90, length.out = 100)
mu_matrix <- matrix(nrow = 5400, ncol = 100)

for (i in seq_along(x_grid)) {
  mu_matrix[, i] <- posteriorECSF$beta0 + posteriorECSF$beta1 * x_grid[i]
}

mu_mean <- apply(mu_matrix, 2, mean)
mu_qts <- t(apply(mu_matrix, 2, function(x) quantile(x, c(0.025, 0.975))))
mu_qts2 <- t(apply(mu_matrix, 2, function(x) quantile(x, c(0.25, 0.75))))

# Finalmente, se lamacenan los valores calculados en un data frame
data_mu <- data.frame(
  x = x_grid, 
  y = mu_mean,
  lower_95 = mu_qts[, 1],
  upper_95 = mu_qts[, 2],
  lower_50 = mu_qts2[, 1],
  upper_50 = mu_qts2[, 2]
)

edadxecsf = ggplot(cerebros) +
  geom_ribbon(
    aes(x, ymin = lower_95, ymax = upper_95),
    fill = "grey50",
    alpha = 0.6,
    data = data_mu
  ) +
  geom_ribbon(
    aes(x, ymin = lower_50, ymax = upper_50),
    fill = "grey35",
    alpha = 0.6,
    data = data_mu
  ) +
  geom_point(aes(x = edad, y = lh_cortex_superiorfrontal_thickness, color = diag),
             alpha = 0.6, size = 2) +
  scale_colour_manual(values = c("#1A9C5A", "#DE4627")) +
  geom_line(
    aes(x, y), 
    color = "firebrick",
    data = data_mu
  ) +
  labs(x = "Edad", y = expression(paste("ECSF⠀", (mm))),
       color = "Diagnóstico") + theme_minimal()

edadxecsf2 = ggplot(cerebros) +
  geom_ribbon(
    aes(x, ymin = lower_95, ymax = upper_95),
    fill = "grey50",
    alpha = 0.6,
    data = data_mu
  ) +
  geom_ribbon(
    aes(x, ymin = lower_50, ymax = upper_50),
    fill = "grey35",
    alpha = 0.6,
    data = data_mu
  ) +
  geom_point(aes(x = edad, y = lh_cortex_superiorfrontal_thickness ), color = "#232323", size = 2) +
  scale_colour_manual(values = c("#1A9C5A", "#DE4627")) +
  geom_line(
    aes(x, y), 
    color = "firebrick",
    data = data_mu
  ) +
  labs(x = "Edad", y = expression(paste("ECSF (mm)")),
       color = "Diagnóstico") + theme_minimal()



# --------------------------- VCF -------------------------------

lista <- list(N = nrow(cerebros),
              edad = cerebros$edad,
              ver = cerebros$lh_cortex_fusiform_volume/1000
)

reglin_edadxVCF <- stan(
  file = "RegLin_edadXvar.stan",
  data = lista,
  chains = 4,
  warmup = 150,
  iter = 1500,
  control = list(adapt_delta = 0.99),
  seed = 1997
)

posteriorVCF <- extract(reglin_edadxVCF, c("beta0", "beta1", "sigma"))

# ------------- grafico

x_grid <- seq(20, 90, length.out = 100)
mu_matrix <- matrix(nrow = 5400, ncol = 100)

for (i in seq_along(x_grid)) {
  mu_matrix[, i] <- posteriorVCF$beta0 + posteriorVCF$beta1 * x_grid[i]
}

mu_mean <- apply(mu_matrix, 2, mean)
mu_qts <- t(apply(mu_matrix, 2, function(x) quantile(x, c(0.025, 0.975))))
mu_qts2 <- t(apply(mu_matrix, 2, function(x) quantile(x, c(0.25, 0.75))))

data_mu <- data.frame(
  x = x_grid, 
  y = mu_mean,
  lower_95 = mu_qts[, 1],
  upper_95 = mu_qts[, 2],
  lower_50 = mu_qts2[, 1],
  upper_50 = mu_qts2[, 2]
)

edadxvcf = ggplot(cerebros) +
  geom_ribbon(
    aes(x, ymin = lower_95, ymax = upper_95),
    fill = "grey50",
    alpha = 0.6,
    data = data_mu
  ) +
  geom_ribbon(
    aes(x, ymin = lower_50, ymax = upper_50),
    fill = "grey35",
    alpha = 0.6,
    data = data_mu
  ) +
  geom_point(aes(x = edad, y = lh_cortex_fusiform_volume/1000, color = diag),
             alpha = 0.6, size = 2) +
  scale_colour_manual(values = c("#1A9C5A", "#DE4627")) +
  geom_line(
    aes(x, y), 
    color = "firebrick",
    data = data_mu
  ) +
  labs(x = "Edad", y = expression(paste("VCF⠀", (cm^{3}))),
       color = "Diagnóstico") + theme_minimal()

edadxvcf2 = ggplot(cerebros) +
  geom_ribbon(
    aes(x, ymin = lower_95, ymax = upper_95),
    fill = "grey50",
    alpha = 0.6,
    data = data_mu
  ) +
  geom_ribbon(
    aes(x, ymin = lower_50, ymax = upper_50),
    fill = "grey35",
    alpha = 0.6,
    data = data_mu
  ) +
  geom_point(aes(x = edad, y = lh_cortex_fusiform_volume/1000), color = "#232323",
             size = 2) +
  scale_colour_manual(values = c("#1A9C5A", "#DE4627")) +
  geom_line(
    aes(x, y), 
    color = "firebrick",
    data = data_mu
  ) +
  labs(x = "Edad", y = expression(paste("VCF⠀", (cm^{3}))),
       color = "Diagnóstico") + theme_minimal()



save(edadxvhi, edadxvi, edadxecsf , edadxvcf, edadxvhi2, edadxvi2, edadxecsf2, edadxvcf2, file = "Graficos_edad_morf.RData")
