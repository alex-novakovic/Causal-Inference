library(MatchIt)
library(ggplot2)
library(dplyr)
library(xtable)


# 1) ISTI SKUP: D NASUMICAN
set.seed(20)
n <- 200
X1 <- rnorm(n); X2 <- rnorm(n)
D <- rbinom(n,1,0.5)
Y <- 2*D + X1 + rnorm(n)

data <- data.frame(Y, D, X1, X2)

# Naivna razlika u sredinama
ate_diff <- mean(data$Y[data$D==1]) - mean(data$Y[data$D==0])

# Regresija (kontrola za X)
mod_lm <- lm(Y ~ D + X1 + X2, data=data)
ate_reg <- coef(mod_lm)["D"]

# Uparivanje (verovatnoca dodele)

m.out <- matchit(D ~ X1 + X2, method="nearest", data=data)
matched_data <- match.data(m.out, distance = 'ps', data = data, drop.unmatched = FALSE)
matched_data$Grupa <- ifelse(matched_data$D==1, "Tretirana", "Kontrolna")



# ilustracija uparivanja, dodela tretmana je nasumicna, ne mora svaka tacka biti uparena
# zato crtamo ovako 
matched_data_matched <- matched_data %>% 
  filter(weights > 0)

ate_match <- with(matched_data_matched, mean(Y[D==1]) - mean(Y[D==0]))

ggplot() +
  # 1️⃣ Linije samo za uparene
  geom_line(
    data = matched_data_matched,
    aes(x = ps, y = Y, group = subclass),
    color = "gray70", alpha = 0.5
  ) +
  # 2️⃣ Tačke za sve jedinice (uparene i neuparene)
  geom_point(
    data = matched_data,
    aes(x = ps, y = Y, color = Grupa),
    size = 2, alpha = 0.8
  ) +
  labs(
    title = "Uparivanje tretiranih i kontrolnih jedinica po verovatnoćama dodele tretmana",
    x = "Verovatnoća dodele tretmana",
    y = "Ishod (Y)"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Tretirana" = "blue", "Kontrolna" = "red"))


# 2) ISTI SKUP: D ZAVISI OD X -> KONFUNDOVANJE
set.seed(22)
n <- 200
X1 <- rnorm(n); X2 <- rnorm(n)
# D zavisi od X1 (ne nasumicno)
ps_true <- plogis(0.5*X1 - 0.2*X2)    # stvarni propensity
D <- rbinom(n, 1, ps_true)
Y <- 2*D + X1 + rnorm(n)

data2 <- data.frame(Y, D, X1, X2)

# Naivna razlika u sredinama (obicno PRISTRASNA)
ate_diff2 <- mean(data2$Y[data2$D==1]) - mean(data2$Y[data2$D==0])

# Regresija (kontrola za X)
mod_lm2 <- lm(Y ~ D + X1 + X2, data=data2)
ate_reg2 <- coef(mod_lm2)["D"]

# Matching (propensity score estimated)
m.out2 <- matchit(D ~ X1 + X2, method="nearest", data=data2)
mdata2 <- match.data(m.out2, distance = 'ps', data = data, drop.unmatched = FALSE)
mdata2$Grupa <- ifelse(mdata2$D==1, "Tretirana", "Kontrolna")

# Filtriraj samo uparene za linije
mdata2_matched <- mdata2 %>% 
  filter(weights > 0)
ate_match2 <- with(mdata2_matched, mean(Y[D==1]) - mean(Y[D==0]))

ggplot() +
  # 1️⃣ Linije samo za uparene
  geom_line(
    data = mdata2_matched,
    aes(x = ps, y = Y, group = subclass),
    color = "gray70", alpha = 0.5
  ) +
  # 2️⃣ Tačke za sve jedinice (uparene i neuparene)
  geom_point(
    data = mdata2,
    aes(x = ps, y = Y, color = Grupa),
    size = 2, alpha = 0.8
  ) +
  labs(
    title = "Uparivanje tretiranih i kontrolnih jedinica po verovatnoćama dodele tretmana",
    x = "Verovatnoća dodele tretmana",
    y = "Ishod (Y)"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Tretirana" = "blue", "Kontrolna" = "red"))

# spajamo dobijene podatke
ate_estim <- rbind(
  "D nezavisno od X" = c(ate_diff = ate_diff, ate_reg = ate_reg, ate_match = ate_match),
  "D zavisi od X" = c(ate_diff = ate_diff2, ate_reg = ate_reg2, ate_match = ate_match2)
)
colnames(ate_estim) <- c("Razlika", "Regresija", "Uparivanje")

print(ate_estim)


#pravimo tabelu za ubacivanje u latex
ate_estim <- xtable(ate_estim,
                    caption = 'Razlicite metode procene ATE-a, u razlicitim scenarijima',
                    label = 'tab:ate_estim')
print(ate_estim)
