# Ustawienie lokalizacij konsoli
install.packages("rstudioapi")
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
getwd()

install.packages("ggplot2")
install.packages("fitdistrplus")

library(ggplot2)
library(fitdistrplus)


#-----------------------------------------
# Zad 1
# Ceny akcji spółki na rok 2022
#-----------------------------------------


nwg <- read.csv("nwg_d.csv")

kurs_zamkniecia_nwg <- nwg$Zamkniecie
kurs_data_nwg <- as.Date(nwg$Data)

nwgf <- data.frame(data = kurs_data_nwg, zamkniecie = kurs_zamkniecia_nwg)

dyna <- read.csv("dt_us_d.csv")

kurs_zamkniecia_dyn <- dyna$Zamkniecie
kurs_data_dyn <- as.Date(dyna$Data)

dynf <- data.frame(data = kurs_data_dyn, zamkniecie = kurs_zamkniecia_dyn)


#-----------------------------------------
# Wykres zamknięcia
#-----------------------------------------

wykres_kursu_nwg <- ggplot(nwgf, aes(x = data, y = zamkniecie, group = 1)) +
  geom_line(color = "blue") +
  labs(x = NULL, y = "Cena zamknięcia (zł)") +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y",
    limits = c(min(nwgf$data), max(nwgf$data))
  ) +
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1))

wykres_kursu_nwg

ggsave(
  "img/Wykres_cen_akcji_nwg.png",
  plot = wykres_kursu_nwg,
  width = 12,
  height = 9,
  units = "cm",
  dpi = 480
)

wykres_kursu_dyn <- ggplot(dynf, aes(x = data, y = zamkniecie, group = 1)) +
  geom_line(color = "blue") +
  labs(x = NULL, y = "Cena zamknięcia (zł)") +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y",
    limits = c(min(dynf$data), max(dynf$data))
  ) +
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1))

wykres_kursu_dyn

ggsave(
  "img/Wykres_cen_akcji_dt.png",
  plot = wykres_kursu_dyn,
  width = 12,
  height = 9,
  units = "cm",
  dpi = 480
)

#-----------------------------------------
# Histogram kursów zamknięcia
#-----------------------------------------

histogram_kursu_ggplot_nwg <- ggplot(
  nwgf,
  aes(x = zamkniecie, y = after_stat(density))
) +
  geom_histogram(binwidth = 1, fill = "grey", color = "black") +
  labs(title = NULL, x = "Cena zamknięcia (zł)", y = "Gęstość")

# hist(kurs_zamkniecia_nwg, prob = TRUE, xlab = "Zamknięcie", ylab = "Gęstość", main = "")

histogram_kursu_ggplot_nwg

ggsave(
  "img/histogram_nwg.png",
  plot = histogram_kursu_ggplot_nwg,
  width = 12,
  height = 9,
  units = "cm",
  dpi = 480
)

histogram_kursu_ggplot_dyn <- ggplot(
  dynf,
  aes(x = zamkniecie, y = after_stat(density))
) +
  geom_histogram(binwidth = 1, fill = "grey", color = "black") +
  labs(title = NULL, x = "Cena zamknięcia (zł)", y = "Gęstość")

# hist(kurs_zamkniecia_dyn, prob = TRUE, xlab = "Zamknięcie", ylab = "Gęstość", main = "")

histogram_kursu_ggplot_dyn

ggsave(
  "img/histogram_dt.png",
  plot = histogram_kursu_ggplot_dyn,
  width = 12,
  height = 9,
  units = "cm",
  dpi = 480
)

#-----------------------------------------
# Log zwroty (stopy zwrotu)
#-----------------------------------------

log_zwroty_nwg <- diff(log(kurs_zamkniecia_nwg))
log_zwroty_dt <- diff(log(kurs_zamkniecia_dyn))

# Przycięcie pierwszego elementu kurs_data_nwg
kurs_data_cut_nwg <- kurs_data_nwg[-1]

log_zwroty_nwg_df <- data.frame(data = kurs_data_cut_nwg, log_zwroty = log_zwroty_nwg)

# Przycięcie pierwszego elementu kurs_data_dyn
kurs_data_cut_dt <- kurs_data_dyn[-1]

log_zwroty_dt_df <- data.frame(data = kurs_data_cut_dyn, log_zwroty = log_zwroty_dt)

histogram_zwrotów_nwg_ggplot <- ggplot(
  log_zwroty_nwg_df,
  aes(x = log_zwroty, y = after_stat(density))
) +
  geom_histogram(fill = "grey", color = "black") +
  labs(title = NULL, x = "log-zwroty (%)", y = "Gęstość")

histogram_zwrotów_nwg_ggplot

wykres_zwrotów_nwg <- ggplot(log_zwroty_nwg_df, aes(x = data, y = log_zwroty, group = 1)) +
  geom_line(color = "blue") +
  labs(x = NULL, y = "log-zwroty (%)") +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y",
    limits = c(min(kurs_data_cut_nwg), max(kurs_data_cut_nwg))
  ) +
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1))

wykres_zwrotów_nwg

ggsave(
  "img/histogram_zwrotów_nwg.png",
  plot = histogram_zwrotów_nwg_ggplot,
  width = 12,
  height = 9,
  units = "cm",
  dpi = 480
)

# dt

histogram_zwrotów_dt_ggplot <- ggplot(
  log_zwroty_dt_df,
  aes(x = log_zwroty, y = after_stat(density))
) +
  geom_histogram(fill = "grey", color = "black") +
  labs(title = NULL, x = "log-zwroty (%)", y = "Gęstość")

histogram_zwrotów_dt_ggplot

wykres_zwrotów_dt <- ggplot(log_zwroty_dt_df, aes(x = data, y = log_zwroty, group = 1)) +
  geom_line(color = "blue") +
  labs(x = NULL, y = "log-zwroty (%)") +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y",
    limits = c(min(kurs_data_cut_dt), max(kurs_data_cut_dt))
  ) +
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1))

wykres_zwrotów_dt

ggsave(
  "img/histogram_zwrotów_dt.png",
  plot = histogram_zwrotów_dt_ggplot,
  width = 12,
  height = 9,
  units = "cm",
  dpi = 480
)

#-----------------------------------------
# [NWG] Estymacja kwantyli 0.05, 0.5, 0.95
#-----------------------------------------
# Wartość oczekiwana
mu_nwg <- mean(log_zwroty_nwg)

# Wariancja
variance_nwg <- var(log_zwroty_nwg)

# Odchylenie standardowe
std_dev_nwg <- sqrt(variance_nwg)

# Kwantyle
quantiles_nwg <- quantile(log_zwroty_nwg, probs = c(0.05, 0.5, 0.95))

mu_nwg
variance_nwg
std_dev_nwg
quantiles_nwg


# Tworzenie histogramu z naniesionymi liniami dla średniej i kwantyli
wykres_kwantyli_nwg <- ggplot(log_zwroty_nwg_df, aes(x = log_zwroty_nwg)) +
  geom_histogram(binwidth = 0.01, fill = "grey", color = "black", alpha = 0.7) + # Histogram
  geom_vline(aes(xintercept = mu_nwg, color = "Średnia"), linetype = "dashed", size = 1) + # Średnia
  geom_vline(aes(xintercept = quantiles_nwg[1], color = "Kwantyl 5%"), linetype = "dotted", size = 1) + # Kwantyl 5%
  geom_vline(aes(xintercept = quantiles_nwg[2], color = "Mediana"), linetype = "dotted", size = 1) + # Mediana
  geom_vline(aes(xintercept = quantiles_nwg[3], color = "Kwantyl 95%"), linetype = "dotted", size = 1) + # Kwantyl 95%
  scale_color_manual(name = "Legenda", 
                     values = c("Średnia" = "red", 
                                "Kwantyl 5%" = "blue", 
                                "Mediana" = "green", 
                                "Kwantyl 95%" = "orange")) + # Kolory i etykiety legendy
  labs(title = NULL, x = "Log-Zwroty", y = "Gęstość") + # Tytuły osi i wykresu
  theme_minimal() + # Minimalny styl wykresu
  theme(legend.position = "top") # Ustawienie legendy na górze

wykres_kwantyli_nwg

ggsave(
  "img/estymacja_kwantyli_nwg.png",
  plot = wykres_kwantyli_nwg,
  width = 12,
  height = 9,
  units = "cm",
  dpi = 480
)

#-----------------------------------------
# [DT] Estymacja kwantyli 0.05, 0.5, 0.95
#-----------------------------------------
# Wartość oczekiwana
mu_dt <- mean(log_zwroty_dt)

# Wariancja
variance_dt <- var(log_zwroty_dt)

# Odchylenie standardowe
std_dev_dt <- sqrt(variance_dt)

# Kwantyle
quantiles_dt <- quantile(log_zwroty_dt, probs = c(0.05, 0.5, 0.95))

mu_dt
variance_dt
std_dev_dt
quantiles_dt


# Tworzenie histogramu z naniesionymi liniami dla średniej i kwantyli
wykres_kwantyli_dt <- ggplot(log_zwroty_dt_df, aes(x = log_zwroty_dt)) +
  geom_histogram(binwidth = 0.01, fill = "grey", color = "black", alpha = 0.7) + # Histogram
  geom_vline(aes(xintercept = mu_dt, color = "Średnia"), linetype = "dashed", size = 1) + # Średnia
  geom_vline(aes(xintercept = quantiles_dt[1], color = "Kwantyl 5%"), linetype = "dotted", size = 1) + # Kwantyl 5%
  geom_vline(aes(xintercept = quantiles_dt[2], color = "Mediana"), linetype = "dotted", size = 1) + # Mediana
  geom_vline(aes(xintercept = quantiles_dt[3], color = "Kwantyl 95%"), linetype = "dotted", size = 1) + # Kwantyl 95%
  scale_color_manual(name = "Legenda", 
                     values = c("Średnia" = "red", 
                                "Kwantyl 5%" = "blue", 
                                "Mediana" = "green", 
                                "Kwantyl 95%" = "orange")) + # Kolory i etykiety legendy
  labs(title = NULL, x = "Log-Zwroty", y = "Gęstość") + # Tytuły osi i wykresu
  theme_minimal() + # Minimalny styl wykresu
  theme(legend.position = "top") # Ustawienie legendy na górze

wykres_kwantyli_dt

ggsave(
  "img/estymacja_kwantyli_dt.png",
  plot = wykres_kwantyli_dt,
  width = 12,
  height = 9,
  units = "cm",
  dpi = 480
)


#-----------------------------------------
# [NWG] Esytmacjia dystrybuanty empirycznej
#-----------------------------------------

# Estymacja dystrybuanty empirycznej
dystrybuanta_empiryczna <- ecdf(log_zwroty_nwg)

# Wartości x dla wykresu
x_wartosci <- seq(min(log_zwroty_nwg), max(log_zwroty_nwg), length.out = 100)

# Wartości dystrybuanty empirycznej
F_empiryczna <- dystrybuanta_empiryczna(x_wartosci)

temp_df <- data.frame(x = x_wartosci, F = F_empiryczna)

dystrybuanta_empiryczna_nwg <- ggplot(temp_df, aes(x = x, y = F)) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = NULL,
    x = "Log-Zwroty",
    y = "Dystrybuanta Empiryczna F(x)"
  ) +
  theme_minimal()

dystrybuanta_empiryczna_nwg

ggsave(
  "img/dystrybuanta_empiryczna_nwg.png",
  plot = dystrybuanta_empiryczna_nwg,
  width = 12,
  height = 9,
  units = "cm",
  dpi = 480
)

#-----------------------------------------
# [DT] Esytmacjia dystrybuanty empirycznej
#-----------------------------------------

# Estymacja dystrybuanty empirycznej
dystrybuanta_empiryczna <- ecdf(log_zwroty_dt)

# Wartości x dla wykresu
x_wartosci <- seq(min(log_zwroty_dt), max(log_zwroty_dt), length.out = 100)

# Wartości dystrybuanty empirycznej
F_empiryczna <- dystrybuanta_empiryczna(x_wartosci)

temp_df <- data.frame(x = x_wartosci, F = F_empiryczna)

dystrybuanta_empiryczna_dt <- ggplot(temp_df, aes(x = x, y = F)) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = NULL,
    x = "Log-Zwroty",
    y = "Dystrybuanta Empiryczna F(x)"
  ) +
  theme_minimal()

dystrybuanta_empiryczna_dt

ggsave(
  "img/dystrybuanta_empiryczna_dt.png",
  plot = dystrybuanta_empiryczna_dt,
  width = 12,
  height = 9,
  units = "cm",
  dpi = 480
)

#-----------------------------------------
# [NWG] Estymacja parametrów rozkładu normalnego i t-studenta
#-----------------------------------------

dist_norm_nwg <- fitdist(log_zwroty_nwg, "norm")
dist_t_nwg <- fitdist(log_zwroty_nwg, "t", start = list(df = 12))

curve(dt(x, dist_t_nwg$estimate), xlim = c(-4, 4), col = 2, lwd = 2)

dist_norm_nwg
dist_t_nwg

#-----------------------------------------
# [DT] Estymacja parametrów rozkładu normalnego i t-studenta
#-----------------------------------------

dist_norm_dt <- fitdist(log_zwroty_dt, "norm")
dist_t_dt <- fitdist(log_zwroty_dt, "t", start = list(df = 12))

curve(dt(x, dist_t_dt$estimate), xlim = c(-4, 4), col = 2, lwd = 2)

dist_norm_dt
dist_t_dt

#-----------------------------------------
# [NWG] Wykresy diagnostyczne
#-----------------------------------------

par(mfrow = c(1, 1))
curve(dnorm(x, dist_norm_nwg$estimate[1], dist_norm_nwg$estimate[2]), xlim = c(-4, 4), lwd = 2)
curve(dt(x, dist_t_nwg$estimate), add = T, col = 2, lwd = 2)

key_nwg <- c("norm", "t-student")

png(
  "img/wykresy_diagnostyczne_nwg.png",
  width = 18,
  height = 18,
  pointsize = 9,
  units = "cm",
  res = 480
)
par(mfrow = c(2, 2))
denscomp(list(dist_norm_nwg, dist_t_nwg), legendtext = key_nwg)
qqcomp(list(dist_norm_nwg, dist_t_nwg), legendtext = key_nwg)
cdfcomp(list(dist_norm_nwg, dist_t_nwg), legendtext = key_nwg)
ppcomp(list(dist_norm_nwg, dist_t_nwg), legendtext = key_nwg)

dev.off()


#-----------------------------------------
# [DT] Wykresy diagnostyczne
#-----------------------------------------

par(mfrow = c(1, 1))
curve(dnorm(x, dist_norm_dt$estimate[1], dist_norm_dt$estimate[2]), xlim = c(-4, 4), lwd = 2)
curve(dt(x, dist_t_dt$estimate), add = T, col = 2, lwd = 2)

key_dt <- c("norm", "t-student")

png(
  "img/wykresy_diagnostyczne_dt.png",
  width = 18,
  height = 18,
  pointsize = 9,
  units = "cm",
  res = 480
)
par(mfrow = c(2, 2))
denscomp(list(dist_norm_dt, dist_t_dt), legendtext = key_dt)
qqcomp(list(dist_norm_dt, dist_t_dt), legendtext = key_dt)
cdfcomp(list(dist_norm_dt, dist_t_dt), legendtext = key_dt)
ppcomp(list(dist_norm_dt, dist_t_dt), legendtext = key_dt)

dev.off()


#-----------------------------------------
# [NWG] Analiza wartości statystyk
#-----------------------------------------

gofstat(
  list(dist_norm_nwg, dist_t_nwg),
  fitnames = key_nwg
)

#-----------------------------------------
# [DT] Analiza wartości statystyk
#-----------------------------------------

gofstat(
  list(dist_norm_dt, dist_t_dt),
  fitnames = key_dt
)


#-----------------------------------------
# Zad 5
# [NWG] Test hipotezy o równości rozkładów
#-----------------------------------------

iterations_nwg <- 10000
n_nwg <- length(log_zwroty_nwg)
n_nwg

D_nwg <- c()

for (i in 1:iterations) {
  y_ln_nwg <- rnorm(n, dist_norm_nwg$estimate[1], dist_norm_nwg$estimate[2])
  D_nwg[i] <- ks.test(
    y_ln_nwg,
    pnorm,
    dist_norm_nwg$estimate[1],
    dist_norm_nwg$estimate[2],
    exact = TRUE
  )$statistic
}

# Obliczamy dn_ln, czyli wartosc statystyki D,
# dla danych kurs_zamkniecia_nwg i rozkładu F0 wybranego w punkcie
dn_n_nwg <- ks.test(
  log_zwroty_nwg,
  pnorm,
  dist_norm_nwg$estimate[1],
  dist_norm_nwg$estimate[2],
  exact = TRUE
)$statistic

dn_n_nwg

png(
  "img/hipoteza_o_rownosci_nwg.png",
  width = 9,
  height = 12,
  pointsize = 9,
  units = "cm",
  res = 480
)
par(mfrow = c(1, 1))
hist(D_nwg, prob = TRUE, xlab = "")
points(dn_n_nwg, 0, pch = 19, col = "red")
dev.off()


# Odleglosc dystrybuanty empirycznej dla kurs_zamkniecia_nwg,
# oraz dystrybuanty F0 jest istotnie większa od odleglosci obserwowanych
# dla probek tej samej licznosci z rozkladu F0.

p_value_n_nwg <- length(D_nwg[D_nwg > dn_n_nwg]) / iterations_nwg
p_value_n_nwg


alfa_nwg <- c(0.05)
p_value_n_nwg <= alfa_nwg
# Wartosc p-value jest mniejsza od przyjetego poziomu istotnosci, zatem
# hipoteze o rownosci dystrybuant (F = F0, gdzie F poszukiwany rozklad) odrzucam.


#-----------------------------------------
# Zad 5
# [DT] Test hipotezy o równości rozkładów
#-----------------------------------------

iterations_dt <- 10000
n_dt <- length(log_zwroty_dt)
n_dt

D_dt <- c()

for (i in 1:iterations) {
  y_ln_dt <- rnorm(n, dist_norm_dt$estimate[1], dist_norm_dt$estimate[2])
  D_dt[i] <- ks.test(
    y_ln_dt,
    pnorm,
    dist_norm_dt$estimate[1],
    dist_norm_dt$estimate[2],
    exact = TRUE
  )$statistic
}

# Obliczamy dn_ln, czyli wartosc statystyki D,
# dla danych kurs_zamkniecia_dt i rozkładu F0 wybranego w punkcie
dn_n_dt <- ks.test(
  log_zwroty_dt,
  pnorm,
  dist_norm_dt$estimate[1],
  dist_norm_dt$estimate[2],
  exact = TRUE
)$statistic

dn_n_dt

png(
  "img/hipoteza_o_rownosci_dt.png",
  width = 9,
  height = 12,
  pointsize = 9,
  units = "cm",
  res = 480
)
par(mfrow = c(1, 1))
hist(D_dt, prob = TRUE, xlab = "")
points(dn_n_dt, 0, pch = 19, col = "red")
dev.off()


# Odleglosc dystrybuanty empirycznej dla kurs_zamkniecia_dt,
# oraz dystrybuanty F0 jest istotnie większa od odleglosci obserwowanych
# dla probek tej samej licznosci z rozkladu F0.

p_value_n_dt <- length(D_dt[D_dt > dn_n_dt]) / iterations_dt
p_value_n_dt


alfa_dt <- c(0.05)
p_value_n_dt <= alfa_dt
# Wartosc p-value jest mniejsza od przyjetego poziomu istotnosci, zatem
# hipoteze o rownosci dystrybuant (F = F0, gdzie F poszukiwany rozklad) odrzucam.


#-----------------------------------------
# Rozdział 2
# Wykres rozrzutu z histogramami rozkładów brzegowych
#-----------------------------------------

# Sprawdzanie czy daty w naszych spółkach się pokrywają
identical(kurs_data_nwg, kurs_data_dyn)
which(kurs_data_nwg != kurs_data_dyn) # bardzo się nie pokrywają


# Rekreacja tabeli csv ale tylko z dwóch kolumn
temp_nwg <- data.frame(kurs_zamkniecia_nwg, kurs_data_nwg)
colnames(temp_nwg) <- c("kurs", "data")
temp_dyn <- data.frame(kurs_zamkniecia_dyn, kurs_data_dyn)
colnames(temp_dyn) <- c("kurs", "data")

# Spajanie obu spółek na podstawie dat
merged_kursy <- merge(temp_nwg, temp_dyn, by = "data")
merged_kursy

# Wykonywanie operacji na przygotowanych i spojonych danych
m_log_kursu_nwg <- log(merged_kursy$kurs.x)
m_log_kursu_dyn <- log(merged_kursy$kurs.y)

m_log_zwroty_nwg <- diff(m_log_kursu_nwg)
m_log_zwroty_dyn <- diff(m_log_kursu_dyn)

# ?diff

# Tworzenie wykresów na podstawie danych
library(ggExtra)
merged_logi <- data.frame(m_log_zwroty_dyn, m_log_zwroty_nwg)
merged_logi
l <- ggplot(merged_logi, aes(x = m_log_zwroty_dyn, y = m_log_zwroty_nwg)) +
  geom_point()
l
rozrzut <- ggMarginal(l, type = "histogram")
rozrzut
ggsave(
  "img/rozrzut_z_histogramami.png",
  plot = rozrzut,
  width = 12,
  height = 9,
  units = "cm",
  dpi = 480
)

#-----------------------------------------
# estymacja wektora srednich, macierzy kowariancji, macierzy korelacji
#-----------------------------------------

mu <- colMeans(merged_logi) # wektor średnich
mu
Sigma <- cov(merged_logi) # estymator nieobciazony

n <- nrow(merged_logi)
n
Sigma_ob <- (n - 1) * cov(merged_logi) / n # estymator obciążony

Sigma
Sigma_ob

P <- cor(merged_logi) # macierz korelacji
P

sqrtthing <- sqrt(Sigma[1, 1])

sqrtthing
#-----------------------------------------
# wykres gestosci
#-----------------------------------------
library(mnormt)

s1 <- s2 <- 1 # odchylenia standardowe
s1 <- sqrt(Sigma[2, 2])
s2 <- sqrt(Sigma[1, 1])

x <- seq(-3.5 * s1, 3.5 * s1, 0.005)
y <- seq(-3.5 * s2, 3.5 * s2, 0.005)

# gestosc rozkladu normalnego o sredniej mu i macierzy kowariancji S
f <- function(x, y) dmnorm(cbind(x, y), mu, Sigma)
z <- outer(x, y, f)
# Funkcja outer tworzy macierz wynikową z
# przez aplikację funkcji f na wszystkie kombinacje x i y.

# Wykres jednowymiarowy dla NWG
m_log_zwroty_nwg_dens <- dnorm(x, mean = mu[2], sd = sqrt(Sigma[2, 2]))

# Wykres jednowymiarowy dla Dyn
m_log_zwroty_dyn_dens <- dnorm(y, mean = mu[1], sd = sqrt(Sigma[1, 1]))

# Rysowanie wykresów jednowymiarowych
m_log_zwroty_nwg_jednowymiarowy <- ggplot() +
  geom_line(aes(x = x, y = m_log_zwroty_nwg_dens), color = "blue") +
  ggtitle("Gęstość jednowymiarowa dla NWG")
m_log_zwroty_nwg_jednowymiarowy

m_log_zwroty_dyn_jednowymiarowy <- ggplot() +
  geom_line(aes(x = y, y = m_log_zwroty_dyn_dens), color = "blue") +
  ggtitle("Gęstość jednowymiarowa dla Dynatrace")
m_log_zwroty_dyn_jednowymiarowy

ggsave(
  "img/zwroty_nwg_wykres_jednowymiarowy.png",
  plot = m_log_zwroty_nwg_jednowymiarowy,
  width = 12,
  height = 9,
  units = "cm",
  dpi = 480
)

ggsave(
  "img/zwroty_dyn_wykres_jednowymiarowy.png",
  plot = m_log_zwroty_dyn_jednowymiarowy,
  width = 12,
  height = 9,
  units = "cm",
  dpi = 480
)

# Połączenie obu wykresów na jeden plik png
library(cowplot)
zwroty_merged_jednowymiarowy <- plot_grid(
  m_log_zwroty_nwg_jednowymiarowy,
  m_log_zwroty_dyn_jednowymiarowy
)
zwroty_merged_jednowymiarowy

ggsave(
  "img/zwroty_wykresy_jednowymiarowe.png",
  plot = zwroty_merged_jednowymiarowy,
  width = 18,
  height = 6,
  units = "cm",
  dpi = 480
)



# wykres gestosci
png(
  "img/zwroty_gestosc_laczona.png",
  width = 9,
  height = 12,
  pointsize = 9,
  units = "cm",
  res = 480
)
persp(x, y, z, theta = 30, phi = 30, col = "lightblue", main = "Gęstość łączna")
dev.off()

# lub dokladniejszy wykres
png(
  "img/diff_gestosc_laczona_detailed.png",
  width = 9,
  height = 12,
  pointsize = 9,
  units = "cm",
  res = 480
)
persp(x, y, z,
  theta = -30, phi = 25,
  shade = 0.75, col = "lightblue", expand = 0.5, r = 2,
  ltheta = 25, ticktype = "detailed", main = "Gęstość łączna"
)
dev.off()

