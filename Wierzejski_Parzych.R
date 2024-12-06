install.packages("ggplot2")
install.packages("fitdistrplus")


library(ggplot2)
library(fitdistrplus)

# Ustawienie lokalizacij konsoli
install.packages("rstudioapi")
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
getwd()



#-----------------------------------------
# Zad 1
# Ceny akcji spółki na rok 2022
#-----------------------------------------


nwg <- read.csv("nwg_d.csv")

kurs_zamkniecia <- nwg$Zamkniecie
kurs_data <- as.Date(nwg$Data)

nwgf <- data.frame(data = kurs_data, zamkniecie = kurs_zamkniecia)

#-----------------------------------------
# Wykres zamknięcia
#-----------------------------------------

wykres_kursu <- ggplot(nwgf, aes(x = data, y = zamkniecie, group = 1)) +
  geom_line(color = "blue") +
  labs(x = NULL, y = "Cena zamknięcia (zł)") +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y",
    limits = c(min(nwgf$data), max(nwgf$data))
  ) +
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1))

wykres_kursu

ggsave(
  "img/Wykres_cen_akcji_nwg.png",
  plot = wykres_kursu,
  width = 12,
  height = 9,
  units = "cm",
  dpi = 480
)

#-----------------------------------------
# Histogram kursów zamknięcia
#-----------------------------------------

histogram_kursu_ggplot <- ggplot(
  nwgf,
  aes(x = zamkniecie, y = after_stat(density))
) +
  geom_histogram(binwidth = 1, fill = "grey", color = "black") +
  labs(title = NULL, x = "Cena zamknięcia (zł)", y = "Gęstość")

hist(kurs_zamkniecia, prob = TRUE, xlab = "Zamknięcie", ylab = "Gęstość")

histogram_kursu_ggplot

ggsave(
  "img/historgram_nwg.png",
  plot = histogram_kursu_ggplot,
  width = 12,
  height = 9,
  units = "cm",
  dpi = 480
)

#-----------------------------------------
# Log zwroty (stopy zwrotu)
#-----------------------------------------


log_zwroty_nwg <- diff(log(kurs_zamkniecia))

# Przycięcie pierwszego elementu kurs_data
kurs_data_cut <- kurs_data[-1]

log_zwroty_nwgdf <- data.frame(data = kurs_data_cut, log_zwroty = log_zwroty_nwg)

?hist


histogram_zwrotów_ggplot <- ggplot(
  log_zwroty_nwgdf,
  aes(x = log_zwroty, y = after_stat(density))
) +
  geom_histogram(fill = "grey", color = "black") +
  labs(title = NULL, x = "log-zwroty (%)", y = "Gęstość")

histogram_zwrotów_ggplot

wykres_zwrotów <- ggplot(log_zwroty_nwgdf, aes(x = data, y = log_zwroty, group = 1)) +
  geom_line(color = "blue") +
  labs(x = NULL, y = "log-zwroty (%)") +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y",
    limits = c(min(kurs_data_cut), max(kurs_data_cut))
  ) +
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1))

wykres_zwrotów

ggsave(
  "img/historgram_zwrotów_nwg.png",
  plot = histogram_zwrotów_ggplot,
  width = 12,
  height = 9,
  units = "cm",
  dpi = 480
)

ggsave(
  "img/wykres_zwrotów_nwg.png",
  plot = wykres_zwrotów,
  width = 12,
  height = 9,
  units = "cm",
  dpi = 480
)

#-----------------------------------------
# Estymacja parametrów rozkładu normalnego i t-studenta
#-----------------------------------------

dist_norm <- fitdist(log_zwroty_nwg, "norm")
dist_t <- fitdist(log_zwroty_nwg, "t", start = list(df = 12))

curve(dt(x, dist_t$estimate), xlim = c(-4, 4), col = 2, lwd = 2)


dist_norm
dist_t
#-----------------------------------------
# Wykresy diagnostyczne
#-----------------------------------------

par(mfrow = c(1, 1))
curve(dnorm(x, dist_norm$estimate[1], dist_norm$estimate[2]), xlim = c(-4, 4), lwd = 2)
curve(dt(x, dist_t$estimate), add = T, col = 2, lwd = 2)

key <- c("norm", "t-student")

png(
  "img/wykresy_diagnostyczne_nwg.png",
  width = 18,
  height = 18,
  pointsize = 9,
  units = "cm",
  res = 480
)
par(mfrow = c(2, 2))
denscomp(list(dist_norm, dist_t), legendtext = key)
qqcomp(list(dist_norm, dist_t), legendtext = key)
cdfcomp(list(dist_norm, dist_t), legendtext = key)
ppcomp(list(dist_norm, dist_t), legendtext = key)

dev.off()


#-----------------------------------------
# Analiza wartości statystyk
#-----------------------------------------

gofstat(
  list(dist_norm, dist_t),
  fitnames = key
)


#-----------------------------------------
# Zad 5
# Test hipotezy o równości rozkładów
#-----------------------------------------

iterations <- 10000
n <- length(log_zwroty_nwg)
n

D <- c()

for (i in 1:iterations) {
  y_ln <- rnorm(n, dist_norm$estimate[1], dist_norm$estimate[2])
  D[i] <- ks.test(
    y_ln,
    pnorm,
    dist_norm$estimate[1],
    dist_norm$estimate[2],
    exact = TRUE
  )$statistic
}

# Obliczamy dn_ln, czyli wartosc statystyki D,
# dla danych kurs_zamkniecia i rozkładu F0 wybranego w punkcie
dn_n <- ks.test(
  log_zwroty_nwg,
  pnorm,
  dist_norm$estimate[1],
  dist_norm$estimate[2],
  exact = TRUE
)$statistic

dn_n

png(
  "img/hipoteza_o_rownosci_nwg.png",
  width = 9,
  height = 12,
  pointsize = 9,
  units = "cm",
  res = 480
)
par(mfrow = c(1, 1))
hist(D, prob = TRUE, xlab = "")
points(dn_n, 0, pch = 19, col = "red")
dev.off()


# Odleglosc dystrybuanty empirycznej dla kurs_zamkniecia,
# oraz dystrybuanty F0 jest istotnie większa od odleglosci obserwowanych
# dla probek tej samej licznosci z rozkladu F0.

p_value_n <- length(D[D > dn_n]) / iterations
p_value_n


alfa <- c(0.05)
p_value_n <= alfa
# Wartosc p-value jest mniejsza od przyjetego poziomu istotnosci, zatem
# hipoteze o rownosci dystrybuant (F = F0, gdzie F poszukiwany rozklad) odrzucam.



#-----------------------------------------
# !TODO: Put dynatrace here
#-----------------------------------------

dyna <- read.csv("dt_us_d.csv")

kurs_zamkniecia_dyn <- dyna$Zamkniecie
kurs_data_dyn <- as.Date(dyna$Data)

log_zwroty_dyn <- diff(log(kurs_zamkniecia_dyn))

# Przycięcie pierwszego elementu kurs_data
kurs_data_cut_dyn <- kurs_data_dyn[-1]

log_zwroty_dyndf <- data.frame(data = kurs_data_cut_dyn, log_zwroty = log_zwroty_dyn)


#-----------------------------------------
# Rozdział 2
# Wykres rozrzutu z histogramami roskładów brzegowych
#-----------------------------------------

# Sprawdzanie czy daty w naszych spółkach się pokrywają
identical(kurs_data, kurs_data_dyn)
which(kurs_data != kurs_data_dyn) # bardzo się nie pokrywają


# Rekreacja tabeli csv ale tylko z dwóch kolumn
temp_nwg <- data.frame(kurs_zamkniecia, kurs_data)
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

?diff

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

