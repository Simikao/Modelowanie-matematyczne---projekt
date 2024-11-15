install.packages("ggplot2")

library(ggplot2)

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


log_zworty_nwg <- diff(log(kurs_zamkniecia))

# Przycięcie pierwszego elementu kurs_data
kurs_data_cut <- kurs_data[-1]

log_zworty_nwgdf <- data.frame(data = kurs_data_cut, log_zwroty= log_zworty_nwg)

?hist


histogram_zwrotów_ggplot <- ggplot(
  log_zworty_nwgdf,
  aes(x = log_zwroty, y = after_stat(density))
) +
  geom_histogram(fill = "grey", color = "black") +
  labs(title = NULL, x = "log-zwroty (%)", y = "Gęstość")

histogram_zwrotów_ggplot

wykres_zwrotów <- ggplot(log_zworty_nwgdf, aes(x = data, y = log_zwroty, group = 1)) +
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

