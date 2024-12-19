# Ładowanie biblioteki ggplot2
library(ggplot2)

# Funkcja oryginalna
f <- function(x) exp(-x)

# Wielomiany Taylora
T2 <- function(x) 1 - x + (x^2) / 2
T3 <- function(x) 1 - x + (x^2) / 2 - (x^3) / 6

# Zakres x
x <- seq(-4, 10, length.out = 500)

# Tworzenie ramki danych
data <- data.frame(
  x = x,
  f = f(x),
  T2 = T2(x),
  T3 = T3(x)
)

# Wykres
ggplot(data, aes(x = x)) +
  geom_line(aes(y = f, color = "f(x) = e^(-x)"), size = 1.2) +
  geom_line(aes(y = T2, color = "T2(x)"), linetype = "dashed", size = 1) +
  geom_line(aes(y = T3, color = "T3(x)"), linetype = "dashed", size = 1) +
  scale_color_manual(values = c("blue", "green", "red"),
                     name = "Legenda") +
  labs(
    title = "Aproksymacja funkcji e^(-x) wielomianami Taylora",
    x = "x",
    y = "y"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, size = 16)
  )


# Warunek błędu
tolerance <- 1e-6

# Funkcja obliczająca maksymalny błąd
error <- function(n) {
  (1 / factorial(n + 1)) * (1 / 2)^(n + 1)
}

# Znalezienie minimalnego n
n <- 0
while (error(n) > tolerance) {
  n <- n + 1
}

n

# Wielomian Taylora rzędu 7
T7 <- function(x) {
  1 - x + (x^2) / factorial(2) - (x^3) / factorial(3) + 
    (x^4) / factorial(4) - (x^5) / factorial(5) +
    (x^6) / factorial(6) - (x^7) / factorial(7)
}

# Zakres x
x <- seq(0, 0.5, length.out = 500)

# Tworzenie ramki danych
data <- data.frame(
  x = x,
  f = f(x),
  T7 = T7(x)
)

# Wykres
library(ggplot2)
ggplot(data, aes(x = x)) +
  geom_line(aes(y = f, color = "f(x) = e^(-x)"), size = 1.2) +
  geom_line(aes(y = T7, color = "T7(x)"), linetype = "dashed", size = 1) +
  scale_color_manual(values = c("blue", "red"),
                     name = "Legenda") +
  labs(
    title = "Aproksymacja funkcji e^(-x) wielomianem Taylora rzędu 7",
    x = "x",
    y = "y"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, size = 16)
  )

# Funkcja różnicy
difference <- function(x) {
  abs(f(x) - T7(x))
}

# Znalezienie maksimum różnicy
max_diff <- optimise(difference, interval = c(0, 0.5), maximum = TRUE)

max_diff

# Szukanie maksimum funkcji
max_f <- optimise(f, interval = c(-pi/4, pi/4), maximum = TRUE)
max_T3 <- optimise(T3, interval = c(-pi/4, pi/4), maximum = TRUE)

# Wyniki
cat("Największa wartość f(x):", max_f$objective, "\n")
cat("Największa wartość T3(x):", max_T3$objective, "\n")

# Całka z f
integral_f <- integrate(f, lower = -pi/4, upper = pi/4)$value

# Całka z T3
integral_T3 <- integrate(T3, lower = -pi/4, upper = pi/4)$value

# Wyniki
cat("Całka z f(x):", integral_f, "\n")
cat("Całka z T3(x):", integral_T3, "\n")


# Funkcja oryginalna
fsin <- function(x) sin(2 * x)

# Wielomian Maclaurina T3
T3sin <- function(x) 2 * x - (4 / 3) * x^3

# Wykres funkcji i wielomianu
curve(fsin, from = -pi/4, to = pi/4, col = "blue", lwd = 2, 
      xlab = "x", ylab = "y", main = "Funkcja sin(2x) i T3(x)")
curve(T3sin, from = -pi/4, to = pi/4, col = "red", lwd = 2, lty = 2, add = TRUE)
legend("topright", legend = c("sin(2x)", "T3(x)"), 
       col = c("blue", "red"), lwd = 2, lty = c(1, 2))
grid()

# Całka z sin(2x)
integral_f <- integrate(fsin, lower = -pi/4, upper = pi/4)$value

# Całka z cos(2x)
g <- function(x) cos(2 * x)
integral_g <- integrate(g, lower = -pi/4, upper = pi/4)$value

# Całka z T3
integral_T3 <- integrate(T3sin, lower = -pi/4, upper = pi/4)$value

# Wyniki
cat("Całka z sin(2x):", integral_f, "\n")
cat("Całka z cos(2x):", integral_g, "\n")
cat("Całka z T3(x):", integral_T3, "\n")


# Maksimum funkcji sin(2x)
max_f <- optimise(fsin, interval = c(-pi/4, pi/4), maximum = TRUE)

# Maksimum wielomianu Taylora T3
max_T3 <- optimise(T3sin, interval = c(-pi/4, pi/4), maximum = TRUE)

# Wyniki
cat("Największa wartość sin(2x):", max_f$objective, "w punkcie", max_f$maximum, "\n")
cat("Największa wartość T3(x):", max_T3$objective, "w punkcie", max_T3$maximum, "\n")

