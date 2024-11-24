# тестовое задание по биостатистике

# Задание 1.

# задание выборок данных по препаратам
prep_A <- c(48, 52, 55, 39, 60, 57, 46, 53, 50, 45, 63, 51, 40, 37, 44)
prep_B <- c(42, 46, 39, 35, 52, 49, 38, 41, 47, 36, 54, 50, 33, 30, 43)

nA <- length(prep_A)
nB <- length(prep_B)

# построение гистограмм для визуальной оценки распределения данных
hist(prep_A, col = "red", breaks = 6, main = "Частоты процентов снижения симптомов после препарата A")
hist(prep_B, col = "blue", breaks = 6, main = "Частоты процентов снижения симптомов после препарата B")

# проверка (тоже не совсем корректная) на согласие с нормальным распределением
chisq.test(prep_A)
chisq.test(prep_B)

# проведение двухвыборочного t-теста для проверки утверждения компании
f <- t.test(prep_A, prep_B, alternative=c("greater"), conf.level=0.95)
mA <- mean(prep_A)
mB <- mean(prep_B)
s2A <- sd(prep_A)
s2B <- sd(prep_B)


df <- (s2A / 15 + s2B / 15)^2 / (1 / 14 * (s2A / 15)^2 + 1 / 14 * (s2B / 15)^2)
qt(p = 0.95, df = 28)

# Задание 2.

# двухвыборочный тест и его использование для получения требуемого размеры выборки
t.test(prep_A, prep_B, alternative=c("greater"), conf.level=0.95)
t.test(prep_A, prep_B, alternative=c("greater"), conf.level=0.95, var.equal=TRUE)

(qt(0.025, 14) + qt(0.2, 14))^2 / (5 / 10)^2

res <- power.t.test(n = 60, delta=5, sd=10, sig.level=0.05)
power.t.test(n=50, sd=10, power=0.8, delta=5, sig.level=0.05)
qt(0.2, 14)
qt(0.975, 14)
qt(0.975, 28)
qnorm(0.2)
qnorm(0.1)
