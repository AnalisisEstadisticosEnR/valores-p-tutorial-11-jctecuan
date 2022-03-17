#Calcular valor de p de una distribucion normal
#10-03-2022
#Juan Carlos Martinez

library(tidyverse)

#resultados de la encuesta

Morelos <- c(rep("sigue", 2473707), rep ("revoca", 2461779))
Guanajuato <- c(rep("sigue", 153778), rep("revoca", 189951))

proportions(table(Morelos))
proportions(table(Guanajuato))

#muestreo de 1000 votantes

#simular las encuestas de salida de Guanajuato con set.seed()

set.seed(2020)

survey_a1 <- sample(Guanajuato,1000, replace = F)

table(survey_a1) %>% as.data.frame %>% 
  ggplot(aes(x=survey_a1, y = Freq, label = Freq)) +
  geom_col(fill= c("blue", "red")) +
  geom_label(label = paste(proportions(table(survey_a1))*100,"%")) +
  theme_minimal() +
  ggtitle("Resultados de encuesta en Guanajuato (N = 1,000)")

prop.test(table(survey_a1))

#segunda encuesta

survey_a2 <- sample(Guanajuato,1000, replace = F)

table(survey_a2) %>% as.data.frame %>% 
  ggplot(aes(x=survey_a2, y = Freq, label = Freq)) +
  geom_col(fill= c("red", "green")) +
  geom_label(label = paste(proportions(table(survey_a1))*100,"%")) +
  theme_minimal() +
  ggtitle("Resultados de la segunda encuesta en Guanajuato (N = 1,000)")

#medida extrema

draw_sample <- function(x, n = 1000){
  s <- sample(x, n, F)
  proportions(table(s))
  }

set.seed(2020)

Guanajuato_sim <- replicate(1000, draw_sample(Guanajuato))

Guanajuato_sim[1:2,1:20]

#trazar resultados de 1000 encuestas

dat <- data.frame(t(Guanajuato_sim))

qplot() + theme_minimal() +
  geom_histogram(aes(x = dat$sigue), fill = "red", alpha = .5) +
  geom_histogram(aes(x = dat$revoca), fill = "blue", alpha = .5) +
  geom_vline(xintercept = mean(dat$sigue), color = "red") +
  geom_vline(xintercept = mean(dat$revoca), color = "blue") +
  ggtitle("1,000 encuestas en Guanajuato de 1,000 votantes cada una",
          subtitle = "Histogramas de valores simulados para revocacion (azul) y seguir en mandato (rojo)")

#promediar para saber dato cercano a valor real
c(mean(dat$sigue), mean(dat$revoca))

#desviacion estandar sqr(pqn) 
#distribucion normal de N=1000

voto <- .564

N <- 1000

mean <- voto*N

sd <- sqrt(voto*(1-voto)*N)

x <- (mean - 5*sd):(mean + 5*sd)

norm1 <- dnorm(x, mean, sd)

p <- pnorm(x, mean, sd)

qplot () + theme_minimal() +
  geom_line(aes(x = x, y = norm1), color = "blue") +
  geom_area(aes(x = x, y = norm1), fill = "blue", alpha = .1) +
  geom_vline(xintercept = x[which.min(abs(p-0.025))], color = "blue") + 
  geom_vline(xintercept = x[which.min(abs(p-0.975))], color = "blue") +
  ggtitle("Distribucion normal con N = 1,000",
          subtitle = paste0("Lineas verticales = 95% intervalo alrededor de la media:",
                            round(x[which.min(abs(p-0.025))]/N*100,1), "% a",
                            round(x[which.min(abs(p-0.975))]/N*100,1), "%"))


#muestra mas grande N= 10,000


N <- 10000

mean <- voto*N

sd <- sqrt(voto*(1-voto)*N)

x <- x*10

norm1 <- dnorm(x, mean, sd)

p <- pnorm(x, mean, sd)

qplot () + theme_minimal() +
  geom_line(aes(x = x, y = norm1), color = "blue") +
  geom_area(aes(x = x, y = norm1), fill = "blue", alpha = .1) +
  geom_vline(xintercept = x[which.min(abs(p-0.025))], color = "blue") + 
  geom_vline(xintercept = x[which.min(abs(p-0.975))], color = "blue") +
  ggtitle("Distribucion normal con N = 1,000",
          subtitle = paste0("Lineas verticales = 95% intervalo alrededor de la media:",
                            round(x[which.min(abs(p-0.025))]/N*100,1), "% a",
                            round(x[which.min(abs(p-0.975))]/N*100,1), "%"))

#desviacion estandar

dat <- data.frame(t(Guanajuato_sim))

sd(dat$revoca)

#error estandar sqrt(p*(1-p)/n)

sqrt(0.564*0.436/1000)

#intervalo de confianza

se = sqrt(0.564*0.436/1000)
c(.546-2*se, .564+2*se)

#como hacerlo en R

binom.test(564, 1000, p = 0.5, alternative = "greater")

#Encuesta en Morelos

set.seed(2020)

survey_g1 <- sample(Morelos, 1000, replace = F)

table(survey_g1) %>% as.data.frame %>% 
  ggplot(aes(x = survey_g1, y = Freq, label = Freq)) +
  geom_col(fill = c("red", "blue")) +
  geom_label(label = paste(proportions(table(survey_g1)) *100, "%")) +
  theme_minimal() +
  ggtitle("Resultados de encuesta en Morelos (N= 1,000)")

prop.test(table(survey_g1))


#como evitar error tipo II
#que tan grande debe ser la muestra

set.seed(2020)

survey_g2 <- sample(Morelos, 100000, replace = F)

table(survey_g2) %>% as.data.frame %>% 
  ggplot(aes(x = survey_g2, y = Freq, label = Freq)) +
  geom_col(fill = c("red", "blue")) +
  geom_label(label = paste(proportions(table(survey_g1)) *100, "%")) +
  theme_minimal() +
  ggtitle("Resultados de la segunda encuesta en Morelos (N= 100,000)")

prop.test(table(survey_g2))

#encuesta con fondos ilimitados

survey_g4 <- sample(Morelos, 3000000, replace = F)

table(survey_g4) %>% as.data.frame %>% 
  ggplot(aes(x = survey_g4, y = Freq, label = Freq)) +
  geom_col(fill = c("red", "blue")) +
  geom_label(label = paste(round(proportions(table(survey_g1)) *100,4), "%")) +
  theme_minimal() +
  ggtitle("Resultados de la segunda encuesta en Morelos (N= 3,000,000)")

prop.test(table(survey_g4))
