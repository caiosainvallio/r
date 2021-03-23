# Distribuições ----------------------------------------------------------------
library(tidyverse)

# dist norm plot
tibble(x = c(-4, 4)) %>% 
  ggplot(aes(x)) +
  stat_function(size=3, col='red', fun=dnorm) +
  labs(x=NULL, y=NULL)

tibble(x = c(-8, 8)) %>% 
  ggplot(aes(x)) +
  stat_function(size=3, col='red', fun=dnorm) +
  stat_function(size=3, col='blue', fun=dlnorm) +
  labs(x=NULL, y=NULL)

# simulations
set.seed(123)
n_sim <- 1000
sims <- tibble(
  normal = rnorm(n_sim),
  log_normal = rlnorm(n_sim)
)

ggplot(sims) +
  geom_density(aes(normal, fill='Normal'), alpha=0.5) +
  geom_density(aes(log_normal, fill='Log-Normal'), alpha=0.5) +
  labs(x=NULL, y=NULL) +
  scale_fill_manual(name='Distribuição', values=c('Normal'='red', 'Log-Normal'='blue')) +
  theme(legend.position = 'bottom')

# Teste de Shapiro-Wilk --------------------------------------------------------------
# hipótese nula (H0) que “os dados são distribuídos conforme uma distribuição Normal.”
shapiro.test(sims$normal)

shapiro.test(sims$log_normal)
# Sobre o p-valor que aparece como resultado do teste, p<0.05 (p menor que 0.05) significa 
# fortes evidências de que a variável testada não segue uma distribuição Normal.


# Homogeneidade das variâncias ------------------------------------------------------
library(patchwork)
p1 <- ggplot(data.frame(x=c(-4, 4)), aes(x)) +
  mapply(function(mean, sd, col) {
    stat_function(fun=dnorm, args=list(mean=mean, sd=sd), size=3, col=col)
  },
  # enter means, standard deviations and colors here
  mean = c(0, 1, -1),
  sd = c(1, 1, 1),
  col = c('red', 'blue', 'green')
  ) + labs(x=NULL, y=NULL)

p2 <- ggplot(data.frame(x=c(-4, 4)), aes(x)) +
  mapply(function(mean, sd, col) {
    stat_function(fun=dnorm, args=list(mean=mean, sd=sd), size=3, col=col)
  },
  # enter means, standard deviations and colors here
  mean = c(0, 1, .5),
  sd = c(1, .5, 2),
  col = c('red', 'blue', 'green')
  ) + labs(x=NULL, y=NULL)

p1 | p2 

# simulations
sims2 <- tibble(
  group = c(rep('A', n_sim / 4), rep('B', n_sim / 4)),
  homog = c(rnorm(n_sim / 4, 0, 1), rnorm(n_sim / 4, 1, 2)),
  heterog =c(rnorm(n_sim / 4, 0, .1), rnorm(n_sim / 4, 1, 2))
)

p3 <- ggplot(sims2, aes(homog, fill=group)) +
  geom_density(alpha=.5, show.legend = FALSE)

p4 <- ggplot(sims2, aes(heterog, fill=group)) +
  geom_density(alpha=.5, show.legend = FALSE)

p3 | p4

# Teste de Bartlett ------------------------------------------------------------------
# hipótese nula (H0) que “as variâncias dos grupos/estratos são iguais.” 
# O teste de Bartlett é baseado na média dos grupos, portanto é influenciado por observações extremas 
# (também chamadas de outliers).

bartlett.test(homog ~ group, data=sims2)

bartlett.test(heterog ~ group, data=sims2)
# Sobre o p-valor que aparece como resultado do teste de Bartlett, p<0.05 (p menor que 0.05) significa 
# fortes evidências de que a variável testada não possui homogeneidade de variâncias para os grupos especificados.

# Teste de Levene ---------------------------------------------------------------------
# hipótese nula (H0) que “as variâncias dos grupos/estratos são iguais.” 
# o teste de Levene é baseado na mediana dos grupos, o que faz com que seja robusto à outliers. 
# Recomendamos que sempre usem o teste de Levene por conta de ser mais robusto que o teste de Bartlett.

car::leveneTest(homog ~ group, data=sims2)

car::leveneTest(heterog ~ group, data=sims2)
# Sobre o p-valor que aparece como resultado do teste de Levene, p<0.05 (p menor que 0.05) significa fortes 
# evidências de que a variável testada não possui homogeneidade de variâncias para os grupos especificados.









