# Testes 
library(tidyverse)

# Teste t ------------------------------------------------------------------------------------
# Dados independentes
# Variável dependente com distribuição aproximadamente normal
# Variável dependente com homogeneidade das variâncias

# Welch adaptou o teste t de Student para ser robusto perante heterogeneidade das variâncias.
# No R a função t.test() possui como padrão o teste t de Welch e caso você queira explicitamente 
# usar o teste t de Student você deve incluir o argumento var.equal = TRUE na função.


# Teste t para amostras independentes --------------------------------------------------------

n_sim_t <- 20
sim3 <- tibble(
  group = c(rep('A', n_sim_t), rep('B', n_sim_t)),
  measure = c(rnorm(n_sim_t, mean=0), rnorm(n_sim_t, mean = 5))
)

t.test(measure ~ group, data = sim3)
# O resultado para a simulação é um p-valor menor que 0.05, ou seja um resultado significante apontando 
# que podemos rejeitar a hipótese nula (fortes evidências contrárias que as médias dos grupos são iguais).


# Teste t para amostras pareadas --------------------------------------------------------------

amostra_1 <- tibble(measure = rnorm(n_sim_t, mean=0))
amostra_2 <- tibble(measure = rnorm(n_sim_t, mean=5))

t.test(amostra_1$measure, amostra_2$measure, paired = TRUE)


# Teste t não-paramétricos --------------------------------------------------------------------
# dados violam o pressuposto de normalidade.

# Atenção: testes não-paramétricos são menos sensíveis em rejeitar a hipótese nula quando ela é 
# verdadeira (erro tipo I) do que testes paramétricos quando o pressuposto de normalidade não é violado. 
# Então não pense que deve sempre aplicar um teste não-paramétrico em todas as ocasiões.

# Teste de Mann-Whitney -----------------------------------------------------------------------
# alternativa não-paramétrica ao teste t para amostras independentes.

sim4 <- tibble(
  group = c(rep("A", n_sim_t), rep("B", n_sim_t)),
  measure = c(rlnorm(n_sim_t, mean = 0), rlnorm(n_sim_t, mean = 5))
)

wilcox.test(measure ~ group, data = sim4, conf.int=TRUE)


# Teste de Wilcoxon -------------------------------------------------------------------------
# alternativa não-paramétrica ao teste t para amostras pareadas.

amostra_3 <- tibble(measure = rlnorm(n_sim_t, mean = 0))
amostra_4 <- tibble(measure = rlnorm(n_sim_t, mean = 5))

wilcox.test(amostra_3$measure, amostra_4$measure, paired = TRUE, conf.int = TRUE)


# Visualização dos testes ------------------------------------------------------------------
# install.packages('ggpubr')
library(ggpubr)
ggboxplot(sim3, x = "group", y = "measure", color = "group",
          palette = "lancet", add = "jitter") +
  stat_compare_means(method = "t.test")

ggpaired(sim3, x = "group", y = "measure", color = "group",
         palette = "lancet", line.color = "gray", line.size = 0.4) +
  stat_compare_means(method = "t.test", paired = TRUE)




