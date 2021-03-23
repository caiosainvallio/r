# Analysis of Variance (ANOVA)

# Taxa de erro familiar:
# a probabilidade máxima de que um procedimento consistindo de mais de uma comparação conclua incorretamente 
# que pelo menos uma das diferenças observadas é significativamente diferente da hipótese nula.

library(tidyverse)
library(skimr)

data("ToothGrowth")
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
skim(ToothGrowth)

table(ToothGrowth$supp, ToothGrowth$dose)

# ANOVA ---------------------------------------------------------------------------------------------------------------------------
# Dados independentes
# Variável dependente com distribuição aproximadamente normal
# Variável dependente com homogeneidade das variâncias

# normalidade
shapiro.test(ToothGrowth$len)
# O p-valor do teste é 0.1091005 e com isso falhamos em rejeitar a hipótese nula de que len é distribuída 
# conforma uma distribuição Normal. Ou seja, pressuposto de normalidade não violado.

# Homocedasticidade
car::leveneTest(len ~ supp, data = ToothGrowth)
car::leveneTest(len ~ dose, data = ToothGrowth)
# O p-valor de ambos testes para a homogeneidade de variâncias de len nos grupos de supp e dose são 
# respectivamente 0.28 e 0.53. Com isso falhamos em rejeitar a hipótese nula de que len possui variâncias 
# homogêneas nos grupos tanto de supp quanto de dose. Ou seja, pressuposto de homogeneidade de variâncias não violado.


# ANOVA Unidirecional - One-Way ANOVA ---------------------------------------------------------------------------------------------
# A ANOVA mais simples é chamada de ANOVA Unidirecional que examina a influência de uma variável independente 
# categórica em uma variável contínua dependente.

fit1 <- aov(len ~ supp, data = ToothGrowth)
summary(fit1)
# Podemos ver que a diferença do comprimento dos dentes len conforme o método de dosagem supp (suco de laranja vs ácido ascórbico)
# não é estatisticamente significante (p>0.06). Ao comparar o nível de significância do teste (p=0.06) com o limiar estabelecido 
# (p<0.05) não conseguimos rejeitar a hipótese nula de que não há diferença no comprimento dos dentes.

fit2 <- aov(len ~ dose, data = ToothGrowth)
summary(fit2)
# Porém, o comprimento dos dentes len muda conforme o tamanho da dose dose (0.5, 1.0 ou 2.0 mg) (p<0.05). Ao comparar o nível de 
# significância do teste (p=0.00000000000000095) com o limiar estabelecido (p<0.05) conseguimos rejeitar a hipótese nula de que 
# não há diferença no comprimento dos dentes.


# ANOVA Bidirecional - Two-Way ANOVA ----------------------------------------------------------------------------------------------
# A ANOVA Bidirecional é uma extensão da ANOVA Unidirecional que examina a influência de duas variáveis independentes 
# categóricas em uma variável contínua dependente. Há duas maneiras de analisarmos essa influência:

## Efeitos principais: efeito de uma (ou mais) variável(is) independente(s) em uma variável dependente. Chamamos esses 
## efeitos de aditivos pois podem ser quebrados em dois efeitos distintos e únicos que estão influenciando a variável dependente.

## Efeitos de interações: quando o efeito de uma (ou mais) variável(is) independente(s) em uma variável dependente é afetado 
## pelo nível de outras variável(is) independente(s). Efeitos de interação não são aditivos pois podem ser quebrados em dois 
## efeitos distintos e únicos que estão influenciando a variável dependente. Há uma interação entre as variáveis independentes.


# ANOVA Bidirecional com efeitos principais - Main effects two-way ANOVA ----------------------------------------------------------

fit3 <- aov(len ~ supp + dose, data = ToothGrowth)
summary(fit3)
# Pelos resultados, podemos ver que tanto supp e dose são estatisticamente significantes. Sendo que dose 
# possui o maior tamanho de efeito (F value) 124.0 contra 11.4 de supp.


# ANOVA Bidirecional com efeitos de interação - Interaction effects two-way ANOVA -------------------------------------------------

fit4 <- aov(len ~ supp * dose, data = ToothGrowth)
summary(fit4)
# Como resultado vemos que os efeitos principais tanto de supp e dose se mantiveram com p-valores e tamanho de efeitos similares. 
# Mas a grande novidade agora é que a interação supp:dose também é estatisticamente significante. Nesse caso devemos usar a ANOVA 
# com efeito de interação e não a ANOVA com efeitos principais. Caso a interação supp:dose não fosse estatisticamente significante 
# deveríamos usar a ANOVA com efeitos principais e não a ANOVA com efeito de interação.

# ANOVA Não Paramétrica -----------------------------------------------------------------------------------------------------------
# variável dependente viola os pressupostos de normalidade ou de homogeneidade de variâncias.

# Kruskal-Wallis ------------------------------------------------------------------------------------------------------------------
# ANOVA Unidirecional (One-way) não paramétrica

kruskal.test(len ~ supp, data = ToothGrowth)

# ANOVA Bidirecional (two-way) não paramétrica ------------------------------------------------------------------------------------
# NÃO REALIZAR! Use modelo linar (genelarizado).

# ANOVA de medidas repetidas ------------------------------------------------------------------------------------------------------
# NÃO REALIZAR! Use modelo misto.

# Comparações múltiplas (post-hoc) ------------------------------------------------------------------------------------------------

# Teste de Tukey (paramétrico) ----------------------------------------------------------------------------------------------------
# O teste de Tukey compara as médias de todos os grupos entre si e é considerado o melhor método disponível nos casos em que os
# intervalos de confiança são desejados ou se os tamanhos das amostras são desiguais.

# A estatística de teste usada no teste de Tukey é denotada q e é essencialmente uma estatística t modificada que corrige 
# múltiplas comparações.

TukeyHSD(fit3)
# diff – diferença entre os grupos.
# lwr – intervalo de confiança 95% inferior da diferença.
# upr – intervalo de confiança 95% superior da diferença.
# p adj – estatística q do Teste de Tukey, aqui referida como um p-valor ajustado.

# Teste de Dunn (não paramétrico) -------------------------------------------------------------------------------------------------
# O teste de Tukey assume que a variável dependente é normalmente distribuída e, portanto, não é apropriado como um teste post-hoc 
# após um teste não-paramétrico como Kruskal-Wallis. O único teste post-hoc não paramétrico para esse contexto é o teste de Dunn.

library(DescTools)
DunnTest(len ~ dose, data = ToothGrowth)

# Uma coisa importante de se notar é que não é possível obter um intervalo de confiança de um teste de Dunn.

# Visualização dos testes ---------------------------------------------------------------------------------------------------------

library(ggpubr)
ggboxplot(ToothGrowth, x = "dose", y = "len",
          color = "supp", palette = "lancet",
          add = "jitter", shape = "dose") +
  stat_compare_means(method = "anova")

library(ggpubr)
ggboxplot(ToothGrowth, x = "dose", y = "len",
          color = "supp", palette = "lancet",
          add = "jitter", shape = "dose") +
  stat_compare_means(method = "kruskal.test")

sessionInfo()
