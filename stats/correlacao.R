# Correlação 

library(tidyverse)

ggplot(data.frame(x = c(-4, 4)), aes(x)) +
  mapply(function(mean, sd, col) {
    stat_function(fun = dnorm,
                  args = list(mean = mean,
                              sd = sd),
                  aes(col = col), size = 3)
  },
  mean = rep(0, 3),
  sd = c(1, .5, 2),
  col = c("1", "0.5", "2")) +
  scale_colour_brewer("Desvio\nPadrão", palette = "Set1",
                      guide = guide_legend(ncol = 1,
                                           nrow = 3,
                                           byrow = TRUE))

# A correlação mensura o quanto de variação em unidades de desvio padrão duas variáveis estão associadas. Por exemplo, uma 
# correlação de +0.8 indica que conforme X varia 1 desvio padrão, observa-se uma variação de 0.8 desvio padrão em Y e vice-versa 

correlação <- seq(-1, 1, 0.2)

x <- 1:50
y <- rnorm(50, sd = 10)

complemento <- function(y, correlação, x) {
  y.perp <- residuals(lm(x ~ y))
  correlação * sd(y.perp) * y + y.perp * sd(y) * sqrt(1 - correlação^2)
}

X <- data.frame(z = as.vector(sapply(correlação,
                                     function(correlação) complemento(y, correlação, x))),
                correlação = ordered(rep(signif(correlação, 2),
                                         each = length(y))),
                y = rep(y, length(correlação)))
ggplot(X, aes(y, z, group = correlação)) +
  geom_rug(sides = "b") +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "Red", se = FALSE) +
  facet_wrap(~ correlação, scales = "free", labeller = "label_both", ncol = 4) +
  theme(legend.position = "none")


# Pessupostos da Correlação --------------------------------------------------------------------------------------------------------

# Correlação deve ser aplicada somente em variáveis contínuas, intervalares ou ordinais. Correlação não podem ser usadas para
# variáveis nominais (também chamada de categóricas).

# A lógica por trás desse pressuposto é que magnitudes de associação somente podem ser mensuradas em variáveis que de alguma 
# maneira sejam “mensuráveis e comparáveis numericamente” entre si.

# As variáveis tem que possuir um critério de linearidade entre elas. Isto quer dizer que quanto mais/menos de x mais/menos de y.

# Tipos de correlação --------------------------------------------------------------------------------------------------------------

## Correlação de Pearson: 
# a correlação de Pearson é a correlação mais utilizada em análises estatísticas. Ela é uma técnica paramétrica e possui 
# o pressuposto de que ambas as variáveis são distribuídas conforme uma distribuição Normal. Caso tenha dados que violem o 
# pressuposto da normalidade, correlação de Pearson não é o tipo de correlação que você deva usar.

## Correlação de Spearman: 
# a correlação de Spearman é uma técnica não-paramétrica, sendo a alternativa quando os dados violam o pressuposto de 
# normalidade, pois não faz nenhuma suposição que os dados estejam distribuídos de acordo com uma distribuição específica.

## Correlação de Kendall: 
# Assim, como a correlação de Spearman, a correlação de Kendall também é uma técnica não-paramétrica. Sendo também uma 
# alternativa viável quando os dados violam o pressuposto de normalidade, pois não faz nenhuma suposição que os dados 
# estejam distribuídos de acordo com uma distribuição específica.

# Quando usar Kendall ou Spearman? ------------------------------------------------------------------------------------------------

# Ambas devem ser usadas quando o pressuposto de normalidade para ambas ou qualquer uma das variáveis que estão sendo 
# correlacionadas for violado.

# Sugerimos usar a correlação de Kendall, especialmente quando estamos tratando de amostras pequenas (n<100). 

# Porém, há alguns cenários que a correlação de Spearman é melhor indicada: “Se a variável ordinal, Y, tem um grande número 
# de níveis (digamos, cinco ou seis ou mais), então pode-se usar o coeficiente de correlação de classificação de Spearman 
# para medir a força da associação entre X e Y” 


# Como mensurar? -------------------------------------------------------------------------------------------------------------------
library(mnormt) # biblioteca para gerar distribuições multivariadas Normais e não-Normais.

medias <- c(0, 10)
covariancias <- matrix(c(1, 0.6, 0.6, 1), 2, 2)

mv_normal <- as.data.frame(rmnorm(50, medias, covariancias))
mv_student <- as.data.frame(rmt(50, medias, covariancias, df = 1))

# testar prtessuposto de normalidade
# H0  do teste de Shapiro-Wilk é de que “os dados são distribuídos conforme uma distribuição Normal.”

shapiro.test(mv_normal$V1)
shapiro.test(mv_normal$V2)
# ambos os testes para as distribuições multivariadas Normais geram p-valores acima de 0.05 o que faz com que 
# falhemos em rejeitar a hipótese nula de que “os dados são distribuídos conforme uma distribuição Normal.”

shapiro.test(mv_student$V1)
shapiro.test(mv_student$V2)
# ambos os testes para as distribuições multivariadas t de Student geram p-valores abaixo de 0.05 o que faz com que a 
# hipótese nula de que “os dados são distribuídos conforme uma distribuição Normal” é rejeitada.


# Teste de correlação -------------------------------------------------------------------------------------------------------------
# Além de computarmos o valor da correlação entre duas variáveis, é possível também realizar um teste estatístico de 
# hipótese nula sobre a correlação de duas variáveis. 

# A hipótese nula H0 nesse caso é de que “as variáveis possuem correlação igual a zero”.

# method = "pearson" – Correlação de Pearson.
# method = "spearman" – Correlação de Spearman.
# method = "kendall" – Correlação de Kendall.

# Pearson ------------------------------------------------------------------------------------------------------------------------
# Paramétrico
cor(mv_normal$V1, mv_normal$V2, method = "pearson")
cor.test(mv_normal$V1, mv_normal$V2, method = "pearson")


# Spearman -----------------------------------------------------------------------------------------------------------------------
# Não paramétrico
cor(mv_student$V1, mv_student$V2, method = "spearman")
cor.test(mv_student$V1, mv_student$V2, method = "spearman")


# Kendall ------------------------------------------------------------------------------------------------------------------------
# Não paramétrico
cor(mv_student$V1, mv_student$V2, method = "kendall")
cor.test(mv_student$V1, mv_student$V2, method = "kendall")






