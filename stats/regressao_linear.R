# Regressão Linear
library(tidyverse)

# Regressão linear permite com que você use uma ou mais variáveis discretas ou contínuas como variáveis independentes e mensurar o 
# poder de associação com a variável dependente, que deve ser contínua.


## Interpretação Geométrica: Regressão como uma reta.
## Interpretação Matemática: Regressão como otimização.dy/dx=0
## Interpretação Estatística: Regressão como poder de associação entre variáveis controlando para diversos outros efeitos.

# Pressupostos ----------------------------------------------------------------------------------------------------------------------------

## Independência dos Dados: ##
# o valor de uma observação não influencia ou afeta o valor de outras observações. Este é o pressuposto clássico de todas as técnicas 
# abordadas até agora.

## Linearidade dos Dados: ##
# a relação entre as variáveis independentes e a variável dependente é considerada linear (quanto mais/menos de uma, mais/menos de outra). 
# Linearidade dos Dados pode ser verificada graficamente observando a dispersão dos resíduos com os valores previstos pela regressão.

## Independência dos Erros / Resíduos: ##
# os erros (também chamados de resíduos) não devem possuir correlação. Este pressuposto pode ser testado pelo teste de Durbin-Watson e 
# observando o gráfico quantil-quantil (Q-Q) dos resíduos padronizados.

## Homogeneidade de Variância dos Erros / Resíduos: ##
# os erros devem ter média zero e desvio padrão constante ao longo das observações. Similar ao teste de Levene, mas aplicado aos resíduos 
# da regressão. Pode ser testado usando o Teste de Breusch-Pagan.

## Ausência de Multicolinearidade: ##
# multicolinearidade é a ocorrência de alta correlação entre duas ou mais variáveis independentes e pode levar a resultados distorcidos. 
# Em geral, a multicolinearidade pode fazer com que os intervalos de confiança se ampliem, ou até mudar o sinal de influência das variáveis 
# independentes (de positivo para negativo, por exemplo). Portanto, as inferências estatísticas de uma regressão com multicolinearidade não 
# são confiáveis. Pode ser testado usando o Fator de Inflação de Variância (Variance Inflation Factor – VIF).



library(skimr)
data(mtcars)
skimr::skim(mtcars)

# Modelo linear com variáveis independentes contínuas ---------------------------------------------------------------------------------------
modelo_simples <- lm(mpg ~ hp + wt, data = mtcars)
summary(modelo_simples)

# hipótese nula dos coeficientes da regressão é de que “os coeficientes são nulos/zeros”, então os p-valores devem ser interpretados como
# a probabilidade de observamos valores de coeficientes tão extremos dado que a hipótese nula é verdadeira.

# o acréscimo de 1 unidade da variável independente gera o aumento de <coeficiente> unidade(s) da variável dependente.

# regressão linear controla os efeitos de outras variáveis independente.

# A constante (intercept) representa o valor médio da variável dependente quando todas as variáveis independentes possuem valor nulo (zero).

confint(modelo_simples)


# Modelo linear com variáveis independentes categóricas -------------------------------------------------------------------------------------
modelo_quali <- mtcars %>%
  mutate(am = as.factor(am)) %>%
  lm(mpg ~ hp + wt + am, data = .)
summary(modelo_quali)

modelo_quali$xlevels


# Efeitos Principais e Efeitos de Interação ------------------------------------------------------------------------------------------------
# podemos também mostrar efeitos de interação (também chamados de efeitos de moderação) entre duas variáveis.

## Efeitos principais: ##
# efeito de uma (ou mais) variável(is) independente(s) em uma variável dependente. Chamamos esses efeitos de aditivos pois podem ser 
# quebrados em dois efeitos distintos e únicos que estão influenciando a variável dependente.

## Efeitos de interações: ##
# quando o efeito de uma (ou mais) variável(is) independente(s) em uma variável dependente é afetado pelo nível de outras variável(is) 
# independente(s). Efeitos de interação não são aditivos pois podem ser quebrados em dois efeitos distintos e únicos que estão 
# influenciando a variável dependente. Há uma interação entre as variáveis independentes.


mtcars %>%
  mutate(across(c(am, cyl), as.factor)) %>%
  group_by(am, cyl) %>%
  summarise(mpg = mean(mpg)) %>%
  ggplot(aes(x = am, y = mpg, color = cyl)) +
  geom_line(aes(group = cyl)) +
  geom_point() +
  scale_colour_brewer(palette = "Set1")

modelo_interacao <- lm(mpg ~ hp + wt + cyl * am, data = mtcars)
summary(modelo_interacao)


# Efeitos não lineares ------------------------------------------------------------------------------------------------------------------
mtcars %>%
  ggplot(aes(hp, mpg)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "Red") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "Blue")


modelo_naolinear <- lm(mpg ~ poly(hp, 2) + wt, data = mtcars)
summary(modelo_naolinear)


# Visualização ---------------------------------------------------------------------------------------------------------------------------

library(sjPlot)
# Intervalo de confiança 95%
forest_raw <- sjPlot::plot_model(modelo_quali, sort.est = TRUE, show.values = TRUE)
forest_raw 

# Desvio padrão
forest_std <- sjPlot::plot_model(modelo_quali, type = "std")
forest_std

# Verificação de pressupostos ---------------------------------------------------------------------------------------------------------

## Inspeção visual ---------------------------------------------------------------------------------------------------------------------


## Residuals vs Fitted (Resíduos vs Valores Previstos): ##
# utilizado para verificar os pressupostos de linearidade. Uma linha horizontal, sem padrões distintos é um indicativo de uma relação 
#  linear, o que indica que o pressuposto não foi violado.

## Normal Q-Q (Q-Q dos Resíduos): ##
# utilizado para verificar se os resíduos estão normalmente distribuídos. Se os pontos residuais seguirem a linha reta tracejada, 
# indica que o pressuposto de independência dos resíduos não foi violado.

## Scale-Location (Resíduos “Studentizados” vs Valores Previstos): ##
# utilizado para verificar a homogeneidade da variância dos resíduos (homocedasticidade). Uma linha horizontal com pontos igualmente 
# espalhados é uma boa indicação que o pressuposto não foi violado. Usa o formato “Studentizado” dos resíduos que é o quociente resultante 
# da divisão de um resíduo por uma estimativa de seu desvio padrão. É uma forma de estatística t de Student, com a estimativa de erro 
# variando entre os pontos.

## Residuals vs Leverage (Resíduos vs Influências): ##
# Utilizado para identificar casos influentes, ou seja, valores extremos que podem influenciar os resultados da regressão quando 
# incluídos ou excluídos da análise.


library(ggfortify)
autoplot(modelo_simples)

# exemplo que demonstra diversas patologias (problemas) do modelo. Os resíduos (gráfico superior esquerdo) possui um padrão evidente, 
# o gráfico Q-Q dos resíduos (gráfico superior direito) evidencia algumas observações com resíduos que ferem o pressuposto de independência 
# dos resíduos, e por fim possuímos uma observação extremamente influente (gráfico inferior direito – Maserati B) que talvez prejudique as 
# inferências do modelo.



## Testes estatísticos -------------------------------------------------------------------------------------------------------------------

### Teste de Breusch-Pagan ---------------------------------------------------------------------------------------------------------------

# testar o pressuposto da independência dos resíduos 
# possui como hipótese nula que “as variâncias de erro são todas iguais” e 
# como hipótese alternativa que “as variâncias de erro são uma função multiplicativa de uma ou mais variáveis.”

library(lmtest)
lmtest::bptest(modelo_simples)
# Note que o p-valor do Teste de Breusch-Pagan para o modelo_simples é 0.6, 
# demonstrando fortes evidências em favor da não-rejeição da hipótese nula de dependência dos resíduos.


### Teste de Durbin-Watson ---------------------------------------------------------------------------------------------------------------

# para detectar a presença de autocorrelação dos resíduos de um modelo de regressão e 
# testa o pressuposto da homogeneidade de variância dos resíduos.

# hipótese nula que os “erros são serialmente não correlacionados.” 


lmtest::dwtest(modelo_simples)
# Note que o p-valor do Teste de Durbin-Watson para o modelo_simples é menor que 0.05, indicando a rejeição da hipótese nula de 
# não-correlação entre os resíduos, violando o pressuposto da homogeneidade de variância dos resíduos.



### Multicolinearidade ------------------------------------------------------------------------------------------------------------------

# Multicolinearidade é a ocorrência de alta correlação entre duas ou mais variáveis independentes e pode levar a resultados distorcidos.

# Em geral, a multicolinearidade pode fazer com que os intervalos de confiança se ampliem, ou até mudar o sinal de influência das 
# variáveis independentes (de positivo para negativo, por exemplo). 

# Portanto, as inferências estatísticas de uma regressão com multicolinearidade não são confiáveis.

## Fator de Inflação de Variância (Variance Inflation Factor – VIF)
# Os VIFs medem o quanto da variância de cada coeficiente de regressão do modelo estatístico se encontra inflado em relação à situação 
# em que as variáveis independentes não estão correlacionadas.

# Valores aceitáveis de VIF são menores que 10.

library(car)
car::vif(modelo_simples)

# Note que os valores de VIFs para as variáveis independentes do modelo_simples estão todos dentro do limite aceitável (<10), 
# demonstrando ausência de multicolinearidade e evidenciando que o pressuposto não foi violado.



### R^2 e R^2 ajustado ---------------------------------------------------------------------------------------------------------------
# coeficiente de determinação representa a proporção da variabilidade na variável dependente prevista* pelas variáveis independentes.
#* Muitas definições usam o termo “explicada” o que evitamos pois implica intuitivamente em uma relação causal.

# É uma métrica que quantifica o poder preditivo de um modelo de regressão.

# R^2 ajustado inclui uma pequena penalidade pelo número de variáveis independentes usada no modelo de regressão. 

summary(modelo_simples)

# Veja que o modelo_simples possui como R2 um valor de 0.8267855: isto quer dizer que o modelo_simples possui um poder preditivo 
# de 83% da variância de mpg.




# Coeficientes Padronizados ------------------------------------------------------------------------------------------------------------

# Além de coeficientes brutos, podemos também obter coeficientes padronizados por desvios padrões de regressão linear usando 
# a biblioteca {lm.beta}

# Tal interpretação é vantajosa quando temos variáveis na regressão que possuem medidas diversas e que a comparação não seria tão simples.

# Os coeficientes padronizados são disponibilizados na coluna Standardized.

library(lm.beta)
modelo_simples_padronizado <- lm.beta(modelo_simples)
summary(modelo_simples_padronizado)

# A interpretação de coeficientes padronizados é quase a mesma que coeficientes padrões, apenas mudando a escala de comparação, 
# que deve ser interpretada em escala padronizada em desvios padrões: 

# "o acréscimo de 1 unidade de desvio padrão variável independente gera o aumento de <coeficiente> unidade(s) de desvio padrão(ões) 
# da variável dependente."

# No nosso exemplo, a cada acréscimo de 1 desvio padrão de hp, mpg reduz em -0.3614507 desvio padrão; e a cada acréscimo de 1 
# desvio padrão wt, mpg reduz em -0.6295545 desvio padrão.

confint(modelo_simples_padronizado)


# Conexões com teste-t ANOVA e correlações -----------------------------------------------------------------------------------------------

# https://lindeloev.github.io/tests-as-linear/


# Técnicas Avançadas de Regressão Linear -----------------------------------------------------------------------------------------------

## Regressão Robusta -----------------------------------------------------------------------------------------------
# Regressão linear não é uma boa alternativa na presença de observações extremas (também chamadas de outliers). 

# O pressuposto de que os erros (ou resíduos) são distribuidos conforme uma distribuição Normal com média 0 faz com que as 
# estimativas de uma regressão linear fiquem instáveis. 

# Para exemplificar isso, vamos fazer uma simulação com 50 observações sendo que 40 observações são distribuídas como 
# uma distribuição Normal e 10 observações são extremas (estão além de dois desvios padrões – ±2×σ).

n_sims <- 50

sims <- tibble(
  x = 1:n_sims,
  y = c(rnorm(floor(4 * n_sims / 5)), sample(c(-4:-3, 3:4), ceiling(n_sims / 5), 1)),
  tipo = c(rep("normal", floor(4 * n_sims / 5)), rep("extrema", ceiling(n_sims / 5)))
) %>%
  sample_frac(1L) 

sims %>%
  ggplot(aes(y, fill = tipo)) +
  geom_dotplot(alpha = 0.5) +
  stat_function(fun = dnorm,
                args = list(mean = mean(sims$y), sd = sd(sims$y)),
                aes(color = "Distribuição\nNormal"), size = 3) +
  stat_function(fun = dt,
                args = list(df = 3),
                aes(color = "Distribuição t\nde Student"), size = 3) +
  scale_fill_brewer("Tipo de Observação", palette = "Set1",
                    guide = guide_legend(ncol = 1, nrow = 2, byrow = TRUE)) +
  scale_colour_brewer("Tipo de Distribuição", palette = "Set3",
                      guide = guide_legend(ncol = 1, nrow = 2, byrow = TRUE)) +
  theme(legend.position = "bottom") +
  ylim(c(0, 0.4))

# Essa mostra um diagrama de pontos (dotplot) da simulação e duas distribuições estimadas com os dados: 
# Normal e t de Student (graus de liberdade = 3). Devido às observações extremas, a distribuição Normal, para 
# comportar todas as observações, se alarga e achata. Tal achatamento e alargamento não ocorrem na distribuição t de Student. 
# Isto se traduz em estimativas mais estáveis dos coeficientes da regressão na presença de observações extremas. 
# Por esses motivos, para nós, a melhor a maneira de aplicar um modelo de regressão na presença de observações extremas é 
# por meio de Estatística Bayesiana usando uma distribuição t de Student como o “motor” de inferência.

# Existem duas maneiras de realizar a regressão de maneira não bayesiana:

## M-estimação: ##
# robusta à observações extremas na variável dependentes, mas não nas independentes.

## Mínimos quadrados aparados: ##
# robusto tanto à observações extremas na variável dependente quanto nas independentes. 
# O método recomendado atualmente por diversas fontes.



## Regressão Regularizada -----------------------------------------------------------------------------------------------
# A regressão regularizada é um tipo de regressão em que as estimativas dos coeficientes são restritas a zero. 
# A magnitude (tamanho) dos coeficientes, bem como a magnitude do termo de erro, são penalizados. 
# Modelos complexos são desencorajados, principalmente para evitar overfitting.


## Tipos de regularização:
# Para usar modelos de regressão regularizada use a biblioteca {glmnet}

### Ridge ---------------------------------------------------------------------------------------------------------------
# é uma forma de criar um modelo parcimonioso quando o número de variáveis preditoras em um conjunto excede o número de 
# observações (m>p) ou quando um conjunto de dados tem forte multicolinearidade (correlações entre variáveis preditoras). 

# A regressão Ridge pertence ao conjunto de ferramentas de regularização L2. 
# A regularização L2 adiciona uma penalidade chamada penalidade L2, que é igual ao quadrado da magnitude dos coeficientes. 

# Todos os coeficientes são reduzidos pelo mesmo fator, de modo que todos os coeficientes permanecem no modelo.

# A força do termo de penalidade é controlada por um parâmetro de ajuste. 
# Quando este parâmetro de ajuste (λ) é definido como zero, a regressão Ridge é igual à regressão linear. 
# Se λ=∞, todos os coeficientes são reduzidos a zero. A penalidade ideal é, portanto, algo entre 0 e ∞.



### Lasso ---------------------------------------------------------------------------------------------------------------
# A regressão Lasso (least absolute shrinkage and selection operator – Lasso) é um tipo de regressão linear que usa encolhimento (shrinkage).

# Encolhimento faz com os valores dos coeficientes sejam reduzidos em direção a um ponto central, como a média. 
# Este tipo de redução é muito útil quando você tem altos níveis de muticolinearidade ou quando deseja automatizar 
# certas partes da seleção de modelo, como seleção de variável / eliminação de parâmetro. 

# Lasso usa a regularização L1 que limita o tamanho dos coeficientes adicionando uma penalidade L1 igual ao valor absoluto, 
# ao invés do valor quadrado como L2, da magnitude dos coeficientes.

# Isso às vezes resulta na eliminação de alguns coeficientes completamente, o que pode resultar em modelos esparsos e seleção de variáveis.



## Regressão Aditiva - Modelos Aditivos Generalizados  -----------------------------------------------------------------------

# Em estatística, um modelo aditivo generalizado (generalized additive model – GAM) é um modelo linear generalizado no qual a 
# variável de resposta depende linearmente de funções suaves (chamadas de splines) desconhecidas de algumas variáveis preditoras, 
# e o interesse se concentra na inferência sobre essas funções suaves.

# Para usar GAMs no R use a biblioteca {gam}


## Regressão Multinível -----------------------------------------------------------------------------------------------

# Modelos multiníveis (também conhecidos como modelos lineares hierárquicos, modelo linear de efeitos mistos, modelos mistos, 
# modelos de dados aninhados, coeficiente aleatório, modelos de efeitos aleatórios, modelos de parâmetros aleatórios ou designs 
# de gráfico dividido) são modelos estatísticos de parâmetros que variam em mais de um nível.

# Modelos multiníveis são particularmente apropriados para projetos de pesquisa onde os dados dos participantes são organizados 
# em mais de um nível (ou seja, dados aninhados). As unidades de análise geralmente são indivíduos (em um nível inferior) que 
# estão aninhados em unidades contextuais / agregadas (em um nível superior).

# Modelos multiníveis geralmente se dividem em três abordagens:

## Random intercept model: ##
# Modelo no qual cada grupo recebe uma constante (intercept) diferente;

## Random slope model: ##
# Modelo no qual cada grupo recebe um coeficiente diferente para cada variável independente;

## Random intercept-slope model: ##
#Modelo no qual cada grupo recebe tanto uma constante (intercept) quanto um coeficiente diferente para cada variável independente.

# Para usar modelos multiníveis em R use a biblioteca {lme4}

