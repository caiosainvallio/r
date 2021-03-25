# Tamanho da amostra 
library(tidyverse)


# Tamanho de Efeito ------------------------------------------------------------------------------------------------------------
# Tamanho de efeito é definido como “um número que mede a força da relação entre duas variáveis em uma população 
# estatística ou uma estimativa baseada em amostra dessa quantidade.

# Pode referir-se ao valor de uma estatística calculada a partir de uma amostra, ao valor de um parâmetro de uma população 
# estatística hipotética ou à equação que operacionaliza como as estatísticas ou parâmetros levam ao valor do tamanho do efeito”.

# Exemplos de tamanho de efeito incluem a correlação entre duas variáveis, o coeficiente de uma variável em uma regressão 
# e a diferença média entre grupos distintos.

# Os tamanhos de efeito complementam o teste de hipótese estatística e desempenham um papel importante nas análises 
# de poder de um teste estatístico e planejamento do tamanho da amostra.

# Os tamanhos de efeito podem ser medidos em termos relativos ou absolutos. 

##  Em tamanhos de efeito relativos, dois grupos são comparados diretamente um com o outro, como em razão de probabilidades 
# (odds ratio – OR) e riscos relativos. 

## Para tamanhos de efeito absolutos, um valor absoluto maior sempre indica um efeito mais forte. 

# Muitos tipos de medidas podem ser expressos como absolutos ou relativos e podem ser usados em conjunto 
# porque transmitem informações diferentes. 
# Se atentem ao contexto e quando usar medidas absolutas versus relativas.

# Efeitos pequenos, médios e grandes ----------------------------------------------------------------------------------------

# A principal referência em tamanhos de efeito é Cohen (1988) que distingue tamanhos de efeito em pequeno, médio e grande; 
# mas note que é necessário cautela: “Os termos ‘pequeno,’ ‘médio’ e ‘grande’ são relativos, não apenas uns aos outros, 
# mas à área da ciência do qual pertencem ou, ainda mais particularmente, ao conteúdo específico e ao método de pesquisa 
# empregado em qualquer investigação…”

# Os tamanho de efeito para as diferentes métricas estatística dos testes de hipótese são definidos em Cohen (1988), 
# mas mesmo assim eles podem variar conforme área da ciência e contexto, portanto o pesquisador deve sempre justificar 
# (preferencialmente com argumentos baseado em evidências) caso escolha usar as métricas padrões ou caso precise alterá-las.

# Neste tutorial cobriremos três principais tipos de efeito: d de Cohen, r de Pearson e f2 de Cohen. 
# Esses tipos de efeitos são os usados em testes t, ANOVA, correlação e regressão linear.

# d de Cohen ----------------------------------------------------------------------------------------------------------------
# O d (difference) de Cohen (Cohen, 1988) é a diferença entre a média de dois grupos padronizada por desvios padrões:

## d = mu1-mu2/sigma

# onde mu1 é a média de um grupo, mu2 média do outro grupo e sigma o desvio padrão com base em ambos os grupos.


# d de Cohen é usado em testes de diferença de média como o teste t e teste de Tukey.
# Os tamanhos de efeito para d de Cohen são:
  
## Pequeno: d=0.2 -------------------------------------------------------------
pwr::cohen.ES(test = 'p', size = 'small')

ggplot()  + 
  xlim(-2.5, 5) +
  geom_function(fun = dnorm, args = list(mean = 1, sd = 1), color = 'red') +
  geom_function(fun = dnorm, args = list(mean = 1.2, sd = 1)) + 
  geom_vline(xintercept = 1, color = 'red', linetype="dotted") + 
  geom_vline(xintercept = 1.2, linetype="dotted") +
  labs(y=NULL, x='Tamanho de Efeito (d de Cohen = 0.2)')

## Médio: d=0.5 --------------------------------------------------------------
pwr::cohen.ES(test = 'p', size = 'medium')

ggplot()  + 
  xlim(-2.5, 5) +
  geom_function(fun = dnorm, args = list(mean = 1, sd = 1), color = 'red') +
  geom_function(fun = dnorm, args = list(mean = 1.5, sd = 1)) + 
  geom_vline(xintercept = 1, color = 'red', linetype="dotted") + 
  geom_vline(xintercept = 1.5, linetype="dotted") +
  labs(y=NULL, x='Tamanho de Efeito (d de Cohen = 0.5)')

## Grande: d=0.8 --------------------------------------------------------------
pwr::cohen.ES(test = 'p', size = 'large')

ggplot()  + 
  xlim(-2.5, 5) +
  geom_function(fun = dnorm, args = list(mean = 1, sd = 1), color = 'red') +
  geom_function(fun = dnorm, args = list(mean = 1.8, sd = 1)) + 
  geom_vline(xintercept = 1, color = 'red', linetype="dotted") + 
  geom_vline(xintercept = 1.8, linetype="dotted") +
  labs(y=NULL, x='Tamanho de Efeito (d de Cohen = 0.8)')




# r de Pearson ----------------------------------------------------------------
# Os tamanhos de efeito para r de Pearson são (Cohen, 1988):

x <- 1:50
y <- rnorm(50, sd = 10)

complemento <- function(y, correlação, x) {
  y.perp <- residuals(lm(x ~ y))
  correlação * sd(y.perp) * y + y.perp * sd(y) * sqrt(1 - correlação^2)
}

## Pequeno: r=0.1 -----------------------------------------------------------------------------
pwr::cohen.ES(test = 'r', size = 'small')

correlação.1 <- 0.1
fig.1 <- data.frame(z = as.vector(sapply(correlação.1,
                                     function(correlação.1) complemento(y, correlação.1, x))),
                correlação.1 = ordered(rep(signif(correlação.1, 2),
                                         each = length(y))),
                y = rep(y, length(correlação.1))) %>% 
  ggplot(aes(y, z, group = correlação.1)) +
    geom_rug(sides = "b") +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", color = "Red", se = FALSE) +
    facet_wrap(~ correlação.1, scales = "free", labeller = "label_both", ncol = 4) +
    theme(legend.position = "none")

fig.1


## Médio: r=0.3 ---------------------------------------------------------------------------------
pwr::cohen.ES(test = 'r', size = 'medium')

correlação.3 <- 0.3
fig.3 <- data.frame(z = as.vector(sapply(correlação.3,
                                     function(correlação.3) complemento(y, correlação.3, x))),
                correlação.3 = ordered(rep(signif(correlação.3, 2),
                                         each = length(y))),
                y = rep(y, length(correlação.3))) %>% 
  ggplot(aes(y, z, group = correlação.3)) +
    geom_rug(sides = "b") +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", color = "Red", se = FALSE) +
    facet_wrap(~ correlação.3, scales = "free", labeller = "label_both", ncol = 4) +
    theme(legend.position = "none")

fig.3


## Grande:  r=0.5 --------------------------------------------------------------------------------
pwr::cohen.ES(test = 'r', size = 'large')

correlação.5 <- 0.5
fig.5 <- data.frame(z = as.vector(sapply(correlação.5,
                                     function(correlação.5) complemento(y, correlação.5, x))),
                correlação.5 = ordered(rep(signif(correlação.5, 2),
                                         each = length(y))),
                y = rep(y, length(correlação.5))) %>% 
  ggplot(aes(y, z, group = correlação.5)) +
    geom_rug(sides = "b") +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", color = "Red", se = FALSE) +
    facet_wrap(~ correlação.5, scales = "free", labeller = "label_both", ncol = 4) +
    theme(legend.position = "none")

fig.5



# f^2  DE COHEN -------------------------------------------------------------------------------------------------------------------
# O f^2 de Cohen é usado em contextos de testes estatísticos que usam a distribuição F de Fisher. 
# Em ANOVAs e regressão linear, o f^2 é a proporção da variabilidade na variável dependente prevista pelas variável 
# independentes e indica o poder preditivo de um modelo estatístico. Em especial, para regressões lineares, o f^2 pode 
# ser usado como indicador de poder preditivo de um modelo completo R^2 ou como indicador de poder preditivo (e poder de 
# influência) de uma variável independente sobre uma dependente (aqui o f^2 equivale ao coeficiente padronizado em desvios padrões).

# Os tamanhos de efeito para f^2 de Cohen (Cohen, 1988) são:
  
## ANOVA: -----------------------------------------------------
### Pequeno: f2=0.1 -------------------------------------------
pwr::cohen.ES(test = 'anov', size = 'small')

### Médio: f2=0.25 --------------------------------------------
pwr::cohen.ES(test = 'anov', size = 'medium')

### Grande: f2=0.4 --------------------------------------------
pwr::cohen.ES(test = 'anov', size = 'large')


## Regressão Linear: ------------------------------------------
### Pequeno: f2=0.02 ------------------------------------------
pwr::cohen.ES(test = 'f2', size = 'small')

### Médio: f2=0.15 --------------------------------------------
pwr::cohen.ES(test = 'f2', size = 'medium')

### Grande: f2=0.35 -------------------------------------------
pwr::cohen.ES(test = 'f2', size = 'large')




# Tamanho da amostra -------------------------------------------------------------------------------------------------------------------

# O tamanho de amostra é influenciado diretamente pelo erro tipo I e erro tipo II.

## Erro tipo I, também chamado de “falso positivo” ##
# é a chance de rejeitarmos a hipótese nula quando ela é verdadeira. Esse erro é o alpha α que é usado como 
# limiar de significância do p-valor.

## Erro tipo II, também chamado de “falso negativo” ##
# é a chance de não rejeitarmos a hipótese nula quando ela é falsa. Esse erro é identificado como a letra grega beta β. 
# Além disso, o poder de um teste estatístico é mensurado como 1−β. O poder de um teste estatístico aumenta proporcionalmente 
# ao tamanho amostral. Quanto maior a amostra, maior o poder do teste.


# Esses dois tipos de erros foram cunhados por Jerzy Neyman, fundador do paradigma NHST [Null Hypothesis Significance Testing 
# – NHST (tradução: teste de significância de hipótese nula)], que defendia a ideia de que é melhor absolver um culpado 
# (erro tipo II – falso negativo) do que culpar um inocente (erro tipo I – falso positivo):


# “É mais sério condenar um homem inocente ou absolver um culpado? Isso dependerá das consequências do erro. A punição é morte 
# ou multa? Qual é o risco de criminosos libertados para a sociedade ? Quais são os pontos de vista éticos atuais sobre punição? 
# Do ponto de vista da teoria matemática, tudo o que podemos fazer é mostrar como o risco de erros pode ser controlado e minimizado. 
# O uso dessas ferramentas estatísticas em qualquer caso específico para determinar como o equilíbrio deve ser alcançado , deve 
# ser deixado para o investigador.” 


# Calculando Tamanho de Amostra no R -----------------------------------------------------------------------


# Para calcular um tamanho de amostra no R usaremos a biblioteca {pwr}.
# As funções disponíveis são:

## pwr.t.test() – Teste t para Amostras Independentes e para duas Amostras Pareadas
## pwr.r.test – Correlação usando o r de Pearson
## pwr.anova.test – ANOVA Unidirecional
## pwr.f2.test – Regressão Linear

# A lógica do {pwr} é a seguinte. Cada uma dessas função possui argumentos para:

## 1. Tamanho de Efeito – d, r ou f2
## 2. Tamanho de Amostra – n
## 3. Probabilidade do erro tipo I – falso positivo – α – sig.level padrão 0.05
## 4. Poder Estatístico – 1 menos a probabilidade do erro tipo II – falso negativo – 1−β – power

# Como esses quatro conceitos são interdependentes (para o cálculo de um é necessário saber o valor dos outros três), 
# você deve especificar três dessas quatro métricas para obter o resultado da métrica desejada.


# Exemplo Regressão Linear -----------------------------------------------------------------------

# Aqui o uso da função pwr.f2.test() é um pouco mais complicado porque, além do tamanho de efeito f2, sig.level, 
# n e power, temos que especificar os graus de liberdade do numerador u e graus de liberdade do denominador v.

## Os graus de liberdade do numerador, u ##
# é o número de coeficientes que você terá em seu modelo (menos a constante): u = coeficientes − 1. 

## Os graus de liberdade do denominador, v ##
# é o número de graus de liberdade do erro do modelo. Você pode calcular os graus de liberdade do denominador subtraindo
# o número de observações de amostra do número total de coeficientes exceto a constante menos 1. Então v = n − u − 1.

# Suponha que você queira saber o tamanho de amostra necessário para uma regressão linear com cinco variáveis independentes 
# detectar efeitos grandes, f2=0.35, com taxa de falso positivo (α) de 5%, e poder estatístico (1−β) de 80%. Aqui temos u = 5,
# pois são seis coeficientes (contando com a constante) menos a constante (6−1=5).

pwr::pwr.f2.test(f2 = 0.35, u = 5, sig.level = 0.05, power = 0.80)

# Veja que o resultado não nos informa um tamanho amostral, mas sim os graus de liberdade do denominador: v. Lembre-se que v=n−u−1, 
# então n=v+u+1. Portanto nosso tamanho amostral é 43. Isto quer dizer que para identificar efeitos grandes, de no mínimo f2=0.35, 
# tanto para R2 quanto para influências das variáveis independentes (em coeficientes padronizados por desvios padrões), com α em 5% 
# e poder estatístico de 80%, é necessário no mínimo uma amostra com 43 observações.


