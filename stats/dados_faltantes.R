# Dados faltantes ##
library(tidyverse)

# O que são dados faltantes? ----------------------------------------------------------------------------------------------------------

# Os valores ausentes podem ser de três tipos gerais:

## Totalmente ausente de maneira aleatória (missing completely at random – MCAR): ##
# Quando os dados ausentes são MCAR, a presença / ausência de dados é completamente independente das variáveis observáveis e dos 
# parâmetros de interesse. Nesse caso, as análises realizadas nos dados são imparciais. Na prática, é altamente improvável. Pode 
# haver perda de potência das técnicas estatísticas, mas os parâmetros estimados não são influenciados pela ausência de dados.

## Ausente de maneira aleatória (missing at random – MAR): ##
# Quando os dados ausentes não são aleatórios, mas podem ser totalmente relacionados a uma variável onde há informações completas. 
# Um exemplo 1 é que os homens têm menos probabilidade de preencher uma pesquisa sobre depressão, mas isso não tem nada a ver com 
# seu nível de depressão, após levar em conta a masculinidade. Esse tipo de dados ausentes pode induzir um viés em sua análise, 
# especialmente se desequilibrar seus dados por causa de muitos valores ausentes em uma determinada categoria.

## Ausente de maneira não-aleatória (missing not at random – MNAR): ##
# Quando os valores ausentes não são MCAR nem MAR. No exemplo anterior, esse seria o caso se as pessoas tendessem a não responder à 
# pesquisa dependendo de seu nível de depressão. Os casos de dados MNAR são problemáticos. A única maneira de obter uma estimativa 
# imparcial dos parâmetros em tal caso é modelar os dados ausentes. O modelo pode então ser incorporado em um mais complexo para 
# estimar os valores ausentes.



# Como testar para dados faltantes? --------------------------------------------------------------------------------------------------
# Um teste muito usado é o Teste de MCAR de Little que possui como hipótese nula:
# “os dados faltantes são totalmente ausentes de maneira aleatória (MCAR)”.

# Ou seja, um p-valor pequeno (p<0.05) é geralmente interpretado como evidências contrárias que os dados são MCAR. 
# O teste é baseado em um teste de chi-quadrado sobre frequências esperadas vs frequências observadas.

# A biblioteca {naniar} possui a função mcar_test() que implementa o teste de MCAR de Little. 
# Além disso, possui diversas outras funcionalidades para sumarizar, visualizar e manipular com dados faltantes. 

# Caso queira incorporar na sua análise recomendamos ler o manual da biblioteca. [http://naniar.njtierney.com/index.html]


# O que fazer com dados faltantes? --------------------------------------------------------------------------------------------------

# O melhor método possível de lidar com os dados ausentes é evitar o problema planejando bem o estudo e coletando os dados cuidadosamente. 
# Caso isso não seja possível, temos duas abordagens básicas para lidar com dados faltantes:


## Remover dados faltantes ----------------------------------------------------------------------------------------------------------
# A remoção de dados faltantes se divide em duas principais abordagens usando a função na.omit() padrão do R:

## remoção de observações com dados faltantes: ##
# aqui removemos as linhas com dados faltantes 
# df <- na.omit(df)

### remoção de variáveis com dados faltantes: ##
# aqui removemos as colunas com dados faltantes 
# df <- t(na.omit(t(df)))


## Imputar valores nos dados faltantes ----------------------------------------------------------------------------------------------

# A imputação de dados é um método aplicado para imputar um valor para cada item ausente. 
# Imputações de dados simples podem ser definidas como médias ou extrações de uma distribuição preditiva de valores ausentes, 
# requerem um método de criação de uma distribuição preditiva para imputação com base nos dados observados e definem duas 
# abordagens genéricas para gerar tal distribuição preditiva: modelagem explícita e modelagem implícita.

# Na modelagem explícita, a distribuição preditiva é baseada em um modelo estatístico formal, por exemplo, normal multivariado,
# portanto, as suposições são explícitas. Exemplos de modelagem explícita são imputação de média, imputação de regressão, 
# imputação de regressão estocástica.

# Na modelagem implícita 3 , o foco está em um algoritmo, o que implica um modelo subjacente. Suposições estão implícitas, 
# mas ainda precisam ser avaliadas com cuidado para garantir que sejam razoáveis.

# Dentre as diversas maneiras de imputar valores ao dados faltantes, as mais comuns são três:

## imputar a média.
## imputar a mediana.
## imputar o último valor ocorrido (last observation carried forward – LOFC): muito usada em séries temporais.

# Mas ainda há maneiras mais avançadas e que desempenham melhor em certas condições:

## Imputação por k-vizinhos mais próximos (k-nearest neighbors imputation)
## Imputação por florestas aleatórias (random forest imputation)

# A biblioteca DescTools é uma coleção de funções focadas especialmente na parte descritiva de análise de um dataset. 
# Inclusive a função Impute() – imputar valores em dados faltates usando qualquer função do R – e a função LOCF() que permite 
# imputar o último valor ocorrido.

# Para mostrar as abordagens, geramos um dataset de uma série temporal de uma semana com dados faltantes:

library(DescTools)
set.seed(123)
df <- data.frame(
  dia = c("seg", "ter", "qua", "qui", "sex", "sab", "dom"),
  valor = runif(7))
indices_aleatorios <- sample(1:nrow(df), 2)
df[indices_aleatorios[1], 2] <- NA
df[indices_aleatorios[2], 2] <- NA

# Teste MCAR de Little --------------------------------------------------------------------------------------------------------

mcar_test <- function(data) {
  
  # test for dataframe
  if (!inherits(data, "data.frame")) {
    stop("Input must inherit from data.frame", call. = FALSE)
  }
  
  # norm::prelim.norm needs to work with a data.matrix
  data <- data.matrix(data)
  
  # Number of variables in data
  n_var <- ncol(data)
  
  # Number of rows
  n <- nrow(data)
  var_names <- colnames(data)
  
  # Calculate pattern of missingness for each row
  r <- 1 * is.na(data)
  mdp <- (r %*% (2^((1:n_var - 1)))) + 1
  
  # Add pattern as column to original data
  x_miss_pattern <- data.frame(cbind(data, mdp))
  colnames(x_miss_pattern) <- c(var_names, "miss_pattern")
  
  # Number of unique missing data patterns
  n_miss_pattern <- length(unique(x_miss_pattern$miss_pattern))
  
  # Recode miss_pattern variable to go from 1 through n_miss_pattern
  x_miss_pattern <- x_miss_pattern %>%
    dplyr::mutate(miss_pattern = dplyr::dense_rank(.data$miss_pattern))
  
  # Maximum likelihood estimation from {norm}
  # Adapted from Eric Stemmler
  # https://stats-bayes.com/post/2020/08/14/r-function-for-little-s-test-for-data-missing-completely-at-random/
  s <- norm::prelim.norm(data)
  ll <- norm::em.norm(s, showits = FALSE)
  fit <- norm::getparam.norm(s = s, theta = ll)
  grand_mean <- fit$mu
  grand_cov <- fit$sigma
  colnames(grand_cov) <- rownames(grand_cov) <- colnames(data)
  
  little_calculations <- x_miss_pattern %>%
    # For each of the types of missing patterns...
    dplyr::group_by(.data$miss_pattern) %>%
    tidyr::nest() %>%
    # kj terms for degrees of freedom
    dplyr::mutate(
      kj = purrr::map_dbl(.data$data,
                          ~colSums(as.matrix(1 * !is.na(colSums(.x)))))
    ) %>%
    # Calculate column averages
    dplyr::mutate(
      mu = purrr::map(.data$data, ~colMeans(.x) - grand_mean),
      mu = purrr::map(.data$mu, ~.x[!is.na(.x)])
    ) %>%
    # Binary 0/1 indicating if column should be kept
    dplyr::mutate(
      keep = purrr::map(.data$data, ~1 * !is.na(colSums(.x))),
      keep = purrr::map(.data$keep, ~.x[which(.x[1:n_var] != 0)])
    ) %>%
    # Drop rows and columns from global covariance matrix so that the matrix
    # only contains rows and columns that exist in current missing pattern
    dplyr::mutate(
      sigma = purrr::map(.data$keep,
                         ~grand_cov[which(rownames(grand_cov) %in% names(.x)),
                                    which(colnames(grand_cov) %in% names(.x))])
    ) %>%
    # Finally calculate Little's statistic using the number of rows in missing
    # pattern, average, and covariance
    dplyr::mutate(
      d2 = purrr::pmap_dbl(
        list(.data$data, .data$mu, .data$sigma, .data$kj),
        ~ifelse(..4 == 0,
                0,  # If the pattern is all NA, use 0
                nrow(..1) * (t(..2) %*% solve(..3) %*% ..2))
      )) %>%
    dplyr::ungroup()
  
  # Main calculations
  d2 <- sum(little_calculations$d2)  # Little's d2
  df <- sum(little_calculations$kj) - n_var  # Degrees of freedom
  p_value <- 1 - stats::pchisq(d2, df)  # p-value
  
  # Return everything as a glance-like tibble
  tibble::tibble(statistic = d2, df = df, p.value = p_value,
                 missing.patterns = n_miss_pattern)
}


mcar_test(df)

# O teste retornou um p-valor de 0.4027837, e não conseguimos rejeitar a H0 de que “os dados faltantes são totalmente 
# ausentes de maneira aleatória (MCAR).” Notem que o nosso dataset possui apenas 7 observações e por isso o teste não 
# possui poder o suficiente para rejeitar a hipótese nula caso ela seja falsa.

## IMPUTAR A MÉDIA ------------------------------------------------------------------------------------------------------
df$media <- Impute(df$valor, FUN = mean(df$valor, na.rm = TRUE))

## IMPUTAR A MEDIANA ----------------------------------------------------------------------------------------------------
df$mediana <- Impute(df$valor, FUN = median(df$valor, na.rm = TRUE))

## IMPUTAR O ÚLTIMO VALOR OCORRIDO – LOCF -------------------------------------------------------------------------------
df$LOCF <- LOCF(df$valor)

# COMPARAÇÃO DOS RESULTADOS -------------------------------------------------------------------------------
library(skimr)
library(gt)
df %>% gt() %>% tab_header("Dados Faltantes usando as diferentes Técnicas de Imputação")

skim(df)


