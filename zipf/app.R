library(shiny)
library(tidyverse)
library(pdftools)

# Funções usadas na aplicação -------------------------------------------------
#### Função para limpar o texto
limpar_texto <- function(texto){
    
    
    # Limpar texto e transformar em tabela
    texto_limpo <- texto %>% 
        tolower() %>%                           # letras minúsculas
        str_replace_all('[:punct:]', ' ') %>% # detecta e retira pontuações [!"#$%&’()*+,-./:;<=>?@[]^_`{|}~]
        str_replace_all('[:digit:]', ' ') %>% # detecta e retira caracteres numéricos [0-9]
        str_replace_all('[:space:]', ' ') %>% # detecta e retira [Space, tab, vertical tab, newline, form feed, carriage return]
        str_squish() %>%                        # retira espaços emexcesso
        abjutils::rm_accent()                   # função que retira os acentos
    
    
    # Tranformar em tabela
    df_texto <- as.data.frame(texto_limpo)
    
    # empilhar as palavras
    df_longo <- df_texto %>% 
        separate(col = texto_limpo, into = paste('q_', c(1:10000)), sep = ' ') %>% 
        gather() %>% 
        select(value) %>% 
        filter(complete.cases(value)) %>% 
        mutate(value = as.factor(value))
    
    # Ordenar as palavras por frequência
    df_ordem <- df_longo %>% 
        group_by(value) %>% 
        summarise(
            n = n()
        ) %>% 
        arrange(desc(n))
    
    # Criar uma coluna com o ranking da frequência das palavras
    rank <- c(1:length(df_ordem$value))
    
    # Juntar o rank com a tabela de ordem
    df_completo <- cbind(df_ordem, rank)
    
    df_completo
    
}

#### Função para criar um gráfico
plot_entropia <- function(tabela){
    
    library(tidyverse)
    
    # Criar o gráfico da entropia
    grafico <- tabela %>% 
        ggplot(aes(x = log(rank), y = log(n))) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE) +
        theme_minimal() +
        ggtitle("Curva entrópica da frequência das palavras - Zipf")
    
    grafico
}

#### Função para analizar a entropia
analise <- function(tabela){
    
    # Criar modelo de regressão 
    modelo <- lm(log(rank) ~ log(n), data = tabela)
    
    # valor final
    paste("Coeficiente angular:", round(modelo$coefficients[2], 4))
    
    # # Diferença
    # paste(round(abs(modelo$coefficients[2])-1.048, 2))
    
}

# Ui --------------------------------------------------------------------------
ui <- fluidPage(
    tabPanel(title = "Clipboard",
             
             titlePanel("Estatística Zipf"),
             p("@caiosainvallio"),
             
             sidebarLayout(
                 
                 sidebarPanel(
                     textInput(inputId = "texto", label = "Cole o texto a ser entropisado: "),
                     br(),
                     hr(),
                     strong("Resultado: "),
                     p("O valor esperado deve ser próximo a -1.048"),
                     verbatimTextOutput(outputId = "analise")
                 ),
                 
                 mainPanel(
                     tabsetPanel(
                         tabPanel(title = "Gráfico", 
                                  fluidRow(br()),
                                  plotOutput("grafico")), 
                         tabPanel(title = "Tabela", 
                                  column(tableOutput("tabela1"), width = 6),
                                  column(tableOutput("tabela2"), width = 6))
                     )
                 )
             )
    )
    
)

# server ----------------------------------------------------------------------
server <- function(input, output) {
    
    output$analise <- renderText({
        tabela <- limpar_texto(input$texto)
        analise(tabela)
    })
    
    output$grafico <- renderPlot({
        tabela <- limpar_texto(input$texto)
        plot_entropia(tabela)
    })
    
    output$tabela1 <- renderTable({
        tabela <- limpar_texto(input$texto)
        head(tabela, 15)
    })
    
    output$tabela2 <- renderTable({
        tabela <- limpar_texto(input$texto)
        tabela %>% 
            arrange(rank) %>% 
            top_n(15)
    })
    
}

# app ------------------------------------------------------------------------
shinyApp(ui = ui, server = server)




