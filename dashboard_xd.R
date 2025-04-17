library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(DT)
library(wordcloud2)
library(tm)
library(shinythemes)
library(readxl)
library(dplyr)
library(tidytext)
library(stringr)
library(tidyr)
library(forcats)
library(scales)
library(shinycssloaders)
# ---# ---# -----------------------------------------------------------
# Carga y preprocesamiento de datos
# -----------------------------------------------------------

load_data <- function() {
  data_path <- "D:/Universidad/Business/Base Bussiness .xlsx"
  
  tryCatch({
    base <- read_excel(data_path, sheet = "base de datos") %>%
      mutate(
        empleabilidad = factor(empleabilidad, levels = c("Si", "No")),
        universidad = as.factor(universidad),
        abreviacion_universidad=as.factor(abreviacion_universidad)
        
      )
    
    perfiles <- read_excel(data_path, sheet = "Hoja 1") %>%
      mutate(`Perfil de egreso` = as.character(`Perfil de egreso`))
    
    list(base = base, perfiles = perfiles)
  }, error = function(e) {
    showNotification("Error al cargar los datos. Verifica la ruta del archivo.", type = "error")
    list(base = data.frame(), perfiles = data.frame())
  })
}

data_list <- load_data()
base <- data_list$base
perfiles <- data_list$perfiles

# -----------------------------------------------------------
# Preprocesamiento de texto
# -----------------------------------------------------------

process_text <- function(text) {
  corpus <- Corpus(VectorSource(text)) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(removeWords, stopwords("spanish")) %>%
    tm_map(removeWords, c("de", "la", "el", "en", "y", "para", "del", "las", "los"))
  
  tdm <- TermDocumentMatrix(corpus)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m), decreasing = TRUE)
  data.frame(word = names(v), freq = v)
}

wordcloud_data <- reactive({
  texto <- paste(perfiles$`Perfil de egreso`, collapse = " ")
  process_text(texto)
})

# -----------------------------------------------------------
# UI
# -----------------------------------------------------------

ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(
    title = span(icon("graduation-cap"), "Dashboard Educativo"),
    titleWidth = 300
  ),
  dashboardSidebar(
    width = 300,
    
    sidebarMenu(
      id = "tabs",
      menuItem("Resumen", tabName = "resumen", icon = icon("dashboard")),
      menuItem("Tiempo en empleo", tabName = "tiempo", icon = icon("clock")),
      menuItem("Nube de Palabras", tabName = "nube", icon = icon("cloud")),
      menuItem("Análisis Comparativo", tabName = "multigraficos", icon = icon("chart-bar")),
      menuItem("Análisis Textual", tabName = "analisis_texto", icon = icon("language"))
    ),
    conditionalPanel(
      condition = "input.tabs == 'tiempo' || input.tabs == 'multigraficos'",
      div(style = "padding: 15px;",
          selectInput("universidad_filtro", "Filtrar por Universidad:",
                      choices = c("Todas", sort(unique(as.character(base$universidad))))),
          conditionalPanel(
            condition = "input.tabs == 'tiempo'",
            selectInput("empleabilidad_filtro", "Filtrar por Empleabilidad:",
                        choices = c("Todos", "Si", "No"))
          )
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"),
      tags$style(HTML("
        .box-title { font-weight: bold; }
        .main-header .logo { font-weight: bold; }
        .plotly-container { height: 100% !important; }
        .wordcloud2-container { margin: 0 auto; }
        .info-box { cursor: pointer; }
      "))
    ),
    
    tabItems(
      tabItem(
        tabName = "resumen",
        fluidRow(
          infoBoxOutput("total_egresados"),
          infoBoxOutput("tasa_empleabilidad"),
          infoBoxOutput("promedio_tiempo")
        ),
        fluidRow(
          box(
            title = "Porcentaje de Empleabilidad por Universidad", 
            width = 6, 
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("grafico1") %>% withSpinner()
          ),
          box(
            title = "Tabla Resumen", 
            width = 6, 
            status = "primary",
            solidHeader = TRUE,
            DTOutput("tabla1") %>% withSpinner()
          )
        )
      ),
      
      tabItem(
        tabName = "tiempo",
        fluidRow(
          box(
            title = "Distribución de Tiempo para Conseguir Empleo", 
            width = 12, 
            status = "info",
            solidHeader = TRUE,
            plotlyOutput("grafico_tiempo") %>% withSpinner(),
            footer = "Nota: El tiempo se mide en meses desde la titulación hasta la obtención del primer empleo."
          )
        ),
      ),
      
      tabItem(
        tabName = "nube",
        fluidRow(
          box(
            title = "Nube de Palabras - Perfiles de Egreso", 
            width = 12, 
            status = "success",
            solidHeader = TRUE,
            wordcloud2Output("nube_palabras", height = "600px") %>% withSpinner(),
            footer = "Palabras más frecuentes en los perfiles de egreso de los estudiantes."
          )
        )
      ),
      
      tabItem(
        tabName = "multigraficos",
        fluidRow(
          box(
            title = "Distribución de Empleabilidad", 
            width = 6, 
            status = "warning",
            solidHeader = TRUE,
            plotlyOutput("grafico_barras") %>% withSpinner()
          ),
          box(
            title = "Proporción de Empleabilidad", 
            width = 6, 
            status = "warning",
            solidHeader = TRUE,
            plotlyOutput("grafico_torta") %>% withSpinner()
          )
        )
      ),
      
      tabItem(
        tabName = "analisis_texto",
        fluidRow(
          box(
            title = "Frecuencia de Palabras Clave", 
            width = 6, 
            status = "danger",
            solidHeader = TRUE,
            plotlyOutput("frecuencia_palabras") %>% withSpinner()
          ),
          box(
            title = "Bigramas Más Frecuentes", 
            width = 6, 
            status = "danger",
            solidHeader = TRUE,
            plotlyOutput("bigrama_plot") %>% withSpinner()
          )
        )
      )
    )
  )
)

# -----------------------------------------------------------
# Server
# -----------------------------------------------------------

server <- function(input, output, session) {
  
  datos_filtrados <- reactive({
    req(input$universidad_filtro)
    req(input$empleabilidad_filtro)
    
    df <- base
    
    if (input$universidad_filtro != "Todas") {
      df <- df %>% filter(universidad == input$universidad_filtro)
    }
    
    if (input$empleabilidad_filtro != "Todos") {
      df <- df %>% filter(empleabilidad == input$empleabilidad_filtro)
    }
    
    df
  })
  
  output$total_egresados <- renderInfoBox({
    infoBox(
      "Total Egresados", 
      nrow(base), 
      icon = icon("users"),
      color = "navy"
    )
  })
  
  output$grafico1 <- renderPlotly({
    df <- base %>% 
      group_by(abreviacion_universidad) %>% 
      summarise(
        empleabilidad_pct = mean(empleabilidad == "Si") * 100,
        n = n()
      ) %>%
      mutate(abreviacion_universidad = fct_reorder(abreviacion_universidad, -empleabilidad_pct))
    
    ggplotly(
      ggplot(df, aes(x = abreviacion_universidad, y = empleabilidad_pct, 
                     text = paste("Universidad:", abreviacion_universidad, "<br>",
                                  "Empleabilidad:", round(empleabilidad_pct, 1), "%", "<br>",
                                  "Muestra:", n, "egresados"))) +
        geom_col(aes(fill = empleabilidad_pct)) +
        scale_fill_gradient(low = "#FF9999", high = "#66CC99") +
        labs(title = " ", 
             y = "% Empleabilidad", x = "Universidades") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)),
      tooltip = "text"
    ) %>% config(displayModeBar = FALSE)
  })
  
  
  output$tabla1 <- renderDT({
    df <- base %>% 
      group_by(universidad) %>% 
      summarise(
        Total = n(),
        Empleados = sum(empleabilidad == "Si"),
        No_Empleados = sum(empleabilidad == "No"),
        Porcentaje_Empleabilidad = round(mean(empleabilidad == "Si"), 2),
        Tiempo_Promedio = round(mean(`tiempo que tarda en conseguir empleo post titulacion`, na.rm = TRUE), 1)
      ) %>%
      arrange(desc(Porcentaje_Empleabilidad))
    
    datatable(
      df,
      rownames = FALSE,
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        pageLength = 5,
        autoWidth = TRUE
      ),
      colnames = c('Universidad', 'Total', 'Empleados', 'No Empleados', '% Empleabilidad', 'Tiempo Prom (meses)')
    ) %>% formatPercentage('Porcentaje_Empleabilidad', 2)
  })
  
  output$grafico_tiempo <- renderPlotly({
    validate(
      need(nrow(datos_filtrados()) > 0, "No hay datos disponibles con los filtros seleccionados")
    )
    
    ggplotly(
      ggplot(datos_filtrados(), 
             aes(x = abreviacion_universidad, 
                 y = `tiempo que tarda en conseguir empleo post titulacion`,
                 color = universidad,
                 text = paste("Universidad:", universidad, "<br>",
                              "Tiempo:", `tiempo que tarda en conseguir empleo post titulacion`, "meses"))) +
        geom_boxplot() +
        geom_jitter(alpha = 0.5, width = 0.2) +
        labs(title = " ", 
             y = "Meses", x = "") +
        theme_minimal() +
        theme(legend.position = "none"),
      tooltip = "text"
    )
  })
  
  output$nube_palabras <- renderWordcloud2({
    wordcloud2(wordcloud_data(), 
               size = 1.5, 
               color = "random-dark",
               backgroundColor = "white")
  })
  
  output$grafico_barras <- renderPlotly({
    df <- datos_filtrados() %>%
      count(empleabilidad, .drop = FALSE)
    
    ggplotly(
      ggplot(df, aes(x = empleabilidad, y = n, fill = empleabilidad,
                     text = paste("Empleabilidad:", empleabilidad, "<br>",
                                  "Cantidad:", n))) +
        geom_col() +
        scale_fill_manual(values = c("Si" = "#4DAF4A", "No" = "#E41A1C")) +
        labs(title = " ", 
             y = "Cantidad", x = "Empleabilidad") +
        theme_minimal(),
      tooltip = "text"
    ) %>% config(displayModeBar = FALSE)
  })
  
  output$grafico_torta <- renderPlotly({
    df <- datos_filtrados() %>%
      mutate(
        empleabilidad = case_when(
          empleabilidad == 1 ~ "Si",
          empleabilidad == 2 ~ "No",
          is.na(empleabilidad) ~ "NA",
          TRUE ~ as.character(empleabilidad)
        )
      ) %>%
      count(empleabilidad)
    
    # Asegurar orden y color consistente
    df$empleabilidad <- factor(df$empleabilidad, levels = c("Si", "No", "NA"))
    colores <- c("Si" = "#4DAF4A", "No" = "#E41A1C", "NA" = "#999999")
    
    plot_ly(
      df,
      labels = ~empleabilidad,
      values = ~n,
      type = 'pie',
      textinfo = 'label+percent',
      insidetextorientation = 'radial',
      marker = list(colors = colores[as.character(df$empleabilidad)]),
      hoverinfo = 'text',
      text = ~paste("Cantidad:", n)
    ) %>%
      layout(title = " ",
             showlegend = FALSE)
  })
  
  output$frecuencia_palabras <- renderPlotly({
    palabras <- perfiles %>%
      unnest_tokens(word, `Perfil de egreso`) %>%
      anti_join(stop_words, by = "word") %>%
      filter(!word %in% c("de", "la", "el", "en", "y", "para", "del", "las", "los","con","su","como","se","una","al")) %>%
      count(word, sort = TRUE) %>%
      head(15)
    
    ggplotly(
      ggplot(palabras, aes(x = reorder(word, n), y = n, 
                           text = paste("Palabra:", word, "<br>Frecuencia:", n))) +
        geom_col(fill = "steelblue") +
        coord_flip() +
        labs(title = "Palabras más frecuentes", x = "", y = "Frecuencia") +
        theme_minimal(),
      tooltip = "text"
    )
  })
  
  output$bigrama_plot <- renderPlotly({
    bigramas <- perfiles %>%
      mutate(`Perfil de egreso` = str_squish(`Perfil de egreso`)) %>%
      filter(!is.na(`Perfil de egreso`), `Perfil de egreso` != "") %>%
      unnest_tokens(bigram, `Perfil de egreso`, token = "ngrams", n = 2) %>%
      separate(bigram, into = c("palabra1", "palabra2"), sep = " ", fill = "right") %>%
      filter(!is.na(palabra1), !is.na(palabra2)) %>%
      filter(str_detect(palabra1, "^[a-zA-ZáéíóúñÁÉÍÓÚÑ]+$"),
             str_detect(palabra2, "^[a-zA-ZáéíóúñÁÉÍÓÚÑ]+$")) %>%
      filter(!palabra1 %in% c("de", "la", "el", "en", "y", "para", "del", "las", "los", "con", "su", "como", "se", "un", "a", "tráves", "una"),
             !palabra2 %in% c("de", "la", "el", "en", "y", "para", "del", "las", "los", "con", "su", "como", "se", "un", "a", "tráves", "una")) %>%
      unite(bigram, palabra1, palabra2, sep = " ") %>%
      count(bigram, sort = TRUE) %>%
      head(15)
    
    ggplotly(
      ggplot(bigramas, aes(x = reorder(bigram, n), y = n,
                           text = paste("Bigrama:", bigram, "<br>Frecuencia:", n))) +
        geom_col(fill = "darkorange") +
        coord_flip() +
        labs(title = "Bigramas más frecuentes", x = "", y = "Frecuencia") +
        theme_minimal(),
      tooltip = "text"
    )
  })
  
}

shinyApp(ui, server)