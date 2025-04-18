#Librerías necesarias
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

# Codigo para la verificación de las librerías
librerias <- c(
  "shiny", "shinydashboard", "ggplot2", "plotly", "DT",
  "wordcloud2", "tm", "shinythemes", "readxl", "dplyr",
  "tidytext", "stringr", "tidyr", "forcats", "scales", "shinycssloaders"
)

# Código para saber que librerias están instaladas
no_instaladas <- librerias[!librerias %in% installed.packages()[,"Package"]]

#Instalar todas las librerías que sean necesarias
if (length(no_instaladas) > 0) {
  install.packages(no_instaladas)
}


# ---# ---# -----------------------------------------------------------
# Carga y preprocesamiento de datos
# -----------------------------------------------------------
# Define una función llamada load_data para cargar y procesar los datos
load_data <- function() {
  data_path <- "C:/Users/cesar/OneDrive/Escritorio/Trabajobusinessintelligence/Base Bussiness .xlsx" # Define la ruta donde se encuentra el archivo Excel
  # Intenta ejecutar el bloque de código para leer los datos, y captura cualquier error
  tryCatch({ # Lee la hoja "base de datos" del archivo Excel y transforma algunas columnas
    base <- read_excel(data_path, sheet = "base de datos") %>%
      mutate(
        empleabilidad = factor(empleabilidad, levels = c("Si", "No")), # Convierte la columna empleabilidad en un factor con niveles "Si" y "No"
        universidad = as.factor(universidad),  # Convierte la columna universidad a factor
        abreviacion_universidad=as.factor(abreviacion_universidad) # Convierte la columna abreviacion_universidad a factor
        
      )
    # Lee la hoja "Hoja 1" del archivo Excel y convierte la columna "Perfil de egreso" a texto (character)
    perfiles <- read_excel(data_path, sheet = "Hoja 1") %>%
      mutate(`Perfil de egreso` = as.character(`Perfil de egreso`))
    # Devuelve una lista con los dos dataframes: base y perfiles
    list(base = base, perfiles = perfiles)
  }, error = function(e) {
    showNotification("Error al cargar los datos. Verifica la ruta del archivo.", type = "error")   # En caso de error, muestra una notificación de error al usuario
    list(base = data.frame(), perfiles = data.frame()) # Devuelve listas vacías para base y perfiles en caso de error
  })
}

data_list <- load_data() # Llama a la función load_data y guarda el resultado en data_list
base <- data_list$base # Extrae el dataframe base desde la lista
perfiles <- data_list$perfiles # Extrae el dataframe perfiles desde la lista

# -----------------------------------------------------------
# Preprocesamiento de texto
# -----------------------------------------------------------

process_text <- function(text) { # Función que procesa texto para preparar datos para una nube de palabras
  corpus <- Corpus(VectorSource(text)) %>%  # Crea un corpus (colección de documentos) a partir del texto de entrada
    tm_map(content_transformer(tolower)) %>% # Convierte todo el texto a minúsculas
    tm_map(removePunctuation) %>% # Elimina signos de puntuación
    tm_map(removeNumbers) %>%  # Elimina números
    tm_map(removeWords, stopwords("spanish")) %>% # Elimina palabras vacías del idioma español (stopwords comunes como "que", "como", etc.)
    tm_map(removeWords, c("de", "la", "el", "en", "y", "para", "del", "las", "los")) # Elimina palabras adicionales que puedan ser poco informativas
  
  tdm <- TermDocumentMatrix(corpus) # Crea una matriz término-documento (frecuencia de cada palabra en el corpus)
  m <- as.matrix(tdm)  # Convierte la matriz en un objeto tipo matriz de R
  v <- sort(rowSums(m), decreasing = TRUE)   # Suma las frecuencias de cada palabra en todas las filas (palabras) y ordena en orden descendente
  data.frame(word = names(v), freq = v)   # Devuelve un data frame con las palabras y sus frecuencias
}

wordcloud_data <- reactive({  # Crea un objeto reactivo que procesa el texto de los perfiles para generar datos de una nube de palabras
  texto <- paste(perfiles$`Perfil de egreso`, collapse = " ") # Une todos los textos de la columna "Perfil de egreso" en un solo texto largo
  process_text(texto) # Llama a la función process_text para procesar ese texto
})

# -----------------------------------------------------------
# UI
# -----------------------------------------------------------

ui <- dashboardPage( # Define la interfaz del usuario utilizando dashboardPage de shinydashboard
  skin = "purple",  # Establece el color de la skin (tema visual) del dashboard
  dashboardHeader( # Encabezado del dashboard con un título y un ícono
    title = span(icon("graduation-cap"), "Dashboard Educativo"), # Ícono de birrete + título
    titleWidth = 30  # Ancho del título en píxeles
  ),
  dashboardSidebar( # Barra lateral del dashboard
    width = 300, # Ancho de la barra lateral
    
    sidebarMenu( # Menú lateral con diferentes pestañas
      id = "tabs", # ID del menú (útil para usar en condicionales)
      # Cada menuItem define una pestaña con su nombre, tabName y un ícono
      menuItem("Resumen", tabName = "resumen", icon = icon("dashboard")),
      menuItem("Tiempo en empleo", tabName = "tiempo", icon = icon("clock")),
      menuItem("Nube de Palabras", tabName = "nube", icon = icon("cloud")),
      menuItem("Análisis Comparativo", tabName = "multigraficos", icon = icon("chart-bar")),
      menuItem("Análisis Textual", tabName = "analisis_texto", icon = icon("language"))
    ),
    conditionalPanel(     # Panel condicional que se muestra solo si se está en las pestañas 'tiempo' o 'multigraficos'
      condition = "input.tabs == 'tiempo' || input.tabs == 'multigraficos'", # Condición JS
      div(style = "padding: 15px;",  # Contenedor con padding para los filtros
          selectInput("universidad_filtro", "Filtrar por Universidad:", # Menú desplegable para filtrar por universidad
                      choices = c("Todas", sort(unique(as.character(base$universidad))))),
          conditionalPanel(  # Filtro adicional que solo aparece en la pestaña "tiempo"
            condition = "input.tabs == 'tiempo'",
            selectInput("empleabilidad_filtro", "Filtrar por Empleabilidad:", # Menú desplegable para filtrar por empleabilidad
                        choices = c("Todos", "Si", "No"))
          )
      )
    )
  ),
  dashboardBody(   # Define el cuerpo del dashboard
    tags$head( # Inserta elementos en la cabecera del HTML (como hojas de estilo y estilos personalizados)
      # Enlace a una hoja de estilo externa de Font Awesome (íconos)
      tags$link(rel = "stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"),
      # Estilo CSS personalizado para mejorar la apariencia de algunos elementos
      tags$style(HTML("
        .box-title { font-weight: bold; }
        .main-header .logo { font-weight: bold; }
        .plotly-container { height: 100% !important; }
        .wordcloud2-container { margin: 0 auto; }
        .info-box { cursor: pointer; }
      "))
    ),
    
    tabItems( # Contenedor de pestañas (cada tabItem corresponde a una vista/página)
      tabItem( # Primera pestaña: Resumen
        tabName = "resumen", # Nombre de la pestaña (coincide con tabName del sidebar)
        fluidRow(# Primera fila con infoBoxes resumen
          infoBoxOutput("total_egresados"), # Caja con el total de egresados
          infoBoxOutput("tasa_empleabilidad"), # Caja con la tasa de empleabilidad
          infoBoxOutput("promedio_tiempo") # Caja con el promedio del tiempo en conseguir empleo
        ),
        fluidRow( # Segunda fila con dos cajas (box): gráfico y tabla
          box(
            title = "Porcentaje de Empleabilidad por Universidad",  # Título del box
            width = 6,  # Ocupa 6 de 12 columnas
            status = "primary", # Color azul
            solidHeader = TRUE, # Encabezado sólido (sin fondo gris)
            plotlyOutput("grafico1") %>% withSpinner() # Gráfico interactivo con spinner de carga
          ),
          box(
            title = "Tabla Resumen",  # Título del segundo box
            width = 6,  # Ocupa la otra mitad de la fila
            status = "primary", # Color azul
            solidHeader = TRUE, # Encabezado sólido
            DTOutput("tabla1") %>% withSpinner() # Tabla interactiva con spinner de carga
          )
        )
      ),
      
      tabItem(  # Define el contenido para la pestaña "tiempo"
        tabName = "tiempo", # Este nombre debe coincidir con el tabName del menú lateral
        fluidRow( # Fila que contiene un único box (ocupa todo el ancho: 12 columnas)
          box( 
            title = "Distribución de Tiempo para Conseguir Empleo",  # Título del box
            width = 12,   # El box ocupa las 12 columnas de ancho (toda la fila)
            status = "info", # Color azul claro (informativo)
            solidHeader = TRUE,  # Encabezado con fondo sólido (sin transparencia)
            # Salida de un gráfico interactivo generado con plotly
            plotlyOutput("grafico_tiempo") %>% withSpinner(), # Muestra spinner mientras se carga
            footer = "Nota: El tiempo se mide en meses desde la titulación hasta la obtención del primer empleo."  # Pie de página del box con una aclaración
          )
        ),
      ),
      
      tabItem( # Define el contenido para la pestaña "nube"
        tabName = "nube", # Identificador de la pestaña (coincide con el tabName del menú lateral)
        fluidRow( # Crea una fila que contiene un solo box de ancho completo
          box(
            title = "Nube de Palabras - Perfiles de Egreso",   # Título del box
            width = 12,  # El box ocupa todo el ancho disponible (12 columnas)
            status = "success", # Color verde (éxito)
            solidHeader = TRUE, # Encabezado con fondo sólido (sin transparencia)
            # Salida del gráfico tipo wordcloud, usando la librería wordcloud2
            wordcloud2Output("nube_palabras", height = "600px") %>% withSpinner(), # Spinner mientras carga
            footer = "Palabras más frecuentes en los perfiles de egreso de los estudiantes." # Pie del box con una nota explicativa
          )
        )
      ),
      
      tabItem(# Define el contenido para la pestaña "multigraficos"
        tabName = "multigraficos", # Identificador que debe coincidir con el del menú lateral
        fluidRow(   # Fila con dos gráficos (barras y torta), cada uno ocupando la mitad del ancho
          box(   # Primer box: gráfico de barras
            title = "Distribución de Empleabilidad",  # Título del primer gráfico
            width = 6,  # Ocupa 6 de 12 columnas (mitad del ancho)
            status = "warning", # Color amarillo (advertencia)
            solidHeader = TRUE, # Encabezado sólido
            plotlyOutput("grafico_barras") %>% withSpinner() # Salida del gráfico de barras con spinner
          ),
          box(
            title = "Proporción de Empleabilidad", # Título del segundo gráfico
            width = 6,  # También ocupa 6 columnas
            status = "warning", # Color igual al anterior para coherencia visual
            solidHeader = TRUE, # Encabezado sólido
            plotlyOutput("grafico_torta") %>% withSpinner() # Salida del gráfico de torta con spinner
          )
        )
      ),
      
      tabItem(  # Define el contenido para la pestaña "analisis_texto"
        tabName = "analisis_texto", # Identificador del tab que debe coincidir con el menú lateral
        fluidRow(  # Fila con dos gráficos relacionados al análisis textual
          box( # Primer box: frecuencia de palabras clave
            title = "Frecuencia de Palabras Clave",  # Título del gráfico de frecuencia
            width = 6,  # Ocupa la mitad del ancho (6 columnas)
            status = "danger",  # Color rojo (peligro/énfasis)
            solidHeader = TRUE, # Encabezado con fondo sólido
            plotlyOutput("frecuencia_palabras") %>% withSpinner()  # Muestra gráfico con spinner de carga
          ),
          box( # Segundo box: bigramas más frecuentes (dos palabras juntas)
            title = "Bigramas Más Frecuentes",   # Título del gráfico de bigramas
            width = 6,  # También ocupa 6 columnas (la otra mitad)
            status = "danger", # Mismo color rojo para mantener coherencia visual
            solidHeader = TRUE, # Encabezado sólido
            plotlyOutput("bigrama_plot") %>% withSpinner()# Muestra gráfico de bigramas con spinner
          )
        )
      )
    )
  )
)

# -----------------------------------------------------------
# Server
# -----------------------------------------------------------

server <- function(input, output, session) { # Define la función principal del servidor
  
  datos_filtrados <- reactive({ # Crea un conjunto de datos reactivo filtrado según la universidad y empleabilidad seleccionadas
    req(input$universidad_filtro) # Requiere que el input 'universidad_filtro' esté disponible
    req(input$empleabilidad_filtro) # Requiere que el input 'empleabilidad_filtro' esté disponible
    
    df <- base # Se parte de la base de datos completa
    # Si el usuario seleccionó una universidad específica (distinta a "Todas"), se filtra por esa universidad
    if (input$universidad_filtro != "Todas") {
      df <- df %>% filter(universidad == input$universidad_filtro)
    }
    # Si el usuario seleccionó una opción de empleabilidad específica (distinta a "Todos"), se filtra también por eso
    if (input$empleabilidad_filtro != "Todos") {
      df <- df %>% filter(empleabilidad == input$empleabilidad_filtro)
    }
    
    df # Retorna el dataframe filtrado
  })
  
  output$total_egresados <- renderInfoBox({ # Renderiza el 'output$total_egresados' como un cuadro informativo con el total de egresados
    infoBox(
      "Total Egresados",  # Título del cuadro informativo
      nrow(base),   # Calcula el número total de egresados (filas de la base de datos)
      icon = icon("users"), # Ícono representando usuarios (personas)
      color = "navy"  # Color azul oscuro para el cuadro informativo
    )
  })
  
  output$grafico1 <- renderPlotly({     # Renderiza el 'output$grafico1' como un gráfico de barras usando 'ggplotly'
    df <- base %>% # Prepara los datos para el gráfico: agrupa por abreviación de universidad y calcula la tasa de empleabilidad
      group_by(abreviacion_universidad) %>% # Agrupa por la universidad (abreviatura)
      summarise(
        empleabilidad_pct = mean(empleabilidad == "Si") * 100,# Calcula el porcentaje de empleabilidad
        n = n() # Cuenta el número de egresados por universidad
      ) %>%
      mutate(abreviacion_universidad = fct_reorder(abreviacion_universidad, -empleabilidad_pct)) # Reordena las universidades por el porcentaje de empleabilidad
    # Crea un gráfico de barras interactivo con 'ggplotly'
    ggplotly(
      ggplot(df, aes(x = abreviacion_universidad, y = empleabilidad_pct, # Establece las variables para el gráfico
                     text = paste("Universidad:", abreviacion_universidad, "<br>",# Información para mostrar al pasar el mouse
                                  "Empleabilidad:", round(empleabilidad_pct, 1), "%", "<br>",
                                  "Muestra:", n, "egresados"))) + 
        geom_col(aes(fill = empleabilidad_pct)) + # Dibuja las barras y asigna color según el porcentaje de empleabilidad
        scale_fill_gradient(low = "#FF9999", high = "#66CC99") + # Gradiente de colores desde rojo a verde
        labs(title = " ", 
             y = "% Empleabilidad", x = "Universidades") + # Etiquetas de los ejes
        theme_minimal() + # Estilo minimalista para el gráfico
        theme(axis.text.x = element_text(angle = 45, hjust = 1)), # Rota los textos en el eje X para mayor legibilidad
      tooltip = "text" # Muestra la información adicional al pasar el cursor sobre las barras
    ) %>% config(displayModeBar = FALSE) # Configura el gráfico para no mostrar la barra de herramientas (interacción)
  })
  
  
  output$tabla1 <- renderDT({   # Renderiza el 'output$tabla1' como una tabla interactiva
    df <- base %>%  # Prepara los datos para la tabla: agrupa por universidad y calcula las métricas de interés
      group_by(universidad) %>%  # Agrupa por la universidad
      summarise(
        Total = n(), # Número total de egresados por universidad
        Empleados = sum(empleabilidad == "Si"), # Número de empleados (empleabilidad == "Si")
        No_Empleados = sum(empleabilidad == "No"),   # Número de no empleados (empleabilidad == "No")
        Porcentaje_Empleabilidad = round(mean(empleabilidad == "Si"), 2), # Calcula el porcentaje de empleabilidad
        Tiempo_Promedio = round(mean(`tiempo que tarda en conseguir empleo post titulacion`, na.rm = TRUE), 1) # Calcula el tiempo promedio para conseguir empleo, ignorando valores NA
      ) %>%
      arrange(desc(Porcentaje_Empleabilidad)) # Ordena los datos por porcentaje de empleabilidad de mayor a menor
    # Genera la tabla interactiva con la librería DT
    datatable( 
      df, # Los datos a mostrar en la tabla
      rownames = FALSE, # No muestra los nombres de las filas (índices)
      extensions = 'Buttons', # Activa las extensiones para exportación
      options = list(  # Configura las opciones de la tabla
        dom = 'Bfrtip', # Configura la disposición de los elementos en la tabla (Botones, filtros, etc.)
        buttons = c('copy', 'csv', 'excel'), # Habilita botones para copiar, exportar como CSV o Excel
        pageLength = 5, # Muestra 5 filas por página en la tabla
        autoWidth = TRUE # Ajusta automáticamente el ancho de las columnas
      ),
      colnames = c('Universidad', 'Total', 'Empleados', 'No Empleados', '% Empleabilidad', 'Tiempo Prom (meses)') # Establece los nombres de las columnas
    ) %>% formatPercentage('Porcentaje_Empleabilidad', 2) # Da formato de porcentaje a la columna 'Porcentaje_Empleabilidad' con 2 decimales
  })
  # Renderiza el gráfico 'output$grafico_tiempo' como un gráfico interactivo de Plotly
  output$grafico_tiempo <- renderPlotly({
    validate(    # Valida que haya datos disponibles después de aplicar los filtros seleccionados
      need(nrow(datos_filtrados()) > 0, "No hay datos disponibles con los filtros seleccionados") # Si no hay datos después del filtro, muestra un mensaje de advertencia
    )
    # Genera el gráfico con ggplot2 y luego lo convierte en un gráfico interactivo con ggplotly
    ggplotly(
      ggplot(datos_filtrados(),  # Utiliza el conjunto de datos filtrados
             aes(x = abreviacion_universidad, # Eje X: abreviación de la universidad
                 y = `tiempo que tarda en conseguir empleo post titulacion`, # Eje Y: tiempo para conseguir empleo post-titulación
                 color = universidad, # Color por universidad
                 text = paste("Universidad:", universidad, "<br>",  # Texto que aparecerá al pasar el mouse
                              "Tiempo:", `tiempo que tarda en conseguir empleo post titulacion`, "meses"))) + # Incluye el tiempo en el texto
        geom_boxplot() + # Agrega un boxplot para mostrar la distribución del tiempo
        geom_jitter(alpha = 0.5, width = 0.2) + # Agrega puntos dispersos (jitter) para mejorar la visibilidad de los datos
        labs(title = " ",  # No muestra un título para el gráfico
             y = "Meses", x = "") + # Etiqueta del eje Y (meses) y el eje X (vacío)
        theme_minimal() + # Utiliza un tema minimalista para el gráfico
        theme(legend.position = "none"), # Elimina la leyenda
      tooltip = "text" # Activa el tooltip para mostrar información al pasar el mouse
    )
  })
  # Renderiza la nube de palabras 'output$nube_palabras' como una visualización interactiva de Wordcloud2
  output$nube_palabras <- renderWordcloud2({
    # Genera la nube de palabras utilizando la función 'wordcloud2' con los datos procesados de 'wordcloud_data()'
    wordcloud2(wordcloud_data(),  # Datos de la nube de palabras
               size = 1.5,  # Tamaño de las palabras en la nube (1.5 veces el tamaño por defecto)
               color = "random-dark", # Colores aleatorios oscuros para las palabras
               backgroundColor = "white") # Fondo blanco para la nube de palabras
  })
  # Renderiza el gráfico de barras 'output$grafico_barras' como un gráfico interactivo de Plotly
  output$grafico_barras <- renderPlotly({
    # Crea un resumen de la cantidad de respuestas por cada categoría de 'empleabilidad'
    df <- datos_filtrados() %>% 
      count(empleabilidad, .drop = FALSE) # Cuenta las observaciones para cada valor de 'empleabilidad'
    # Genera el gráfico con ggplot2 y luego lo convierte en un gráfico interactivo con ggplotly
    ggplotly(
      ggplot(df, aes(x = empleabilidad, y = n, fill = empleabilidad, # Mapeo de las variables
                     text = paste("Empleabilidad:", empleabilidad, "<br>",  # Texto que aparece al pasar el mouse
                                  "Cantidad:", n))) +
        geom_col() + # Crea un gráfico de barras (columna)
        scale_fill_manual(values = c("Si" = "#4DAF4A", "No" = "#E41A1C")) + # Colorea las barras según la empleabilidad (verde para 'Sí' y rojo para 'No')
        labs(title = " ",  # No muestra título
             y = "Cantidad", x = "Empleabilidad") +# Etiquetas para los ejes Y (Cantidad) y X (Empleabilidad)
        theme_minimal(), # Utiliza un tema minimalista para el gráfico
      tooltip = "text" # Habilita el tooltip para mostrar detalles al pasar el mouse
    ) %>% config(displayModeBar = FALSE) # Desactiva la barra de herramientas de Plotly
  })
  # Renderiza el gráfico de torta 'output$grafico_torta' como un gráfico interactivo de Plotly
  output$grafico_torta <- renderPlotly({
    df <- datos_filtrados() %>% # Prepara los datos filtrados y transforma la variable 'empleabilidad' para tener valores comprensibles
      mutate(
        empleabilidad = case_when(
          empleabilidad == 1 ~ "Si", # Si empleabilidad es 1, lo convierte a "Si"
          empleabilidad == 2 ~ "No",  # Si empleabilidad es 2, lo convierte a "No"
          is.na(empleabilidad) ~ "NA", # Si empleabilidad es NA, lo convierte a "NA"
          TRUE ~ as.character(empleabilidad)  # Si es otro valor, lo convierte a texto
        )
      ) %>%
      count(empleabilidad) # Cuenta la cantidad de observaciones por cada categoría de 'empleabilidad'
    
    # Asegura que las categorías de 'empleabilidad' estén en un orden consistente (Si, No, NA)
    df$empleabilidad <- factor(df$empleabilidad, levels = c("Si", "No", "NA"))
    colores <- c("Si" = "#4DAF4A", "No" = "#E41A1C", "NA" = "#999999") # Define los colores para cada categoría
    # Crea un gráfico de torta utilizando 'plot_ly' para una visualización interactiva
    plot_ly(
      df, # Utiliza los datos 'df' filtrados y contados
      labels = ~empleabilidad, # Utiliza las categorías de 'empleabilidad' como etiquetas
      values = ~n,  # Utiliza los recuentos (n) como valores
      type = 'pie', # Especifica que el gráfico es de tipo torta
      textinfo = 'label+percent', # Muestra las etiquetas y el porcentaje en cada sector
      insidetextorientation = 'radial', # Alinea el texto dentro del gráfico radialmente
      marker = list(colors = colores[as.character(df$empleabilidad)]), # Asigna colores a cada categoría
      hoverinfo = 'text', # Muestra información adicional cuando el usuario pasa el ratón
      text = ~paste("Cantidad:", n)# Texto adicional al pasar el ratón (cantidad)
    ) %>%
      layout(title = " ", # No se muestra un título en el gráfico
             showlegend = FALSE) # Desactiva la leyenda del gráfico
  })
  
  output$frecuencia_palabras <- renderPlotly({
    palabras <- perfiles %>% # Preprocesa los textos para obtener las palabras más frecuentes
      unnest_tokens(word, `Perfil de egreso`) %>% # Convierte el texto en palabras (tokens)
      anti_join(stop_words, by = "word") %>% # Elimina las palabras de parada comunes (como "y", "de", "el", etc.)
      filter(!word %in% c("de", "la", "el", "en", "y", "para", "del", "las", "los","con","su","como","se","una","al")) %>% # Filtra algunas palabras específicas
      count(word, sort = TRUE) %>% # Cuenta la frecuencia de cada palabra
      head(15) # Toma las 15 palabras más frecuentes
    # Crea el gráfico de barras de las 15 palabras más frecuentes
    ggplotly(
      ggplot(palabras, aes(x = reorder(word, n), y = n, 
                           text = paste("Palabra:", word, "<br>Frecuencia:", n))) + # Define el texto emergente para cada barra
        geom_col(fill = "steelblue") + # Crea las barras con el color azul acero
        coord_flip() + # Rota el gráfico para que las palabras aparezcan horizontalmente
        labs(title = "Palabras más frecuentes", x = "", y = "Frecuencia") + # Agrega el título y etiquetas
        theme_minimal(), # Usa un tema minimalista para el gráfico
      tooltip = "text"# Muestra el texto definido cuando el usuario pasa el ratón sobre las barras
    )
  })
  
  output$bigrama_plot <- renderPlotly({
    bigramas <- perfiles %>% # Preprocesamiento del texto para obtener bigramas (pares de palabras consecutivas)
      mutate(`Perfil de egreso` = str_squish(`Perfil de egreso`)) %>%  # Eliminar espacios innecesarios al inicio y al final
      filter(!is.na(`Perfil de egreso`), `Perfil de egreso` != "") %>% # Filtrar filas con texto vacío o NA
      unnest_tokens(bigram, `Perfil de egreso`, token = "ngrams", n = 2) %>% # Dividir en bigramas
      separate(bigram, into = c("palabra1", "palabra2"), sep = " ", fill = "right") %>% # Separar los bigramas en dos columnas
      filter(!is.na(palabra1), !is.na(palabra2)) %>% # Filtrar los bigramas con valores NA
      filter(str_detect(palabra1, "^[a-zA-ZáéíóúñÁÉÍÓÚÑ]+$"), # Filtrar palabras que solo contienen caracteres alfabéticos (incluidos acentos)
             str_detect(palabra2, "^[a-zA-ZáéíóúñÁÉÍÓÚÑ]+$")) %>%# Hacer lo mismo para la segunda palabra del bigrama
      filter(!palabra1 %in% c("de", "la", "el", "en", "y", "para", "del", "las", "los", "con", "su", "como", "se", "un", "a", "tráves", "una"), # Filtrar palabras comunes irrelevantes
             !palabra2 %in% c("de", "la", "el", "en", "y", "para", "del", "las", "los", "con", "su", "como", "se", "un", "a", "tráves", "una")) %>%  # Lo mismo para la segunda palabra
      unite(bigram, palabra1, palabra2, sep = " ") %>%  #Reunir las dos palabras en un solo bigrama 
      count(bigram, sort = TRUE) %>%  # Contar la frecuencia de cada bigrama
      head(15) # Seleccionar los 15 bigramas más frecuentes
    # Crear un gráfico de barras interactivo con los bigramas más frecuentes
    ggplotly(
      ggplot(bigramas, aes(x = reorder(bigram, n), y = n, # Reordenar los bigramas por frecuencia
                           text = paste("Bigrama:", bigram, "<br>Frecuencia:", n))) + # Agregar texto al tooltip con el bigrama y su frecuencia
        geom_col(fill = "darkorange") + # Dibujar las barras con color naranja oscuro
        coord_flip() + # Rotar el gráfico para mostrar barras horizontales
        labs(title = "Bigramas más frecuentes", x = "", y = "Frecuencia") + # Títulos de los ejes
        theme_minimal(), # Usar un tema minimalista para el gráfico
      tooltip = "text" # Mostrar el texto en el tooltip cuando se pasa el ratón sobre las barras
    )
  })
  
}
# Llamamos a shinyApp para lanzar la aplicación
shinyApp(ui, server)