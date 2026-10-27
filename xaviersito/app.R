install.packages(c("shiny", "shinydashboard", "DT", "ggplot2",
                   "plotly", "dplyr", "readxl", "bslib"))
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(readxl)
library(bslib)

# ============================================
# UI
# ============================================
ui <- dashboardPage(
  skin = "purple",

  dashboardHeader(title = "Análisis de Notas"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Cargar Datos", tabName = "datos", icon = icon("upload")),
      menuItem(" Resumen", tabName = "resumen", icon = icon("chart-line")),
      menuItem("Aprobados/Reprobados", tabName = "aprob", icon = icon("check-circle")),
      menuItem("Gráficas", tabName = "graficas", icon = icon("chart-bar"))
    )
  ),

  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #f4f6fb; }
        .box { border-radius: 12px; box-shadow: 0 4px 12px rgba(0,0,0,0.08); }
        .small-box { border-radius: 12px; }
        .small-box h3 { font-weight: bold; }
      "))
    ),

    tabItems(
      # -------- TAB CARGAR DATOS --------
      tabItem(tabName = "datos",
              fluidRow(
                box(
                  title = "Cargar archivo (CSV o Excel)",
                  status = "primary", solidHeader = TRUE, width = 6,
                  fileInput("archivo", "Selecciona tu archivo:",
                            accept = c(".csv", ".xlsx", ".xls")),
                  helpText("El archivo debe tener una columna llamada 'Nombre' y
                     una o varias columnas de notas (ej: 'Nota1', 'Nota2', 'NotaFinal')."),
                  selectInput("columna_nota", "Columna de nota a analizar:",
                              choices = NULL),
                  numericInput("nota_min", "Nota mínima para aprobar:",
                               value = 70, min = 0, max = 100, step = 1)
                ),
                box(
                  title = "Vista previa de los datos",
                  status = "info", solidHeader = TRUE, width = 6,
                  DTOutput("tabla_preview")
                )
              ),
              fluidRow(
                box(
                  title = "¿No tienes datos? Usa el ejemplo",
                  status = "success", solidHeader = TRUE, width = 12,
                  actionButton("ejemplo", "Cargar datos de ejemplo",
                               icon = icon("magic"),
                               style = "color:#fff; background-color:#7b2cbf; border:none;")
                )
              )
      ),

      # -------- TAB RESUMEN --------
      tabItem(tabName = "resumen",
              fluidRow(
                valueBoxOutput("box_max", width = 3),
                valueBoxOutput("box_min", width = 3),
                valueBoxOutput("box_promedio", width = 3),
                valueBoxOutput("box_mediana", width = 3)
              ),
              fluidRow(
                valueBoxOutput("box_moda", width = 3),
                valueBoxOutput("box_desv", width = 3),
                valueBoxOutput("box_varianza", width = 3),
                valueBoxOutput("box_total", width = 3)
              ),
              fluidRow(
                box(
                  title = "Medidas de Tendencia Central y Dispersión",
                  status = "primary", solidHeader = TRUE, width = 12,
                  DTOutput("tabla_resumen")
                )
              )
      ),

      # -------- TAB APROBADOS --------
      tabItem(tabName = "aprob",
              fluidRow(
                valueBoxOutput("box_aprobados", width = 4),
                valueBoxOutput("box_reprobados", width = 4),
                valueBoxOutput("box_porcentaje", width = 4)
              ),
              fluidRow(
                box(
                  title = "alumnos Aprobados",
                  status = "success", solidHeader = TRUE, width = 6,
                  DTOutput("tabla_aprobados")
                ),
                box(
                  title = "alumnos Reprobados",
                  status = "danger", solidHeader = TRUE, width = 6,
                  DTOutput("tabla_reprobados")
                )
              ),
              fluidRow(
                box(
                  title = "Ranking de Mejores Notas",
                  status = "warning", solidHeader = TRUE, width = 12,
                  DTOutput("tabla_ranking")
                )
              )
      ),

      # -------- TAB GRÁFICAS --------
      tabItem(tabName = "graficas",
              fluidRow(
                box(
                  title = "Distribución de Notas (Histograma)",
                  status = "primary", solidHeader = TRUE, width = 6,
                  plotlyOutput("plot_hist")
                ),
                box(
                  title = "Boxplot de Notas",
                  status = "info", solidHeader = TRUE, width = 6,
                  plotlyOutput("plot_box")
                )
              ),
              fluidRow(
                box(
                  title = "Aprobados vs Reprobados",
                  status = "success", solidHeader = TRUE, width = 6,
                  plotlyOutput("plot_pie")
                ),
                box(
                  title = "Notas por Alumno",
                  status = "warning", solidHeader = TRUE, width = 6,
                  plotlyOutput("plot_barras")
                )
              )
      )
    )
  )
)

# ============================================
# SERVER
# ============================================
server <- function(input, output, session) {

  # ---- Datos reactivos ----
  datos <- reactiveVal(NULL)

  # Cargar archivo
  observeEvent(input$archivo, {
    req(input$archivo)
    ext <- tools::file_ext(input$archivo$name)
    df <- if (ext == "csv") {
      read.csv(input$archivo$datapath, stringsAsFactors = FALSE)
    } else {
      as.data.frame(read_excel(input$archivo$datapath))
    }
    datos(df)
  })

  # Cargar ejemplo
  observeEvent(input$ejemplo, {
    set.seed(123)
    df <- data.frame(
      Nombre = paste0("Estudiante ", 1:30),
      Nota1 = round(runif(30, 40, 100)),
      Nota2 = round(runif(30, 40, 100)),
      NotaFinal = round(runif(30, 40, 100))
    )
    datos(df)
  })

  # Actualizar selector de columna
  observe({
    req(datos())
    df <- datos()
    cols_num <- names(df)[sapply(df, is.numeric)]
    updateSelectInput(session, "columna_nota",
                      choices = cols_num,
                      selected = cols_num[1])
  })

  # Data frame con nota seleccionada
  df_nota <- reactive({
    req(datos(), input$columna_nota)
    df <- datos()
    df$Nota <- df[[input$columna_nota]]
    df
  })

  # ---- Tabla preview ----
  output$tabla_preview <- renderDT({
    req(datos())
    datatable(datos(),
              options = list(pageLength = 5, scrollX = TRUE),
              rownames = FALSE)
  })

  # ---- Value Boxes Resumen ----
  output$box_max <- renderValueBox({
    req(df_nota())
    valueBox(max(df_nota()$Nota, na.rm = TRUE),
             "Nota Máxima", icon = icon("arrow-up"), color = "green")
  })

  output$box_min <- renderValueBox({
    req(df_nota())
    valueBox(min(df_nota()$Nota, na.rm = TRUE),
             "Nota Mínima", icon = icon("arrow-down"), color = "red")
  })

  output$box_promedio <- renderValueBox({
    req(df_nota())
    valueBox(round(mean(df_nota()$Nota, na.rm = TRUE), 2),
             "Promedio", icon = icon("calculator"), color = "purple")
  })

  output$box_mediana <- renderValueBox({
    req(df_nota())
    valueBox(median(df_nota()$Nota, na.rm = TRUE),
             "Mediana", icon = icon("balance-scale"), color = "blue")
  })

  output$box_moda <- renderValueBox({
    req(df_nota())
    x <- df_nota()$Nota
    ux <- unique(x)
    moda <- ux[which.max(tabulate(match(x, ux)))]
    valueBox(moda, "Moda", icon = icon("bullseye"), color = "yellow")
  })

  output$box_desv <- renderValueBox({
    req(df_nota())
    valueBox(round(sd(df_nota()$Nota, na.rm = TRUE), 2),
             "Desv. Estándar", icon = icon("chart-area"), color = "aqua")
  })

  output$box_varianza <- renderValueBox({
    req(df_nota())
    valueBox(round(var(df_nota()$Nota, na.rm = TRUE), 2),
             "Varianza", icon = icon("sigma"), color = "maroon")
  })

  output$box_total <- renderValueBox({
    req(df_nota())
    valueBox(nrow(df_nota()),
             "Total Estudiantes", icon = icon("users"), color = "navy")
  })

  # ---- Tabla resumen ----
  output$tabla_resumen <- renderDT({
    req(df_nota())
    x <- df_nota()$Nota
    ux <- unique(x)
    moda <- ux[which.max(tabulate(match(x, ux)))]

    resumen <- data.frame(
      Medida = c("Máximo", "Mínimo", "Promedio (Media)", "Mediana",
                 "Moda", "Desviación Estándar", "Varianza",
                 "Rango", "Q1", "Q3", "Total registros"),
      Valor = c(
        max(x, na.rm = TRUE),
        min(x, na.rm = TRUE),
        round(mean(x, na.rm = TRUE), 2),
        median(x, na.rm = TRUE),
        moda,
        round(sd(x, na.rm = TRUE), 2),
        round(var(x, na.rm = TRUE), 2),
        diff(range(x, na.rm = TRUE)),
        quantile(x, 0.25, na.rm = TRUE),
        quantile(x, 0.75, na.rm = TRUE),
        length(x)
      )
    )
    datatable(resumen, options = list(dom = 't'), rownames = FALSE)
  })

  # ---- Aprobados/Reprobados ----
  aprobados <- reactive({
    req(df_nota(), input$nota_min)
    df_nota() %>% filter(Nota >= input$nota_min)
  })

  reprobados <- reactive({
    req(df_nota(), input$nota_min)
    df_nota() %>% filter(Nota < input$nota_min)
  })

  output$box_aprobados <- renderValueBox({
    valueBox(nrow(aprobados()), "Aprobados",
             icon = icon("check"), color = "green")
  })

  output$box_reprobados <- renderValueBox({
    valueBox(nrow(reprobados()), "Reprobados",
             icon = icon("times"), color = "red")
  })

  output$box_porcentaje <- renderValueBox({
    req(df_nota())
    pct <- round(nrow(aprobados()) / nrow(df_nota()) * 100, 1)
    valueBox(paste0(pct, "%"), "Tasa de Aprobación",
             icon = icon("percent"), color = "purple")
  })

  output$tabla_aprobados <- renderDT({
    datatable(aprobados(), options = list(pageLength = 5, scrollX = TRUE),
              rownames = FALSE)
  })

  output$tabla_reprobados <- renderDT({
    datatable(reprobados(), options = list(pageLength = 5, scrollX = TRUE),
              rownames = FALSE)
  })

  output$tabla_ranking <- renderDT({
    req(df_nota())
    df <- df_nota() %>% arrange(desc(Nota)) %>% head(10)
    datatable(df, options = list(dom = 't'), rownames = FALSE)
  })

  # ---- Gráficas ----
  output$plot_hist <- renderPlotly({
    req(df_nota())
    p <- ggplot(df_nota(), aes(x = Nota, fill = ..count..)) +
      geom_histogram(bins = 15, color = "white", alpha = 0.9) +
      scale_fill_gradient(low = "#c77dff", high = "#5a189a") +
      geom_vline(xintercept = mean(df_nota()$Nota, na.rm = TRUE),
                 color = "red", linetype = "dashed", size = 1) +
      labs(x = "Notas", y = "Frecuencia",
           title = "Distribución (línea roja = promedio)") +
      theme_minimal()
    ggplotly(p)
  })

  output$plot_box <- renderPlotly({
    req(df_nota())
    p <- ggplot(df_nota(), aes(y = Nota, x = "")) +
      geom_boxplot(fill = "#9d4edd", color = "#3c096c", alpha = 0.8) +
      labs(x = "", y = "Notas") +
      theme_minimal()
    ggplotly(p)
  })

  output$plot_pie <- renderPlotly({
    req(df_nota(), input$nota_min)
    df <- data.frame(
      Estado = c("Aprobados", "Reprobados"),
      Cantidad = c(nrow(aprobados()), nrow(reprobados()))
    )
    plot_ly(df, labels = ~Estado, values = ~Cantidad, type = "pie",
            marker = list(colors = c("#2ecc71", "#e74c3c"))) %>%
      layout(title = "Aprobados vs Reprobados")
  })

  output$plot_barras <- renderPlotly({
    req(df_nota())
    df <- df_nota()
    df$Color <- ifelse(df$Nota >= input$nota_min, "Aprobado", "Reprobado")
    p <- ggplot(df, aes(x = reorder(Nombre, Nota), y = Nota,
                        fill = Color, text = paste("Alumno:", Nombre,
                                                   "<br>Nota:", Nota))) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = c("Aprobado" = "#2ecc71",
                                   "Reprobado" = "#e74c3c")) +
      labs(x = "", y = "Nota", fill = "Estado") +
      theme_minimal()
    ggplotly(p, tooltip = "text")
  })
}

# ============================================
# RUN APP
# ============================================
shinyApp(ui, server)
