# ==============================================================================
# --- SISTEMA DE GESTIÓN BAR ---
# ==============================================================================
#install.packages(c(
#  "shiny",
#  "shinydashboard",
#  "dplyr",
#  "DT",
#  "shinyjs",
#  "lubridate",
#  "plotly",
#  "DBI",
#  "RPostgres"
#))


# 1. Librerías
library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(shinyjs)
library(lubridate)
library(plotly)      
library(DBI)
library(RPostgres)
library(randomForest)
library(e1071)
library(glmnet)
library(cluster)
library(ggplot2)

# 2. Configuración de Base de Datos
db_config <- list(dbname = "BAR", host = "localhost", port = 5432, user = "postgres", password = "12345")

# --- FUNCIONES BASE DE DATOS ---
get_con <- function() {
  tryCatch({
    dbConnect(RPostgres::Postgres(), dbname=db_config$dbname, host=db_config$host, 
              port=db_config$port, user=db_config$user, password=db_config$password)
  }, error = function(e) return(NULL))
}

cargar_datos_bd <- function() {
  con <- get_con()
  if(is.null(con)) return(data.frame())
  
  query <- "SELECT id AS row_id, fecha, hora, peso, estado FROM pesos ORDER BY fecha DESC, hora DESC"
  df <- dbGetQuery(con, query)
  dbDisconnect(con)
  
  if(nrow(df) > 0) {
    df$fecha <- as.Date(df$fecha)
    df$hora <- as.character(df$hora) 
    
    # Variables auxiliares
    df$dia_semana <- lubridate::wday(df$fecha, week_start = 1, label = TRUE, abbr = FALSE) 
    df$hora_entera <- as.numeric(substr(df$hora, 1, 2))
    df$mes_anio <- format(df$fecha, "%Y-%m")
    df$anio <- format(df$fecha, "%Y")
  }
  return(df)
}

# --- CREDENCIALES ---
# credentials <- data.frame(
#   user = c("admin", "mayorista"),
#   password = c("1234", "mayo123"),
#   role = c("Administrador", "Mayorista"),
#   stringsAsFactors = FALSE
# )

# ==============================================================================
# --- UI PRINCIPAL ---
# ==============================================================================
# --- FUNCIÓN DE LOGIN CON BASE DE DATOS ---
verificar_login_bd <- function(usuario, pass) {
  con <- get_con()
  if(is.null(con)) return(NULL)
  
  # Consultamos usuario y hacemos JOIN con roles para saber si es Admin o Mayorista
  # Nota: Asumimos que la contraseña está en texto plano en 'password_hash' como lo hicimos en el SQL
  query <- sprintf("
    SELECT u.username, r.nombre AS rol 
    FROM usuarios u
    JOIN roles r ON u.rol_id = r.id
    WHERE u.username = '%s' AND u.password_hash = '%s'
  ", usuario, pass)
  
  resultado <- tryCatch({
    dbGetQuery(con, query)
  }, error = function(e) return(data.frame()))
  
  dbDisconnect(con)
  return(resultado)
}
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;700&display=swap", rel = "stylesheet"),
    tags$style(HTML("
      /* --- ESTILOS GENERALES --- */
      body { font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; background-color: #f8f9fa; }
      
      /* --- ESTILOS PÁGINA PRINCIPAL (ORIGINAL) --- */
      .hero-banner {
        background: linear-gradient(135deg, #1b5e20 0%, #4caf50 100%);
        color: white; padding: 30px 20px; text-align: center; margin-bottom: 0;
        box-shadow: 0 4px 12px rgba(0,0,0,0.15); border-radius: 0;
      }
      .header-content { display: flex; align-items: center; justify-content: center; flex-wrap: wrap; }
      .hero-title { font-size: 2.5rem; font-weight: 800; margin-bottom: 5px; text-transform: uppercase; letter-spacing: 2px; text-shadow: 2px 2px 4px rgba(0,0,0,0.2); }
      .hero-subtitle { font-size: 1.2rem; opacity: 0.95; margin-bottom: 10px; font-weight: 300; }
      .login-button { background-color: #ffc107; color: #1b5e20; border: none; padding: 12px 40px; font-size: 1.2rem; font-weight: 600; border-radius: 30px; cursor: pointer; transition: all 0.3s ease; box-shadow: 0 6px 15px rgba(0,0,0,0.2); margin-top: 20px; }
      .login-button:hover { background-color: #ffd54f; transform: translateY(-3px) scale(1.05); }
      .info-card { background: white; border-radius: 12px; padding: 25px; margin-bottom: 25px; box-shadow: 0 4px 12px rgba(0,0,0,0.08); border-top: 5px solid #2E7D32; transition: transform 0.3s ease; }
      .info-card:hover { transform: translateY(-3px); }
      .info-card h3 { color: #2E7D32; font-weight: 700; font-size: 1.5rem; margin-bottom: 15px; display: flex; align-items: center; gap: 10px; }
      .section-title { text-align: center; color: #2E7D32; font-weight: 700; font-size: 1.8rem; margin: 40px 0 25px 0; padding-bottom: 10px; border-bottom: 2px solid #28a745; }
      .landing-img { width: auto; max-height: 90px; object-fit: contain; margin: 0 auto; border-radius: 8px; background: white; padding: 5px; }
      .logo-container { text-align: center; background: rgba(255, 255, 255, 0.15); padding: 10px; border-radius: 10px; backdrop-filter: blur(5px); display: inline-block; }
      .chart-container { background: white; border-radius: 12px; padding: 25px; margin-bottom: 25px; box-shadow: 0 4px 12px rgba(0,0,0,0.08); border: 1px solid #e0e0e0; }
      .chart-title { color: #2E7D32; font-weight: 700; font-size: 1.4rem; margin-bottom: 15px; text-align: center; }
      .chart-description { background-color: #f8f9fa; padding: 15px; border-radius: 8px; border-left: 4px solid #28a745; margin-top: 15px; font-size: 0.95rem; color: #555; line-height: 1.4; }
      
      /* --- ESTILOS DASHBOARD INTERNO --- */
      .box { border-radius: 8px; box-shadow: 0 4px 15px rgba(0,0,0,0.05); border-top: none; background: #ffffff; margin-bottom: 25px; }
      .box-header { border-bottom: 1px solid #f0f0f0; padding: 15px; }
      .box-header .box-title { font-size: 18px; font-weight: 700; color: #2E7D32; font-family: 'Roboto', sans-serif; }
      .control-panel { background: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 10px rgba(0,0,0,0.05); margin-bottom: 20px; }
      /* Footer Explicativo debajo de los gráficos */
      .chart-footer { padding: 15px; background-color: #fafafa; border-top: 1px solid #eee; border-radius: 0 0 8px 8px; font-size: 0.85em; color: #555; }
      .chart-footer strong { color: #2E7D32; }
      .dataTables_wrapper { padding: 20px; background: white; border-radius: 8px; }
      .logout-btn { background-color: #dc3545; color: white; border: none; padding: 8px 20px; border-radius: 4px; cursor: pointer; }
      .logout-btn:hover { background-color: #c82333; }
      ul { padding-left: 20px; margin-bottom: 15px; } li { margin-bottom: 8px; line-height: 1.4; }
    "))
  ),
  
  uiOutput("page_content")
)

# ==============================================================================
# --- SERVER ---
# ==============================================================================
server <- function(input, output, session) {
  
  # Variables Reactivas
  user_logged <- reactiveVal(FALSE)
  user_name <- reactiveVal("")
  user_role <- reactiveVal("")
  datos_app <- reactiveVal(data.frame()) 
  
  # Carga inicial de datos
  observe({ datos_app(cargar_datos_bd()) })
  
  fila_seleccionada_id <- reactiveVal(NULL)
  
  # --- RENDERIZADOR DEL SENSOR (Lee la BD) ---
  output$ui_selector_sensor <- renderUI({
    con <- get_con()
    if(is.null(con)) return(NULL)
    # Traemos los sensores activos
    sensores <- dbGetQuery(con, "SELECT id, nombre FROM sensores WHERE estado = 'activo'")
    dbDisconnect(con)
    
    # Creamos el menú desplegable
    if(nrow(sensores) > 0) {
      lista_sensores <- setNames(sensores$id, sensores$nombre)
      selectInput("admin_sensor_elegido", "Sensor:", choices = lista_sensores)
    } else {
      p("No hay sensores activos en la BD", style="color:red")
    }
  })
  # --- UI RENDERER ---
  output$page_content <- renderUI({
    if (!user_logged()) {
      # ============================================
      # PÁGINA PRINCIPAL (CONCIENTIZACIÓN)
      # ============================================
      tagList(
        div(class = "hero-banner",
            fluidRow(class="header-content",
                     column(2, div(class = "logo-container", img(src = "https://univercimas.com/wp-content/uploads/2021/04/Escuela-Superior-Politecnica-de-Chimborazo-ESPOCH.png", class = "landing-img", alt = "ESPOCH"))),
                     column(8, h1(class = "hero-title", "MONITOREO DE DESPERDICIO ALIMENTARIO"), h3(class = "hero-subtitle", "Sistema de gestión y visualización del Banco de Alimentos del Mercado Mayorista")),
                     column(2, div(class = "logo-container", style = "text-align: left; margin-left: -50px;", img(src = "https://i.ibb.co/Ld4jFk0h/imagen-2025-12-11-234438516.png", class = "landing-img", alt = "Banco de Alimentos")))            ),
            br(),
            actionButton("btn_go_to_login", "ACCEDER AL SISTEMA", class = "login-button", icon = icon("sign-in-alt"))
        ),
        
        h2(class = "section-title", "Impacto Social y Ambiental"),
        
        div(class = "container-fluid",
            fluidRow(
              column(6, 
                     div(class = "chart-container", 
                         h3(class = "chart-title", "🍛 Platos de Comida Rescatados (Esta Semana)"), 
                         plotlyOutput("plot_publico_1", height = "300px"), 
                         div(class="chart-description", h4("¿Qué significa esto?"), "Convertimos los kilos rescatados en raciones reales de comida (1 plato = 0.4kg). Cada barra representa a familias alimentadas.")
                     )
              ),
              column(6, 
                     div(class = "chart-container", 
                         h3(class = "chart-title", "🌍 Contaminación Evitada (Kg CO2)"), 
                         plotlyOutput("plot_publico_2", height = "300px"), 
                         div(class="chart-description", h4("Conciencia Ambiental"), "El desperdicio genera gases. Este gráfico muestra cuántos Kg de CO2 hemos evitado liberar a la atmósfera al rescatar estos alimentos.")
                     )
              )
            ),
            
            h2(class = "section-title", "Proyecto Piloto: Gestión Sostenible"),
            fluidRow(
              column(6, div(class = "info-card", h3(icon("leaf"), "Misión"), p("Implementar tecnología para reducir el desperdicio."), tags$ul(tags$li("Digitalización del registro"), tags$li("Análisis predictivo"), tags$li("Optimización")))),
              column(6, div(class = "info-card", h3(icon("recycle"), "Valor"), p("El Banco de Alimentos rescata productos con valor nutricional:"), tags$ul(tags$li("Reducción de desperdicios"), tags$li("Recuperación constante"), tags$li("Beneficiarios reales"))))
            ),
            div(style = "text-align: center; margin: 30px 0;", actionButton("btn_go_to_login2", "INICIAR SESIÓN EN EL SISTEMA", class = "login-button", style = "font-size: 1.2rem; padding: 12px 35px;"))
        )
      )
    } else {
      # ============================================
      # DASHBOARD PRIVADO (OPERATIVO CLARO)
      # ============================================
      dashboardPage(
        skin = "green",
        header = dashboardHeader(title = "BAR Analytics", 
                                 tags$li(class="dropdown", style="padding:15px; font-weight:bold; color:white;", paste("Hola,", user_name())),
                                 tags$li(class="dropdown", actionLink("btn_logout", "Salir", icon=icon("sign-out-alt")))
        ),
        sidebar = dashboardSidebar(
          sidebarMenu(
            menuItem("Dashboard General", tabName = "dashboard", icon = icon("chart-pie")),
            menuItem("Base de Datos", tabName = "registros", icon = icon("database")),
            if(user_role() == "Administrador")menuItem("Administración CRUD", tabName = "admin", icon = icon("cogs")),
            if(user_role() == "Administrador") menuItem("Modelado Predictivo", tabName = "prediccion_avanzada", icon = icon("chart-line"))
          )
        ),
        body = dashboardBody(
          tabItems(
            # --- TAB DASHBOARD ---
            tabItem(tabName = "dashboard",
                    
                    div(class = "control-panel",
                        h4("Filtros de Análisis", style="margin-bottom: 15px; color: #2E7D32; font-weight: bold;"),
                        fluidRow(
                          column(4, dateRangeInput("dash_date_range", "Rango de Fechas:", start = Sys.Date()-365, end = Sys.Date(), separator = " a ")),
                          column(4, selectInput("dash_year", "Año Fiscal:", choices = c("Todos", 2023:2026), selected = "Todos")),
                          column(4, selectInput("dash_day", "Día Operativo:", choices = c("Todos", "lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo"), selected = "Todos"))
                        )
                    ),
                    
                    fluidRow(
                      valueBoxOutput("kpi_total", width = 3),
                      valueBoxOutput("kpi_promedio", width = 3),
                      valueBoxOutput("kpi_pico", width = 3),
                      valueBoxOutput("kpi_estado", width = 3)
                    ),
                    
                    # GRÁFICOS OPERATIVOS (RESTAURADOS Y MEJORADOS)
                    fluidRow(
                      # 1. Tendencia Horaria (Suma)
                      column(6,
                             div(class = "box",
                                 div(class="box-header", span("Tendencia Horaria del Volumen", class="box-title")),
                                 plotlyOutput("plot_line_time", height = "300px"),
                                 div(class="chart-footer", 
                                     strong("¿Qué muestra?"), " Sumatoria total de kilogramos recibidos a cada hora.",
                                     br(), "Esto indica la ", strong("capacidad de carga"), " necesaria para cada horario.")
                             )
                      ),
                      # 2. Distribución Franja (Con Noche)
                      column(6,
                             div(class = "box",
                                 div(class="box-header", span("Distribución por Franja (Mañana/Tarde/Noche)", class="box-title")),
                                 plotlyOutput("plot_donut_dist", height = "300px"),
                                 div(class="chart-footer", 
                                     strong("¿Qué muestra?"), " Porcentaje del peso total recolectado en cada turno.",
                                     br(), "Mañana (<12h), Tarde (12-17h), Noche (>17h).")
                             )
                      )
                    ),
                    fluidRow(
                      # 3. Volumen por Día (Sumatoria)
                      column(6,
                             div(class = "box",
                                 div(class="box-header", span("Volumen Total por Día de Semana", class="box-title")),
                                 plotlyOutput("plot_bar_css", height = "300px"),
                                 div(class="chart-footer", 
                                     strong("¿Qué muestra?"), " La suma histórica de todo el desperdicio agrupado por día.",
                                     br(), "Permite identificar los días de mayor afluencia de donaciones.")
                             )
                      ),
                      # 4. Evolución Mensual
                      column(6,
                             div(class = "box",
                                 div(class="box-header", span("Evolución Mensual del Desperdicio", class="box-title")),
                                 plotlyOutput("plot_line_month", height = "300px"),
                                 div(class="chart-footer", 
                                     strong("¿Qué muestra?"), " Comportamiento total mes a mes.",
                                     br(), "Útil para detectar estacionalidad (ej: meses de cosecha alta).")
                             )
                      )
                    )
            ),
            
            # --- TAB REGISTROS ---
            tabItem(tabName = "registros",
                    h2("Base de Datos Histórica"),
                    div(class="box",
                        div(class="box-header", span("Explorador de Datos", class="box-title")),
                        div(style="padding:15px;", 
                            DTOutput("tabla_registros_avanzada")
                        )
                    )
            ),
            
            # --- TAB ADMIN ---
            tabItem(tabName = "admin",
                    if(user_role() == "Administrador") {
                      fluidRow(
                        column(4,
                               div(class = "box",
                                   div(class="box-header", span("Formulario de Ingreso", class="box-title")),
                                   div(style="padding: 20px;",
                                       dateInput("admin_fecha", "Fecha:", value = Sys.Date()),
                                       selectInput("admin_hora", "Hora:", choices = sprintf("%02d:00", 7:18), selected = "09:00"),
                                       numericInput("admin_peso", "Peso (kg):", value = 0, min = 0, step = 0.1),
                                       uiOutput("ui_selector_sensor"),
                                       hr(),
                                       actionButton("btn_submit", "Guardar Registro", class = "btn-primary btn-block"),
                                       shinyjs::hidden(actionButton("btn_cancel", "Cancelar", class = "btn-default btn-block"))
                                   )
                               )
                        ),
                        column(8,
                               div(class = "box",
                                   div(class="box-header", span("Gestión de Registros", class="box-title")),
                                   div(style="padding: 15px;", 
                                       DTOutput("tabla_registros_admin_avanzada")
                                   )
                               )
                        )
                      )
                    }
            ),
            # --- TAB PREDICCIÓN AVANZADA (NUEVO) ---
            tabItem(tabName = "prediccion_avanzada",
                    if(user_role() == "Administrador") {
                      fluidPage(
                        h2("Módulo de Predicción y Análisis Avanzado"),
                        tabsetPanel(
                          # --- SUB-TAB 1: ANÁLISIS ---
                          tabPanel("Análisis de Datos", icon = icon("database"),
                                   br(),
                                   fluidRow(
                                     box(width = 12, title = "1. Configuración de Carga", status = "primary", solidHeader = TRUE,
                                         column(4, dateRangeInput("fechas_analisis", "Rango Histórico:", start = Sys.Date()-365, end = Sys.Date())),
                                         column(4, actionButton("btn_analizar", "Cargar Datos para Análisis", class = "btn-primary", style="margin-top: 25px; width: 100%;"))
                                     )
                                   ),
                                   fluidRow(
                                     box(width = 8, title = "Distribución y Outliers", status = "warning", plotlyOutput("plot_outliers", height = "300px")),
                                     box(width = 4, title = "Calidad Estadística", status = "warning", valueBoxOutput("vbox_shapiro_pct", width = 12), p("Interpretación:"), verbatimTextOutput("txt_shapiro_decision"))
                                   )
                          ),
                          # --- SUB-TAB 2: PREDICCIÓN ---
                          tabPanel("Proyección Futura", icon = icon("chart-line"),
                                   br(),
                                   fluidRow(
                                     column(width = 3,
                                            box(width = 12, title = "Modelos", status = "primary", solidHeader = TRUE,
                                                h4("Escenario A: Datos Limpios"),
                                                selectInput("modelo_normal_sel", NULL, choices = c("Regresión Lineal" = "lm", "Ridge Regression" = "ridge")),
                                                actionButton("btn_train_normal", "Proyectar Escenario A", class = "btn-warning btn-block"),
                                                hr(),
                                                h4("Escenario B: Datos Crudos"),
                                                selectInput("modelo_crudo_sel", NULL, choices = c("Random Forest" = "rf", "SVM" = "svm")),
                                                actionButton("btn_train_crudo", "Proyectar Escenario B", class = "btn-success btn-block"),
                                                hr(),
                                                selectInput("dash_anio", "Año Visualizado:", choices = NULL), 
                                                selectInput("dash_agrupacion", "Agrupar:", choices = c("Día"="day", "Semana"="week", "Mes"="month"))
                                            )
                                     ),
                                     column(width = 9,
                                            box(width = 12, title = "Comparativa", status = "primary", plotlyOutput("plot_prediccion_main", height = "450px")),
                                            fluidRow(
                                              valueBoxOutput("kpi_total_kg_A", width = 3),
                                              valueBoxOutput("kpi_escenario_A", width = 3),
                                              valueBoxOutput("kpi_total_kg_B", width = 3),
                                              valueBoxOutput("kpi_escenario_B", width = 3)
                                            )                                     )
                                   )
                          ),
                          # --- SUB-TAB 3: CLUSTERS ---
                          tabPanel("Patrones (Clusters)", icon = icon("project-diagram"),
                                   br(),
                                   fluidRow(
                                     column(width = 3,
                                            box(width = 12, title = "Parámetros", status = "info", solidHeader = TRUE,
                                                selectInput("cluster_algo", "Algoritmo:", choices = c("K-Means"="kmeans", "Jerárquico"="hclust")),
                                                sliderInput("num_clusters", "Número de Clusters (k):", 2, 6, 3),
                                                actionButton("btn_calc_cluster", "Detectar Patrones", class = "btn-info btn-block")
                                            )
                                     ),
                                     column(width = 9,
                                            box(width = 12, title = "Visualización", status = "info", plotlyOutput("plot_clusters", height = "400px")),
                                            box(width = 12, title = "Resumen", tableOutput("tabla_resumen_cluster"))
                                     )
                                   )
                          )
                        )
                      )
                    }
            )
          )
        )
      )
    }
  })
  
  # --- LOGICA LOGIN ---
  observeEvent(input$btn_go_to_login, { showModal(loginModal()) })
  observeEvent(input$btn_go_to_login2, { showModal(loginModal()) })
  
  loginModal <- function() {
    modalDialog(
      title = "Iniciar Sesión",
      textInput("login_user", "Usuario"), passwordInput("login_pass", "Contraseña"),
      footer = tagList(modalButton("Cancelar"), actionButton("do_login", "Ingresar", class = "btn-primary")),
      size = "s", easyClose = TRUE
    )
  }
  # Función auxiliar para consultar usuario y rol en la BD
  verificar_usuario_bd <- function(user_input, pass_input) {
    con <- get_con()
    if(is.null(con)) return(NULL)
    
    # Hacemos JOIN entre usuarios y roles para obtener el nombre del rol (ej: Administrador)
    query <- sprintf("SELECT u.username, r.nombre as rol 
                      FROM usuarios u 
                      JOIN roles r ON u.rol_id = r.id 
                      WHERE u.username = '%s' AND u.password_hash = '%s'", 
                     user_input, pass_input)
    
    # Ejecutamos la consulta
    df_user <- tryCatch({
      dbGetQuery(con, query)
    }, error = function(e) return(data.frame()))
    
    dbDisconnect(con)
    return(df_user)
  }
  
  observeEvent(input$do_login, {
    req(input$login_user, input$login_pass)
    
    # 1. Consultamos a la base de datos real
    datos_usuario <- verificar_usuario_bd(input$login_user, input$login_pass)
    
    # 2. Verificamos si la base de datos devolvió algún resultado
    if (!is.null(datos_usuario) && nrow(datos_usuario) > 0) {
      user_logged(TRUE)
      user_name(datos_usuario$username[1])
      user_role(datos_usuario$rol[1]) # Esto tomará "Administrador" o "Mayorista" desde la BD
      removeModal()
      showNotification("Acceso Correcto", type="message")
    } else {
      showNotification("Error de credenciales (BD)", type="error")
    }
  })
  observeEvent(input$btn_logout, { user_logged(FALSE); user_name(""); user_role("") })
  
  # --- FILTRADO DE DATOS (DASHBOARD) ---
  datos_filtrados <- reactive({
    req(datos_app())
    if(nrow(datos_app()) == 0) return(data.frame())
    
    df <- datos_app() %>% filter(estado == "valido")
    
    # Filtros
    if(!is.null(input$dash_date_range)) {
      df <- df %>% filter(fecha >= input$dash_date_range[1] & fecha <= input$dash_date_range[2])
    }
    if(input$dash_year != "Todos") {
      df <- df %>% filter(anio == input$dash_year)
    }
    if(input$dash_day != "Todos") {
      df <- df %>% filter(tolower(as.character(dia_semana)) == input$dash_day)
    }
    return(df)
  })
  
  # --- KPIs OPERATIVOS ---
  output$kpi_total <- renderValueBox({
    val <- sum(datos_filtrados()$peso, na.rm=T)
    valueBox(paste(format(val, big.mark=","), "kg"), "Kg Desperdicio (Total)", icon=icon("weight-hanging"), color="green")
  })
  
  output$kpi_promedio <- renderValueBox({
    val <- mean(datos_filtrados()$peso, na.rm=T)
    val <- ifelse(is.nan(val), 0, val)
    valueBox(paste(round(val, 1), "kg"), "Promedio por Entrega", icon=icon("balance-scale"), color="olive")
  })
  
  output$kpi_pico <- renderValueBox({
    df <- datos_filtrados()
    txt <- "--"
    if(nrow(df)>0) {
      # Sumamos por hora para ver la hora con más volumen
      p <- df %>% group_by(hora_entera) %>% summarise(s=sum(peso)) %>% top_n(1, s)
      if(nrow(p)>0) txt <- paste0(p$hora_entera[1], ":00")
    }
    valueBox(txt, "Hora Pico (Volumen)", icon=icon("clock"), color="yellow")
  })
  
  output$kpi_estado <- renderValueBox({
    valueBox(nrow(datos_filtrados()), "N° Registros Seleccionados", icon=icon("list"), color="blue")
  })
  
  # --- GRÁFICOS OPERATIVOS (DASHBOARD PRIVADO) ---
  
  # 1. TENDENCIA HORARIA (SUMA DE KG)
  output$plot_line_time <- renderPlotly({
    df <- datos_filtrados()
    if(nrow(df)==0) return(NULL)
    
    # Agrupamos sumando para ver volumen total
    df_g <- df %>% group_by(hora_entera) %>% summarise(total = sum(peso))
    
    plot_ly(df_g, x = ~hora_entera, y = ~total, type = 'scatter', mode = 'lines+markers',
            line = list(color = '#2E7D32', width = 3), marker = list(color = '#1b5e20', size = 8)) %>%
      layout(xaxis = list(title = "Hora del Día"), yaxis = list(title = "Suma Kg Recibidos"), 
             margin = list(l=40, r=20, t=20, b=30))
  })
  
  # 2. DISTRIBUCIÓN FRANJA (CON NOCHE)
  output$plot_donut_dist <- renderPlotly({
    df <- datos_filtrados()
    if(nrow(df)==0) return(NULL)
    
    # Lógica actualizada con Noche
    df <- df %>% mutate(Franja = case_when(
      hora_entera < 12 ~ "Mañana",
      hora_entera < 17 ~ "Tarde",
      TRUE ~ "Noche"
    ))
    df_g <- df %>% group_by(Franja) %>% summarise(count = n()) # O sum(peso) si prefieres volumen
    
    # Colores: Mañana(Verde), Tarde(Naranja), Noche(Azul oscuro)
    colores <- c("Mañana"="#43A047", "Tarde"="#FFB74D", "Noche"="#1565C0")
    
    plot_ly(df_g, labels = ~Franja, values = ~count, type = 'pie', hole = 0.6,
            marker = list(colors = unname(colores[df_g$Franja]))) %>%
      layout(showlegend = TRUE, margin = list(l=20, r=20, t=20, b=20))
  })
  
  # 3. VOLUMEN POR DÍA (SUMATORIA)
  output$plot_bar_css <- renderPlotly({
    df <- datos_filtrados()
    if(nrow(df)==0) return(NULL)
    
    # Suma total por día
    df_g <- df %>% group_by(dia_semana) %>% summarise(total = sum(peso))
    
    plot_ly(df_g, x = ~dia_semana, y = ~total, type = 'bar', marker = list(color = '#66BB6A')) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Total Kg Acumulados"), margin = list(l=40, r=20, t=20, b=30))
  })
  
  # 4. EVOLUCIÓN MENSUAL
  output$plot_line_month <- renderPlotly({
    df <- datos_filtrados()
    if(nrow(df)==0) return(NULL)
    
    df_g <- df %>% group_by(mes_anio) %>% summarise(total = sum(peso))
    
    plot_ly(df_g, x = ~mes_anio, y = ~total, type = 'scatter', mode = 'lines+markers',
            line = list(color = '#1E88E5', width = 3)) %>%
      layout(xaxis = list(title = "Mes"), yaxis = list(title = "Kg"), margin = list(l=40, r=20, t=20, b=30))
  })
  
  # --- GRÁFICOS PÚBLICOS (PANTALLA INICIO) ---
  
  # 1. PLATOS DE COMIDA (Social) - ESTA SEMANA
  output$plot_publico_1 <- renderPlotly({
    df <- datos_app() %>% filter(estado == "valido")
    if(nrow(df)==0) return(NULL)
    
    # Filtro semana actual
    df_sem <- df %>% filter(fecha >= floor_date(Sys.Date(), "week"))
    if(nrow(df_sem) == 0) df_sem <- df # Si no hay, mostrar todo para demo
    
    df_g <- df_sem %>% group_by(dia_semana) %>% summarise(platos = round(sum(peso) / 0.4))
    
    plot_ly(df_g, x = ~dia_semana, y = ~platos, type = 'bar', marker = list(color='#FFA726')) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Raciones Servidas"), 
             margin = list(t=10, b=10))
  })
  
  # 2. HUELLA CO2 (Ambiental) - ACUMULADO
  output$plot_publico_2 <- renderPlotly({
    df <- datos_app() %>% filter(estado == "valido")
    if(nrow(df)==0) return(NULL)
    
    df_g <- df %>% group_by(mes_anio) %>% summarise(co2 = sum(peso) * 2.5)
    
    plot_ly(df_g, x = ~mes_anio, y = ~co2, type = 'scatter', mode='lines+markers', fill='tozeroy', 
            line=list(color='#2E7D32'), fillcolor='rgba(46, 125, 50, 0.2)') %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Kg CO2 Evitado"),
             margin = list(t=10, b=10))
  })
  
  # --- TABLA AVANZADA (PESTAÑA REGISTROS - PÚBLICO) ---
  output$tabla_registros_avanzada <- renderDT({
    df <- datos_app() %>% filter(estado == "valido") %>% select(Fecha=fecha, Hora=hora, Peso_kg=peso, Dia=dia_semana)
    datatable(df, rownames = FALSE, filter = 'top', options = list(pageLength = 10, dom = 'lrtip', autoWidth = TRUE, language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')), class = 'cell-border stripe')
  })
  
  # --- TABLA AVANZADA (PESTAÑA ADMIN - CRUD) ---
  output$tabla_registros_admin_avanzada <- renderDT({
    df <- datos_app()
    if(nrow(df) > 0) {
      df <- df %>% mutate(
        Acciones = paste0(
          '<button class="btn btn-warning btn-xs" onclick="Shiny.setInputValue(\'edit_id\', ', row_id, ', {priority: \'event\'})">✏️</button> ',
          ifelse(estado == 'valido',
                 paste0('<button class="btn btn-danger btn-xs" onclick="Shiny.setInputValue(\'anular_id\', ', row_id, ', {priority: \'event\'})">🗑️</button>'),
                 paste0('<button class="btn btn-info btn-xs" onclick="Shiny.setInputValue(\'restore_id\', ', row_id, ', {priority: \'event\'})">♻️</button>')
          )
        )
      ) %>% select(Fecha=fecha, Hora=hora, Peso=peso, Estado=estado, Acciones)
      
      datatable(df, escape = FALSE, selection = "none", filter = 'top', options = list(pageLength = 5, dom = 'lrtip', language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')), class = 'cell-border stripe')
    }
  })
  
  # --- LÓGICA CRUD ---
  observeEvent(input$btn_submit, {
    req(input$admin_fecha, input$admin_hora, input$admin_peso, input$admin_sensor_elegido) # Agregamos el sensor aquí
    con <- get_con()
    tryCatch({
      if(is.null(fila_seleccionada_id())) {
        # INSERTAR: Usamos el sensor elegido en lugar del "1"
        query <- sprintf("INSERT INTO pesos (fecha, hora, peso, estado, sensor_id) VALUES ('%s', '%s', %s, 'valido', %s)",
                         input$admin_fecha, input$admin_hora, input$admin_peso, input$admin_sensor_elegido)
        dbExecute(con, query)
        showNotification("Registro Guardado", type="success")
      } else {
        # EDITAR (UPDATE)
        query <- sprintf("UPDATE pesos SET fecha='%s', hora='%s', peso=%s, sensor_id=%s WHERE id=%s",
                         input$admin_fecha, input$admin_hora, input$admin_peso, input$admin_sensor_elegido, fila_seleccionada_id())
        dbExecute(con, query)
        showNotification("Registro Actualizado", type="message")
      }
    }, finally = {
      dbDisconnect(con)
      datos_app(cargar_datos_bd())
      fila_seleccionada_id(NULL)
      updateNumericInput(session, "admin_peso", value=0)
      shinyjs::hide("btn_cancel")
      updateActionButton(session, "btn_submit", label="Guardar Registro")
    })
  })
  
  observeEvent(input$edit_id, {
    id <- input$edit_id
    df <- datos_app() %>% filter(row_id == id)
    fila_seleccionada_id(id)
    updateDateInput(session, "admin_fecha", value=df$fecha)
    updateSelectInput(session, "admin_hora", selected=substr(df$hora, 1, 5))
    updateNumericInput(session, "admin_peso", value=df$peso)
    updateActionButton(session, "btn_submit", label="Actualizar")
    shinyjs::show("btn_cancel")
  })
  
  observeEvent(input$btn_cancel, {
    fila_seleccionada_id(NULL)
    updateNumericInput(session, "admin_peso", value=0)
    updateActionButton(session, "btn_submit", label="Guardar Registro")
    shinyjs::hide("btn_cancel")
  })
  
  cambiar_estado_bd <- function(id, nuevo_estado) {
    con <- get_con()
    dbExecute(con, paste0("UPDATE pesos SET estado='", nuevo_estado, "' WHERE id=", id))
    dbDisconnect(con)
    datos_app(cargar_datos_bd())
  }
  
  observeEvent(input$anular_id, { cambiar_estado_bd(input$anular_id, "anulado") })
  observeEvent(input$restore_id, { cambiar_estado_bd(input$restore_id, "valido") })
  
  # ==============================================================================
  # --- LÓGICA DE PREDICCIÓN Y CLUSTERS ---
  # ==============================================================================
  
  # Variables Reactivas para Predicción
  datos_base_pred <- reactiveVal(NULL)
  predicciones <- reactiveValues(normal = NULL, crudo = NULL) 
  info_modelos <- reactiveValues(nombre_normal = "", nombre_crudo = "")
  datos_cluster_res <- reactiveVal(NULL)
  
  # --- CARGA DE DATOS PARA ANÁLISIS ---
  observeEvent(input$btn_analizar, {
    req(input$fechas_analisis)
    con <- get_con()
    if(is.null(con)) { showNotification("Error Conexión BD", type="error"); return() }
    
    df <- dbGetQuery(con, "SELECT fecha, hora, peso FROM pesos WHERE estado='valido' ORDER BY fecha, hora")
    dbDisconnect(con)
    
    df$fecha <- as.Date(df$fecha)
    df <- df %>% filter(fecha >= input$fechas_analisis[1], fecha <= input$fechas_analisis[2])
    
    if(nrow(df) > 0) {
      datos_base_pred(df)
      showNotification("Datos cargados para análisis", type="message")
    } else {
      showNotification("No hay datos en ese rango", type="warning")
    }
    
    output$plot_outliers <- renderPlotly({
      plot_ly(y = df$peso, type = "box", name = "Historico", 
              marker = list(color = 'red'), line = list(color = 'blue')) %>%
        layout(yaxis = list(title = "Peso (kg)"), xaxis = list(title = ""))
    })
    
    shapiro <- shapiro.test(sample(df$peso, min(nrow(df), 5000)))
    pval <- shapiro$p.value
    output$vbox_shapiro_pct <- renderValueBox({
      color <- if(pval > 0.05) "green" else "red"
      val_txt <- paste0(round(shapiro$statistic*100, 1), "%")
      valueBox(val_txt, "Normalidad", icon=icon("chart-bar"), color=color)
    })
    output$txt_shapiro_decision <- renderText({
      if(pval > 0.05) "RESULTADO: Datos Normales -> Usar Modelo A" else "RESULTADO: Datos Atípicos -> Usar Modelo B"
    })
  })
  
  # --- FUNCIÓN RECURSIVA DE ENTRENAMIENTO ---
  # --- FUNCIÓN RECURSIVA DE ENTRENAMIENTO CON MÉTRICAS ---
  entrenar_recursivo <- function(df, tipo_modelo, algoritmo) {
    df_train <- df %>% arrange(fecha, hora) %>%
      mutate(hora_entera = as.numeric(substr(hora, 1, 2)), 
             dia_semana = wday(fecha, week_start=1),
             peso_ant = lag(peso, 1, default = mean(peso, na.rm=T))) %>% 
      filter(!is.na(peso_ant))
    
    if(tipo_modelo == "normal") df_train$peso[is.na(df_train$peso)] <- mean(df_train$peso, na.rm=T)
    else df_train$peso[is.na(df_train$peso)] <- median(df_train$peso, na.rm=T)
    
    modelo <- NULL
    # Entrenamiento y cálculo de R2 inicial
    if(algoritmo == "lm") {
      modelo <- lm(peso ~ hora_entera + dia_semana + peso_ant, data = df_train)
      r2_val <- summary(modelo)$r.squared
    } else if (algoritmo == "rf") {
      modelo <- randomForest(peso ~ hora_entera + dia_semana + peso_ant, data = df_train, ntree=100)
      r2_val <- max(0, cor(df_train$peso, predict(modelo))^2)
    } else {
      modelo <- lm(peso ~ hora_entera + dia_semana + peso_ant, data = df_train)
      r2_val <- summary(modelo)$r.squared
    }
    
    # Proyección (Lógica original mantenida)
    ultima_fecha <- max(df$fecha); fechas_futuras <- seq(ultima_fecha + 1, ultima_fecha + 1095, by="day")
    futuro <- data.frame(fecha = fechas_futuras, hora_entera = 11, dia_semana = wday(fechas_futuras, week_start=1), pred = NA, peso_ant_sim = NA)
    ultimo_peso <- tail(df_train$peso, 1)
    
    for(i in 1:nrow(futuro)) {
      futuro$peso_ant_sim[i] <- ultimo_peso
      newdata <- data.frame(hora_entera = futuro$hora_entera[i], dia_semana = futuro$dia_semana[i], peso_ant = futuro$peso_ant_sim[i])
      p <- predict(modelo, newdata)
      futuro$pred[i] <- max(0, as.numeric(p))
      ultimo_peso <- futuro$pred[i] * runif(1, 0.95, 1.05)
    }
    
    return(list(proyeccion = futuro, r2 = r2_val, r = sqrt(r2_val)))
  }
  
  # Variables reactivas para guardar métricas
  metricas <- reactiveValues(r2_A = 0, r_A = 0, r2_B = 0, r_B = 0)
  
  observeEvent(input$btn_train_normal, {
    req(datos_base_pred())
    withProgress(message = 'Calculando Modelo A...', {
      res <- entrenar_recursivo(datos_base_pred(), "normal", input$modelo_normal_sel)
      predicciones$normal <- res$proyeccion
      metricas$r2_A <- res$r2
      metricas$r_A <- res$r
      info_modelos$nombre_normal <- paste("Modelo A:", input$modelo_normal_sel)
      updateSelectInput(session, "dash_anio", choices = sort(unique(year(res$proyeccion$fecha))))
    })
  })
  
  observeEvent(input$btn_train_crudo, {
    req(datos_base_pred())
    withProgress(message = 'Calculando Modelo B...', {
      res <- entrenar_recursivo(datos_base_pred(), "crudo", input$modelo_crudo_sel)
      predicciones$crudo <- res$proyeccion
      metricas$r2_B <- res$r2
      metricas$r_B <- res$r
      info_modelos$nombre_crudo <- paste("Modelo B:", input$modelo_crudo_sel)
      updateSelectInput(session, "dash_anio", choices = sort(unique(year(res$proyeccion$fecha))))
    })
  })
  
  output$plot_prediccion_main <- renderPlotly({
    if(is.null(predicciones$normal) && is.null(predicciones$crudo)) return(NULL)
    p <- plot_ly() %>% layout(legend = list(orientation = "h", x = 0.1, y = 1.1), yaxis = list(title = "Kg Proyectados"))
    anio_sel <- as.numeric(input$dash_anio)
    agrupar <- function(d) d %>% filter(year(fecha)==anio_sel) %>% group_by(fecha=floor_date(fecha, input$dash_agrupacion)) %>% summarise(total=sum(pred))
    
    if(!is.null(predicciones$normal)) {
      df_n <- agrupar(predicciones$normal)
      p <- p %>% add_lines(x=df_n$fecha, y=df_n$total, name=info_modelos$nombre_normal, line=list(color='orange', width=3))
    }
    if(!is.null(predicciones$crudo)) {
      df_c <- agrupar(predicciones$crudo)
      p <- p %>% add_lines(x=df_c$fecha, y=df_c$total, name=info_modelos$nombre_crudo, line=list(color='green', width=3, dash='dot'))
    }
    p
  })
  
  # --- CUADROS MODELO A ---
  output$kpi_total_kg_A <- renderValueBox({
    val <- if(!is.null(predicciones$normal)) sum(predicciones$normal$pred[year(predicciones$normal$fecha)==as.numeric(input$dash_anio)]) else 0
    valueBox(paste(format(round(val,0), big.mark=","),"kg"), "Total Proyectado A", icon=icon("calculator"), color="orange")
  })
  
  output$kpi_escenario_A <- renderValueBox({
    val_r2 <- round(metricas$r2_A * 100, 1)
    val_r <- round(metricas$r_A * 100, 1)
    valueBox(paste0(val_r2, "%"), HTML(paste0("Eficiencia A (R²)<br><small>Raíz R: ", val_r, "%</small>")), icon=icon("tachometer-alt"), color="orange")
  })
  
  # --- CUADROS MODELO B ---
  output$kpi_total_kg_B <- renderValueBox({
    val <- if(!is.null(predicciones$crudo)) sum(predicciones$crudo$pred[year(predicciones$crudo$fecha)==as.numeric(input$dash_anio)]) else 0
    valueBox(paste(format(round(val,0), big.mark=","),"kg"), "Total Proyectado B", icon=icon("tree"), color="green")
  })
  
  output$kpi_escenario_B <- renderValueBox({
    val_r2 <- round(metricas$r2_B * 100, 1)
    val_r <- round(metricas$r_B * 100, 1)
    valueBox(paste0(val_r2, "%"), HTML(paste0("Eficiencia B (R²)<br><small>Raíz R: ", val_r, "%</small>")), icon=icon("chart-line"), color="green")
  })
  
  # --- CLUSTERS ---
  observeEvent(input$btn_calc_cluster, {
    req(datos_base_pred())
    df <- datos_base_pred() %>% mutate(hora_entera=as.numeric(substr(hora,1,2))) %>% select(hora_entera, peso) %>% na.omit()
    df_scaled <- scale(df)
    clusters <- if(input$cluster_algo=="kmeans") kmeans(df_scaled, centers=input$num_clusters)$cluster else cutree(hclust(dist(df_scaled), method="ward.D2"), k=input$num_clusters)
    df$Cluster <- as.factor(clusters)
    datos_cluster_res(df)
  })
  
  output$plot_clusters <- renderPlotly({
    req(datos_cluster_res())
    plot_ly(datos_cluster_res(), x=~hora_entera, y=~peso, color=~Cluster, type="scatter", mode="markers", marker=list(size=10, opacity=0.8))
  })
  
  output$tabla_resumen_cluster <- renderTable({
    req(datos_cluster_res())
    datos_cluster_res() %>% group_by(Cluster) %>% summarise(Cant=n(), Promedio_Kg=mean(peso), Hora_Prom=mean(hora_entera))
  }, width="100%")
}

shinyApp(ui, server)