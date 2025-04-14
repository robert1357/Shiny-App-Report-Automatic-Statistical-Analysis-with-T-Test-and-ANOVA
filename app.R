library(shiny)
library(readxl)
library(ggplot2)
library(dplyr)
library(gridExtra) # Para combinar múltiples gráficos

ui <- fluidPage(
  titlePanel("Análisis T-Test y ANOVA Automático"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("archivo", "Sube un archivo CSV o Excel", 
                accept = c(".csv", ".xlsx")),
      
      checkboxInput("encabezado", "El archivo tiene encabezados", TRUE),
      
      uiOutput("var_depen"),
      uiOutput("var_grupo"),
      uiOutput("opciones_grafico"),
      
      selectInput("tipo_grafico", "Tipo de visualización:",
                  choices = c("Boxplot" = "boxplot",
                              "Campana de Gauss con regiones" = "gauss",
                              "Densidad" = "densidad",
                              "Violín" = "violin",
                              "Barras (media)" = "barras",
                              "Dispersión" = "scatter",
                              "Todos" = "todos")),
      
      sliderInput("alpha", "Nivel de significancia (α):", 
                  min = 0.01, max = 0.1, value = 0.05, step = 0.01),
      
      checkboxGroupInput("extras", "Mostrar:",
                         choices = c("Tabla resumen" = "resumen",
                                     "Tabla de medias por grupo" = "medias",
                                     "Prueba de normalidad" = "normalidad",
                                     "Prueba de homogeneidad de varianzas" = "varianza")),
      
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Análisis", 
                 verbatimTextOutput("resultado"),
                 plotOutput("grafico", height = "600px"),
                 verbatimTextOutput("prueba_normalidad"),
                 verbatimTextOutput("prueba_varianza")),
        tabPanel("Datos",
                 tableOutput("tabla_resumen"),
                 tableOutput("tabla_medias"),
                 dataTableOutput("datos_crudos"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  datos <- reactive({
    req(input$archivo)
    ext <- tools::file_ext(input$archivo$name)
    
    tryCatch({
      if (ext == "csv") {
        df <- read.csv(input$archivo$datapath, header = input$encabezado, stringsAsFactors = FALSE)
      } else if (ext == "xlsx") {
        df <- read_excel(input$archivo$datapath, col_names = input$encabezado)
      } else {
        showNotification("Formato no soportado. Usa CSV o XLSX.", type = "error")
        return(NULL)
      }
      
      validate(need(nrow(df) > 0, "El archivo está vacío."))
      # Convertir automáticamente columnas con pocos valores únicos a factores
      for (col in names(df)) {
        if (!is.numeric(df[[col]]) || length(unique(df[[col]])) <= 10) {
          df[[col]] <- as.factor(df[[col]])
        }
      }
      df
    }, error = function(e) {
      showNotification(paste("Error al cargar el archivo:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # Detectar automáticamente variables numéricas y categóricas
  columnas_numericas <- reactive({
    req(datos())
    sapply(datos(), is.numeric)
  })
  
  columnas_categoricas <- reactive({
    req(datos())
    sapply(datos(), function(x) is.factor(x) || is.character(x) || is.logical(x))
  })
  
  output$var_depen <- renderUI({
    req(datos())
    num_cols <- names(datos())[columnas_numericas()]
    selectInput("var_dep", "Variable dependiente (numérica):",
                choices = num_cols)
  })
  
  output$var_grupo <- renderUI({
    req(datos())
    cat_cols <- names(datos())[columnas_categoricas()]
    selectInput("var_grup", "Variables independientes (categóricas):",
                choices = cat_cols, multiple = TRUE)
  })
  output$opciones_grafico <- renderUI({
    req(datos(), input$var_dep, input$var_grup)
    
    df <- datos()
    grupo_vars <- input$var_grup
    
    # Verificar si es un caso de t-test (1 variable independiente con 2 niveles)
    es_ttest <- FALSE
    if (length(grupo_vars) == 1) {
      if (length(unique(df[[grupo_vars[1]]])) == 2) {
        es_ttest <- TRUE
      }
    }
    
    # Opciones de gráficos según el tipo de prueba
    if (es_ttest) {
      # Para t-test, incluir opción de campana de Gauss
      selectInput("tipo_grafico", "Tipo de visualización:",
                  choices = c("Boxplot" = "boxplot",
                              "Campana de Gauss con regiones" = "gauss",
                              "Densidad" = "densidad",
                              "Violín" = "violin",
                              "Barras (media)" = "barras",
                              "Dispersión" = "scatter",
                              "Todos" = "todos"))
    } else if (length(grupo_vars) >= 2) {
      # Para ANOVA con 2+ factores, incluir opción de interacción
      selectInput("tipo_grafico", "Tipo de visualización:",
                  choices = c("Boxplot" = "boxplot",
                              "Interacción" = "interaccion",
                              "Densidad" = "densidad",
                              "Violín" = "violin",
                              "Barras (media)" = "barras",
                              "Dispersión" = "scatter",
                              "Todos" = "todos"))
    } 
  })
  crear_interaccion <- function(df, var_dep, var_grup) {
    # Asegurarse de que hay exactamente dos variables de grupo
    if (length(var_grup) != 2) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                        label = "El gráfico de interacción requiere\nexactamente dos factores", size = 5) +
               theme_void())
    }
    
    # Calcular medias por grupos
    medias <- df %>%
      group_by(across(all_of(var_grup))) %>%
      summarise(Media = mean(.data[[var_dep]], na.rm = TRUE),
                EE = sd(.data[[var_dep]], na.rm = TRUE) / sqrt(n()),
                .groups = "drop")
    
    # Crear gráfico de interacción
    ggplot(medias, aes_string(x = var_grup[1], y = "Media", color = var_grup[2], group = var_grup[2])) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      geom_errorbar(aes(ymin = Media - EE, ymax = Media + EE), width = 0.2) +
      theme_minimal() +
      labs(title = paste("Gráfico de interacción para", var_dep),
           subtitle = paste("Factores:", paste(var_grup, collapse = " × ")),
           y = paste("Media de", var_dep)) +
      theme(legend.position = "right",
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5))
  }
  # Análisis estadístico
  resultado_analisis <- reactive({
    req(datos(), input$var_dep, input$var_grup)
    
    df <- datos()
    y <- df[[input$var_dep]]
    grupo_vars <- input$var_grup
    
    validate(
      need(is.numeric(y), "La variable dependiente debe ser numérica."),
      need(length(grupo_vars) >= 1, "Selecciona al menos una variable independiente.")
    )
    
    for (var in grupo_vars) {
      df[[var]] <- as.factor(df[[var]])
    }
    
    formula_txt <- paste(input$var_dep, "~", paste(grupo_vars, collapse = " + "))
    formula <- as.formula(formula_txt)
    
    resultado <- list()
    resultado$alpha <- input$alpha
    
    if (length(grupo_vars) == 1 && length(unique(df[[grupo_vars[1]]])) == 2) {
      # Realizar prueba t
      resultado$tipo <- "t-test"
      resultado$test <- t.test(formula, data = df)
      resultado$p_valor <- resultado$test$p.value
      resultado$interpretacion <- if (resultado$p_valor < resultado$alpha) {
        paste0("❌ Se rechaza la hipótesis nula con α = ", resultado$alpha, ": hay diferencia significativa entre los grupos.")
      } else {
        paste0("✅ No se rechaza la hipótesis nula con α = ", resultado$alpha, ": no hay diferencia significativa.")
      }
      
      # Detalles para graficar la distribución t
      resultado$grados_libertad <- resultado$test$parameter
      resultado$estadistico <- resultado$test$statistic
      resultado$valor_critico <- qt(1 - resultado$alpha/2, df = resultado$grados_libertad)
      
    } else {
      # Realizar ANOVA
      resultado$tipo <- "anova"
      resultado$modelo <- aov(formula, data = df)
      resultado$test <- summary(resultado$modelo)
      resultado$p_valor <- summary(resultado$modelo)[[1]][["Pr(>F)"]][1]
      resultado$interpretacion <- if (!is.na(resultado$p_valor) && resultado$p_valor < resultado$alpha) {
        paste0("❌ Se rechaza la hipótesis nula con α = ", resultado$alpha, ": al menos un grupo difiere significativamente.")
      } else {
        paste0("✅ No se rechaza la hipótesis nula con α = ", resultado$alpha, ": no hay diferencias significativas entre grupos.")
      }
      
      # Detalles para graficar la distribución F
      resultado$grado_libertad_1 <- resultado$test[[1]]["Df"][1,1]
      resultado$grado_libertad_2 <- resultado$test[[1]]["Df"][2,1]
      resultado$estadistico <- resultado$test[[1]]["F value"][1,1]
      resultado$valor_critico <- qf(1 - resultado$alpha, 
                                    df1 = resultado$grado_libertad_1, 
                                    df2 = resultado$grado_libertad_2)
      
      # Si ANOVA es significativo, realizar post-hoc
      if (!is.na(resultado$p_valor) && resultado$p_valor < resultado$alpha) {
        resultado$posthoc <- TukeyHSD(resultado$modelo, conf.level = 1 - resultado$alpha)
      }
    }
    
    resultado$datos <- df
    resultado$formula <- formula
    resultado$var_dep <- input$var_dep
    resultado$var_grup <- grupo_vars
    
    return(resultado)
  })
  
  output$resultado <- renderPrint({
    req(resultado_analisis())
    
    res <- resultado_analisis()
    
    if (res$tipo == "t-test") {
      print(res$test)
      
      cat("\nValores críticos para α =", res$alpha, ":\n")
      cat("t crítico: ±", round(res$valor_critico, 4), "\n")
      cat("t calculado:", round(res$estadistico, 4), "\n\n")
      
    } else {
      print(res$test)
      
      cat("\nValores críticos para α =", res$alpha, ":\n")
      cat("F crítico:", round(res$valor_critico, 4), "\n")
      cat("F calculado:", round(res$estadistico, 4), "\n\n")
      
      if (!is.na(res$p_valor) && res$p_valor < res$alpha) {
        cat("\nAnálisis Post-hoc (Tukey HSD):\n")
        print(res$posthoc)
      }
    }
    
    cat("\nInterpretación:\n")
    cat(res$interpretacion)
  })
  
  # Pruebas de supuestos
  output$prueba_normalidad <- renderPrint({
    req("normalidad" %in% input$extras, resultado_analisis())
    
    res <- resultado_analisis()
    df <- res$datos
    
    cat("Prueba de normalidad (Shapiro-Wilk):\n")
    
    # Hacemos la prueba por grupos
    for (var in res$var_grup) {
      cat("\nPor grupos de", var, ":\n")
      grupos <- unique(df[[var]])
      
      for (g in grupos) {
        muestra <- df[df[[var]] == g, res$var_dep]
        if (length(muestra) < 3) {
          cat("Grupo", g, ": Insuficientes datos para prueba (n<3)\n")
        } else if (length(muestra) <= 5000) {  # Shapiro-Wilk solo funciona hasta n=5000
          test <- shapiro.test(muestra)
          cat("Grupo", g, ": W =", round(test$statistic, 4), 
              ", p-valor =", round(test$p.value, 4), 
              ifelse(test$p.value > 0.05, "✅ Normal", "❌ No normal"), "\n")
        } else {
          cat("Grupo", g, ": Demasiados datos para Shapiro-Wilk (n>5000)\n")
        }
      }
    }
  })
  
  output$prueba_varianza <- renderPrint({
    req("varianza" %in% input$extras, resultado_analisis())
    
    res <- resultado_analisis()
    
    if (length(res$var_grup) == 1) {
      cat("Prueba de homogeneidad de varianzas (Levene):\n")
      # Requiere el paquete car
      if (!requireNamespace("car", quietly = TRUE)) {
        cat("Paquete 'car' no disponible. Instale con install.packages('car')")
      } else {
        tryCatch({
          test <- car::leveneTest(res$formula, data = res$datos)
          print(test)
          if (test$`Pr(>F)`[1] > 0.05) {
            cat("\n✅ Varianzas homogéneas (p > 0.05)")
          } else {
            cat("\n❌ Varianzas no homogéneas (p < 0.05)")
          }
        }, error = function(e) {
          cat("Error en la prueba de Levene:", e$message)
        })
      }
    } else {
      cat("La prueba de homogeneidad de varianzas está disponible solo con una variable independiente.")
    }
  })
  
  # Funciones para generar diferentes tipos de gráficos
  crear_boxplot <- function(df, var_dep, var_grup) {
    if (length(var_grup) == 1) {
      ggplot(df, aes_string(x = var_grup[1], y = var_dep, fill = var_grup[1])) +
        geom_boxplot() +
        geom_jitter(width = 0.2, alpha = 0.3) +  # Añadir puntos
        theme_minimal() +
        labs(title = paste("Boxplot de", var_dep, "por", var_grup[1]), 
             y = var_dep, x = var_grup[1]) +
        theme(legend.position = "none")
    } else if (length(var_grup) == 2) {
      ggplot(df, aes_string(x = var_grup[1], y = var_dep, fill = var_grup[2])) +
        geom_boxplot() +
        theme_minimal() +
        labs(title = paste("Boxplot de", var_dep, "por", paste(var_grup, collapse=" y ")))
    } else {
      # Para más de dos variables, usamos facetas
      ggplot(df, aes_string(x = var_grup[1], y = var_dep, fill = var_grup[2])) +
        geom_boxplot() +
        facet_wrap(as.formula(paste("~", paste(var_grup[-(1:2)], collapse = "+")))) +
        theme_minimal() +
        labs(title = paste("Boxplot con facetas por", paste(var_grup[-(1:2)], collapse=", ")))
    }
  }
  
  crear_densidad <- function(df, var_dep, var_grup) {
    if (length(var_grup) == 1) {
      ggplot(df, aes_string(x = var_dep, fill = var_grup[1])) +
        geom_density(alpha = 0.5) +
        theme_minimal() +
        labs(title = paste("Densidad de", var_dep, "por", var_grup[1]))
    } else {
      # Para múltiples variables, combinamos las primeras dos y facetamos por el resto
      df$grupo_combinado <- interaction(df[var_grup[1:min(2, length(var_grup))]])
      
      grafico <- ggplot(df, aes_string(x = var_dep, fill = "grupo_combinado")) +
        geom_density(alpha = 0.5) +
        theme_minimal() +
        labs(title = paste("Densidad de", var_dep))
      
      if (length(var_grup) > 2) {
        grafico <- grafico + 
          facet_wrap(as.formula(paste("~", paste(var_grup[-(1:2)], collapse = "+"))))
      }
      return(grafico)
    }
  }
  
  # Nueva función para crear la campana de Gauss con regiones de aceptación/rechazo
  crear_campana_gauss <- function(res) {
    if (res$tipo == "t-test") {
      # Distribución t de Student
      df <- res$grados_libertad
      t_stat <- res$estadistico
      alpha <- res$alpha
      
      # Valores críticos
      t_critical <- qt(1 - alpha/2, df)
      
      # Crear datos para la curva
      x <- seq(-4, 4, length.out = 1000)
      y <- dt(x, df)
      
      # Datos para las regiones sombreadas
      reject_region1 <- data.frame(
        x = seq(-4, -t_critical, length.out = 100),
        y = dt(seq(-4, -t_critical, length.out = 100), df)
      )
      
      reject_region2 <- data.frame(
        x = seq(t_critical, 4, length.out = 100),
        y = dt(seq(t_critical, 4, length.out = 100), df)
      )
      
      p <- ggplot() +
        # Añadir la curva t
        geom_line(data = data.frame(x = x, y = y), aes(x = x, y = y), linewidth = 1) +
        
        # Añadir regiones de rechazo
        geom_area(data = reject_region1, aes(x = x, y = y), fill = "red", alpha = 0.5) +
        geom_area(data = reject_region2, aes(x = x, y = y), fill = "red", alpha = 0.5) +
        
        # Añadir líneas verticales para los valores críticos y t calculado
        geom_vline(xintercept = -t_critical, linetype = "dashed", color = "darkred") +
        geom_vline(xintercept = t_critical, linetype = "dashed", color = "darkred") +
        geom_vline(xintercept = as.numeric(t_stat), linetype = "solid", color = "blue", linewidth = 1) +
        
        # Añadir etiquetas
        annotate("text", x = -t_critical - 0.5, y = max(y) * 0.8, 
                 label = paste("t crítico =", round(-t_critical, 3)), color = "darkred") +
        annotate("text", x = t_critical + 0.5, y = max(y) * 0.8, 
                 label = paste("t crítico =", round(t_critical, 3)), color = "darkred") +
        annotate("text", x = as.numeric(t_stat), y = max(y) * 0.5, 
                 label = paste("t calculado =", round(as.numeric(t_stat), 3)), color = "blue") +
        
        # Añadir leyenda para las regiones
        annotate("text", x = 0, y = max(y) * 0.9, 
                 label = paste("Región de aceptación\n(1-α) =", 1-alpha), color = "darkgreen") +
        annotate("text", x = -3, y = max(y) * 0.3, 
                 label = paste("Región de rechazo\nα/2 =", alpha/2), color = "darkred") +
        annotate("text", x = 3, y = max(y) * 0.3, 
                 label = paste("Región de rechazo\nα/2 =", alpha/2), color = "darkred") +
        
        # Añadir título y etiquetas
        labs(title = paste("Distribución t de Student (gl =", round(df, 2), ")"),
             subtitle = paste("Prueba de hipótesis con α =", alpha, 
                              "\nValor p =", round(res$p_valor, 4)),
             x = "Valor t", y = "Densidad") +
        
        # Mejorar tema
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"),
              plot.subtitle = element_text(hjust = 0.5))
      
      # Añadir conclusión
      if (abs(as.numeric(t_stat)) > t_critical) {
        p <- p + annotate("text", x = 0, y = max(y) * 0.1, 
                          label = "❌ Se rechaza la hipótesis nula", 
                          color = "red", size = 5, fontface = "bold")
      } else {
        p <- p + annotate("text", x = 0, y = max(y) * 0.1, 
                          label = "✅ No se rechaza la hipótesis nula", 
                          color = "darkgreen", size = 5, fontface = "bold")
      }
      
      return(p)
      
    } else if (res$tipo == "anova") {
      # Distribución F
      df1 <- res$grado_libertad_1
      df2 <- res$grado_libertad_2
      f_stat <- res$estadistico
      alpha <- res$alpha
      
      # Valor crítico F
      f_critical <- qf(1 - alpha, df1, df2)
      
      # Crear datos para la curva F
      x <- seq(0, max(f_critical * 2, f_stat * 1.5), length.out = 1000)
      y <- df(x, df1, df2)
      
      # Datos para la región sombreada (rechazo)
      reject_region <- data.frame(
        x = seq(f_critical, max(x), length.out = 100),
        y = df(seq(f_critical, max(x), length.out = 100), df1, df2)
      )
      
      p <- ggplot() +
        # Añadir la curva F
        geom_line(data = data.frame(x = x, y = y), aes(x = x, y = y), linewidth = 1) +
        
        # Añadir región de rechazo
        geom_area(data = reject_region, aes(x = x, y = y), fill = "red", alpha = 0.5) +
        
        # Añadir líneas verticales para el valor crítico y F calculado
        geom_vline(xintercept = f_critical, linetype = "dashed", color = "darkred") +
        geom_vline(xintercept = f_stat, linetype = "solid", color = "blue", linewidth = 1) +
        
        # Añadir etiquetas
        annotate("text", x = f_critical + (max(x) - f_critical) * 0.3, y = max(y) * 0.8, 
                 label = paste("F crítico =", round(f_critical, 3)), color = "darkred") +
        annotate("text", x = f_stat, y = max(y) * 0.5, 
                 label = paste("F calculado =", round(f_stat, 3)), color = "blue") +
        
        # Añadir leyenda para las regiones
        annotate("text", x = f_critical * 0.5, y = max(y) * 0.8, 
                 label = paste("Región de aceptación\n(1-α) =", 1-alpha), color = "darkgreen") +
        annotate("text", x = f_critical + (max(x) - f_critical) * 0.7, y = max(y) * 0.3, 
                 label = paste("Región de rechazo\nα =", alpha), color = "darkred") +
        
        # Añadir título y etiquetas
        labs(title = paste("Distribución F (df1 =", df1, ", df2 =", df2, ")"),
             subtitle = paste("Prueba de hipótesis con α =", alpha, 
                              "\nValor p =", round(res$p_valor, 4)),
             x = "Valor F", y = "Densidad") +
        
        # Mejorar tema
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"),
              plot.subtitle = element_text(hjust = 0.5))
      
      # Añadir conclusión
      if (f_stat > f_critical) {
        p <- p + annotate("text", x = f_critical * 0.5, y = max(y) * 0.1, 
                          label = "❌ Se rechaza la hipótesis nula", 
                          color = "red", size = 5, fontface = "bold")
      } else {
        p <- p + annotate("text", x = f_critical * 0.5, y = max(y) * 0.1, 
                          label = "✅ No se rechaza la hipótesis nula", 
                          color = "darkgreen", size = 5, fontface = "bold")
      }
      
      return(p)
    }
  }
  
  crear_violin <- function(df, var_dep, var_grup) {
    if (length(var_grup) == 1) {
      ggplot(df, aes_string(x = var_grup[1], y = var_dep, fill = var_grup[1])) +
        geom_violin(trim = FALSE) +
        geom_boxplot(width = 0.1, fill = "white", alpha = 0.5) +
        theme_minimal() +
        labs(title = paste("Gráfico de violín de", var_dep, "por", var_grup[1])) +
        theme(legend.position = "none")
    } else if (length(var_grup) == 2) {
      ggplot(df, aes_string(x = var_grup[1], y = var_dep, fill = var_grup[2])) +
        geom_violin(position = position_dodge(0.8), trim = FALSE) +
        geom_boxplot(position = position_dodge(0.8), width = 0.1, fill = "white", alpha = 0.5) +
        theme_minimal() +
        labs(title = paste("Gráfico de violín de", var_dep))
    } else {
      # Para más variables, usamos facetas
      ggplot(df, aes_string(x = var_grup[1], y = var_dep, fill = var_grup[2])) +
        geom_violin(trim = FALSE) +
        geom_boxplot(width = 0.1, fill = "white", alpha = 0.5) +
        facet_wrap(as.formula(paste("~", paste(var_grup[-(1:2)], collapse = "+")))) +
        theme_minimal() +
        labs(title = paste("Gráfico de violín con facetas"))
    }
  }
  
  crear_barras <- function(df, var_dep, var_grup) {
    # Crear un dataframe de resumen
    resumen <- df %>%
      group_by(across(all_of(var_grup))) %>%
      summarise(
        Media = mean(.data[[var_dep]], na.rm = TRUE),
        EE = sd(.data[[var_dep]], na.rm = TRUE) / sqrt(n()),
        .groups = "drop"
      )
    
    if (length(var_grup) == 1) {
      ggplot(resumen, aes_string(x = var_grup[1], y = "Media", fill = var_grup[1])) +
        geom_bar(stat = "identity", position = position_dodge()) +
        geom_errorbar(aes(ymin = Media - EE, ymax = Media + EE), 
                      width = 0.2, position = position_dodge(0.9)) +
        theme_minimal() +
        labs(title = paste("Media de", var_dep, "por", var_grup[1]), 
             y = paste("Media de", var_dep)) +
        theme(legend.position = "none")
    } else if (length(var_grup) == 2) {
      ggplot(resumen, aes_string(x = var_grup[1], y = "Media", fill = var_grup[2])) +
        geom_bar(stat = "identity", position = position_dodge()) +
        geom_errorbar(aes(ymin = Media - EE, ymax = Media + EE), 
                      width = 0.2, position = position_dodge(0.9)) +
        theme_minimal() +
        labs(title = paste("Media de", var_dep), y = paste("Media de", var_dep))
    } else {
      # Para más variables, usamos facetas
      ggplot(resumen, aes_string(x = var_grup[1], y = "Media", fill = var_grup[2])) +
        geom_bar(stat = "identity", position = position_dodge()) +
        geom_errorbar(aes(ymin = Media - EE, ymax = Media + EE), 
                      width = 0.2, position = position_dodge(0.9)) +
        facet_wrap(as.formula(paste("~", paste(var_grup[-(1:2)], collapse = "+")))) +
        theme_minimal() +
        labs(title = "Medias con facetas", y = paste("Media de", var_dep))
    }
  }
  
  crear_scatter <- function(df, var_dep, var_grup) {
    if (length(var_grup) == 1) {
      # Crear una variable numérica para el eje X basada en el índice
      df$index <- 1:nrow(df)
      
      ggplot(df, aes_string(x = "index", y = var_dep, color = var_grup[1])) +
        geom_point(alpha = 0.6) +
        theme_minimal() +
        labs(title = paste("Dispersión de", var_dep, "por", var_grup[1]), 
             x = "Índice", y = var_dep)
    } else {
      # Para múltiples variables, usamos colores y formas
      df$index <- 1:nrow(df)
      ggplot(df, aes_string(x = "index", y = var_dep, color = var_grup[1], shape = var_grup[2])) +
        geom_point(alpha = 0.6) +
        theme_minimal() +
        labs(title = paste("Dispersión de", var_dep), x = "Índice", y = var_dep)
    }
  }
  
  output$grafico <- renderPlot({
    req(resultado_analisis(), input$tipo_grafico)
    
    res <- resultado_analisis()
    df <- res$datos
    var_dep <- res$var_dep
    var_grup <- res$var_grup
    
    if (input$tipo_grafico == "todos") {
      # Crear todos los gráficos y combinarlos
      g1 <- crear_boxplot(df, var_dep, var_grup)
      g2 <- crear_densidad(df, var_dep, var_grup)
      
      # Verificar casos específicos
      if (res$tipo == "t-test") {
        # Para t-test
        g3 <- crear_campana_gauss(res)
        g4 <- crear_barras(df, var_dep, var_grup)
      } else if (length(var_grup) >= 2) {
        # Para ANOVA con múltiples factores
        g3 <- crear_interaccion(df, var_dep, var_grup)
        g4 <- crear_barras(df, var_dep, var_grup)
      } else {
        # Para ANOVA de un factor
        g3 <- crear_barras(df, var_dep, var_grup)
        g4 <- crear_violin(df, var_dep, var_grup)
      }
      
      grid.arrange(g1, g2, g3, g4, ncol = 2, 
                   top = "Múltiples visualizaciones")
    } else if (input$tipo_grafico == "boxplot") {
      crear_boxplot(df, var_dep, var_grup)
    } else if (input$tipo_grafico == "densidad") {
      crear_densidad(df, var_dep, var_grup)
    } else if (input$tipo_grafico == "gauss") {
      if (res$tipo == "t-test") {
        crear_campana_gauss(res)
      } else {
        # Si no es t-test y se pide gauss, mostrar un mensaje
        plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "", axes = FALSE)
        text(0.5, 0.5, "La visualización de campana de Gauss\nsolo está disponible para pruebas t-test", cex = 1.5)
      }
    } else if (input$tipo_grafico == "interaccion") {
      crear_interaccion(df, var_dep, var_grup)
    } else if (input$tipo_grafico == "violin") {
      crear_violin(df, var_dep, var_grup)
    } else if (input$tipo_grafico == "barras") {
      crear_barras(df, var_dep, var_grup)
    } else if (input$tipo_grafico == "scatter") {
      crear_scatter(df, var_dep, var_grup)
    }
  })
  
  # Tablas de resumen
  output$tabla_resumen <- renderTable({
    req("resumen" %in% input$extras, resultado_analisis())
    
    res <- resultado_analisis()
    df <- res$datos
    var_dep <- res$var_dep
    
    # Crear resumen general
    summary_df <- data.frame(
      Estadístico = c("Media", "Mediana", "Desv. Est.", "Mín", "Máx", "N"),
      Valor = c(
        mean(df[[var_dep]], na.rm = TRUE),
        median(df[[var_dep]], na.rm = TRUE),
        sd(df[[var_dep]], na.rm = TRUE),
        min(df[[var_dep]], na.rm = TRUE),
        max(df[[var_dep]], na.rm = TRUE),
        sum(!is.na(df[[var_dep]]))
      )
    )
    summary_df$Valor <- round(summary_df$Valor, 4)
    return(summary_df)
  })
  
  output$tabla_medias <- renderTable({
    req("medias" %in% input$extras, resultado_analisis())
    
    res <- resultado_analisis()
    df <- res$datos
    var_dep <- res$var_dep
    var_grup <- res$var_grup
    
    df %>%
      group_by(across(all_of(var_grup))) %>%
      summarise(
        Media = round(mean(.data[[var_dep]], na.rm = TRUE), 4),
        Mediana = round(median(.data[[var_dep]], na.rm = TRUE), 4),
        `Desv. Est.` = round(sd(.data[[var_dep]], na.rm = TRUE), 4),
        `Error Est.` = round(sd(.data[[var_dep]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[var_dep]]))), 4),
        N = sum(!is.na(.data[[var_dep]])),
        .groups = "drop"
      )
  })
  
  output$datos_crudos <- renderDataTable({
    req(datos())
    datos()
  })
  
  # Generación de PDF
  
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)