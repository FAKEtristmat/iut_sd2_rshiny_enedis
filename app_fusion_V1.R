# Charger les bibliothèques nécessaires
library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)
library(shinydashboard)
library(ggpubr)
library(DT)
# Charger les données depuis les fichiers CSV
df_existants <- read.csv("existants_41.csv", header=TRUE, sep=",", dec=".")
df_neufs <- read.csv("neufs_41.csv", header=TRUE, sep=",", dec=".")

# Ajouter une colonne 'Logement' pour distinguer les types
df_existants$Logement <- "Ancien"
df_neufs$Logement <- "Neuf"

# Ajouter une colonne Année_construction pour les logements neufs avec l'année actuelle
df_neufs$Année_construction <- as.numeric(format(Sys.Date(), "%Y"))

# Supprimer les espaces autour des identifiants pour éviter les problèmes de jointure
df_existants$Identifiant__BAN <- trimws(df_existants$Identifiant__BAN)
df_neufs$Identifiant__BAN <- trimws(df_neufs$Identifiant__BAN)

# Fusionner les deux datasets avec uniquement les colonnes communes
colonnes_communes <- intersect(names(df_neufs), names(df_existants))
df <- rbind(df_neufs[, colonnes_communes], df_existants[, colonnes_communes])

# Supprimer les lignes où Année_construction est NA si nécessaire
df <- df[!is.na(df$Année_construction), ]

# Convertir Date_réception_DPE en date et extraire l'année
df$Date_réception_DPE <- as.Date(df$Date_réception_DPE)
df$Date_réception_DPE_YYYY <- as.numeric(format(df$Date_réception_DPE, "%Y"))

# Vérification des coûts
df$Somme_Coût_total_5_usages <- df$Coût_chauffage + df$Coût_éclairage + df$Coût_ECS + 
  df$Coût_refroidissement + df$Coût_auxiliaires
df$Ecart_Coût_total_5_usages <- df$Coût_total_5_usages - df$Somme_Coût_total_5_usages
df$Verif_Coût_total_5_usages <- ifelse(df$Ecart_Coût_total_5_usages > 0, "ERREUR", "OK")

# Calcul de la part du coût du chauffage
df$Part_Coût_chauffage <- round(df$Coût_chauffage / df$Coût_total_5_usages, 2)

# Créer une colonne Période_construction avec des classes
df$Periode_construction <- cut(df$Année_construction,
                               breaks = c(0, 1960, 1970, 1980, 1990, 2000, 2010, 2050),
                               labels = c("Avant 1960", "1961 - 1970", "1971 - 1980",
                                          "1981 - 1990", "1991 - 2000", "2001 - 2010", "Après 2010"))

# Interface utilisateur (UI)
ui <- dashboardPage(
  dashboardHeader(
    title = tags$div(
      tags$img(src = "Logo_enedis_header.png", height = "35px", style = "vertical-align: middle;")
    ),
    titleWidth = 250
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Contexte", tabName = "contexte", icon = icon("info")),
      menuItem("Visualisation", tabName = "visualisation", icon = icon("chart-bar")),
      menuItem("Cartographie", tabName = "cartographie", icon = icon("map"))
    ),
    selectInput("logementType", "Type de logement", choices = c("Tous", "Ancien", "Neuf")),
    sliderInput("yearRange", "Année de construction", min = 1950, max = 2025, value = c(1950, 2025)),
    selectInput("theme", "Choisir un thème", choices = c("Thème Bleu" = "blue_theme", "Thème Rouge" = "red_theme")),
    # Ajouter les sélecteurs de variables X et Y
    selectInput("variableX", "Variable X", choices = NULL),
    selectInput("variableY", "Variable Y", choices = NULL)
  ),
  dashboardBody(
    # Inclure le fichier CSS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    tabItems(
      tabItem(tabName = "contexte",
              fluidRow(
                box(
                  title = "Contexte", width = 12, status = "primary", solidHeader = TRUE,
                  div(
                    img(src = "enedis.png", height = "100px", style = "float: right; margin-left: 20px;"),
                    "Ce tableau de bord permet de visualiser et d'analyser les données des logements existants et neufs 
                    du département du Loir-et-Cher. L'utilisateur peut explorer les caractéristiques des logements, 
                    les coûts énergétiques, et bien plus encore."
                  )
                )
              ),
      fluidRow(
        box(
          title = "Tableau des données", width = 12, status = "primary", solidHeader = TRUE,
          dataTableOutput("dataTable")
        )
      )
    ),
      tabItem(tabName = "visualisation",
              fluidRow(
                valueBoxOutput("avgSurfaceBox"),
                valueBoxOutput("avgCostBox"),
                valueBoxOutput("proportionAncienBox")
              ),
              fluidRow(
                box(title = "Coût de chauffage par type de logement", status = "primary", solidHeader = TRUE, width = 6,
                    plotOutput("costPlot"),
                    downloadButton("downloadCostPlot","Télécharger le graphique en PNG")),
                box(title = "Répartition des étiquettes DPE", status = "primary", solidHeader = TRUE, width = 6,
                    plotOutput("dpePlot"),
                    downloadButton("downloadDPEPlot","Télécharger le graphique en PNG"))
              ),
              fluidRow(
                box(title = "Coût de chauffage vs Surface", status = "primary", solidHeader = TRUE, width = 6,
                    plotOutput("scatterPlot"),
                    downloadButton("downloadScatterPlot","Télécharger le graphique en PNG")),
                box(title = "Surface habitable par type de logement", status = "primary", solidHeader = TRUE, width = 6,
                    plotOutput("surfacePlot"),
                    downloadButton("downloadSurfacePlot","Télécharger le graphique en PNG"))
              ),
              fluidRow(
                valueBoxOutput("corrsurfacecout", width = 6)
              )
      ),
      tabItem(tabName = "cartographie",
              fluidRow(
                box(title = "Carte interactive", status = "primary", solidHeader = TRUE, width = 12,
                    leafletOutput("map"))
              )
      )
    )
  ))


# Serveur
server <- function(input, output, session) {
  
  
  # Liste des variables numériques disponibles
  numeric_vars <- names(df)[sapply(df, is.numeric)]
  
  # Mettre à jour les choix des selectInput pour les variables X et Y
  observe({
    updateSelectInput(session, "variableX", choices = numeric_vars, selected = "Surface_habitable_logement")
    updateSelectInput(session, "variableY", choices = numeric_vars, selected = "Coût_chauffage")
  })
  
  # Données pour les variables sélectionnées
  selectedData <- reactive({
    data <- filteredData()
    x_var <- input$variableX
    y_var <- input$variableY
    
    data <- data[, c(x_var, y_var, "Logement")]
    data <- na.omit(data)
    data
  })
  
  
  # Fonction pour définir les couleurs selon le thème
  theme_colors <- reactive({
    if (input$theme == "blue_theme") {
      return(list(ancien = "blue", neuf = "lightgreen"))
    } else {
      return(list(ancien = "red", neuf = "pink"))
    }
  })
  
  # Filtrer les données en fonction des entrées de l'utilisateur
  filteredData <- reactive({
    data <- df
    if (input$logementType != "Tous") {
      data <- data[data$Logement == input$logementType, ]
    }
    data <- data[data$Année_construction >= input$yearRange[1] & data$Année_construction <= input$yearRange[2], ]
    
    # Supprimer les lignes avec des NA pour les variables utilisées dans les graphiques
    data <- data[!is.na(data$Surface_habitable_logement) & !is.na(data$Coût_chauffage), ]
    data
  })
  
  output$dataTable <- renderDataTable({
    datatable(filteredData(),options = list(scrollX = TRUE))
    })
  
  # Valeur moyenne de la surface habitable
  output$avgSurfaceBox <- renderValueBox({
    avg_surface <- mean(filteredData()$Surface_habitable_logement, na.rm = TRUE)
    valueBox(
      value = round(avg_surface, 2),  "Surface habitable moyenne (m²)", 
      icon = icon("home"),
      color = "blue"
    )
  })
  
  # Valeur moyenne du coût de chauffage
  output$avgCostBox <- renderValueBox({
    avg_cost <- mean(filteredData()$Coût_chauffage, na.rm = TRUE)
    valueBox(round(avg_cost, 2), "Coût moyen du chauffage (€)", icon = icon("fire"), color = "red")
  })
  
  # Proportion de logements anciens
  output$proportionAncienBox <- renderValueBox({
    proportion_ancien <- nrow(filteredData()[filteredData()$Logement == "Ancien", ]) / nrow(filteredData()) * 100
    valueBox(paste0(round(proportion_ancien, 1), "%"), "Proportion de logements anciens", icon = icon("building"), color = "orange")
  })
  
  # Coefficient de corrélation
  output$corrsurfacecout <- renderValueBox({
    correlation <- cor(filteredData()$Surface_habitable_logement, filteredData()$Coût_chauffage)
    valueBox(round(correlation, 4), "Coefficient de corrélation: surface et coût chauffage", color = "orange")
  })
  
  # Graphique du coût de chauffage
  output$costPlot <- renderPlot({
    ggplot(filteredData(), aes(x = Logement, y = Coût_chauffage, fill = Logement)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = "Coût de chauffage par type de logement", y = "Coût de chauffage (€)", x = "Type de logement") +
      scale_fill_manual(values = c("Ancien" = theme_colors()$ancien, "Neuf" = theme_colors()$neuf)) +
      coord_cartesian(ylim = c(0, 5000))
  })
  
  # Graphique de la répartition des étiquettes DPE
  output$dpePlot <- renderPlot({
    ggplot(filteredData(), aes(x = Etiquette_DPE, fill = Logement)) +
      geom_bar(position = "dodge") +
      theme_minimal() +
      labs(title = "Répartition des étiquettes DPE", y = "Nombre de logements", x = "Étiquette DPE") +
      scale_fill_manual(values = c("Ancien" = theme_colors()$ancien, "Neuf" = theme_colors()$neuf))
  })
  
  # Graphique de la surface habitable
  output$surfacePlot <- renderPlot({
    ggplot(filteredData(), aes(x = Surface_habitable_logement, fill = Logement)) +
      geom_histogram(binwidth = 10, alpha = 0.7, position = "identity") +
      theme_minimal() +
      labs(title = "Distribution de la surface habitable", x = "Surface habitable (m²)", y = "Nombre de logements") +
      scale_fill_manual(values = c("Ancien" = theme_colors()$ancien, "Neuf" = theme_colors()$neuf)) +
      coord_cartesian(xlim = c(0, 400))
  })
  
  output$scatterPlot <- renderPlot({
    data <- selectedData()
    ggplot(data, aes_string(x = input$variableX, y = input$variableY, color = "Logement")) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      stat_regline_equation(label.x = 0.1 * max(data[[input$variableX]], na.rm = TRUE), 
                            label.y = 0.9 * max(data[[input$variableY]], na.rm = TRUE)) +
      theme_minimal() +
      labs(title = paste("Relation entre", input$variableX, "et", input$variableY), 
           x = input$variableX, 
           y = input$variableY) +
      scale_color_manual(values = c("Ancien" = theme_colors()$ancien, "Neuf" = theme_colors()$neuf))
  })
  
  
  
  # Carte interactive
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(
        data = filteredData(), 
        lng = ~lon, lat = ~lat, 
        popup = ~paste(
          "Logement:", Logement, "<br>", 
          "Surface:", Surface_habitable_logement, "m²","<br>", 
          "Coût chauffage:", Coût_chauffage, "€","<br>",
          "Étiquette DPE:", Etiquette_DPE),
        clusterOptions = markerClusterOptions(),
        color = ~ifelse(Etiquette_DPE == "A", "darkgreen",
                        ifelse(Etiquette_DPE == "B", "green",
                               ifelse(Etiquette_DPE == "C", "lightgreen",
                                      ifelse(Etiquette_DPE == "D", "yellow",
                                             ifelse(Etiquette_DPE == "E", "orange",
                                                    ifelse(Etiquette_DPE == "F", "darkorange",
                                                           ifelse(Etiquette_DPE == "G", "red", "grey"))))))),
        radius = 10, 
        fillOpacity = 0.7,
        stroke = FALSE
      )
  })
  
  # Fonctions pour télécharger les graphiques
  output$downloadCostPlot <- downloadHandler(
    filename = function() { paste("cout_chauffage", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      g <- ggplot(filteredData(), aes(x = Logement, y = Coût_chauffage, fill = Logement)) +
        geom_boxplot() +
        theme_minimal() +
        labs(title = "Coût de chauffage par type de logement", y = "Coût de chauffage (€)", x = "Type de logement") +
        scale_fill_manual(values = c("Ancien" = "orange", "Neuf" = "blue")) +
        coord_cartesian(ylim = c(0, 5000))
      ggsave(file, plot = g, device = "png", width = 8, height = 6)
    }
  )
  
  output$downloadDPEPlot <- downloadHandler(
    filename = function() { paste("repartition_DPE", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      e <- ggplot(filteredData(), aes(x = Etiquette_DPE, fill = Logement)) +
        geom_bar(position = "dodge") +
        theme_minimal() +
        labs(title = "Répartition des étiquettes DPE", y = "Nombre de logements", x = "Étiquette DPE") +
        scale_fill_manual(values = c("Ancien" = "orange", "Neuf" = "blue"))
      ggsave(file, plot = e, device = "png", width = 8, height = 6)
    }
  ) 
  
  output$downloadSurfacePlot <- downloadHandler(
    filename = function() { paste("surface_habitable", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      f <- ggplot(filteredData(), aes(x = Surface_habitable_logement, fill = Logement)) +
        geom_histogram(binwidth = 10, alpha = 0.7, position = "identity") +
        theme_minimal() +
        labs(title = "Distribution de la surface habitable", x = "Surface habitable (m²)", y = "Nombre de logements") +
        scale_fill_manual(values = c("Ancien" = "orange", "Neuf" = "blue")) +
        coord_cartesian(xlim = c(0, 400))
      ggsave(file, plot = f, device = "png", width = 8, height = 6)
    }
  ) 
  # Coefficient de corrélation pour les variables sélectionnées
  output$corrsurfacecout <- renderValueBox({
    data <- selectedData()
    correlation <- cor(data[[input$variableX]], data[[input$variableY]])
    valueBox(round(correlation, 4), paste("Coefficient de corrélation :", input$variableX, "et", input$variableY), color = "orange")
  })
  
  box(title = "Nuage de points avec régression linéaire", status = "primary", solidHeader = TRUE, width = 6,
      plotOutput("scatterPlot"),
      downloadButton("downloadScatterPlot","Télécharger le graphique en PNG"))
  
  
  output$downloadScatterPlot <- downloadHandler(
    filename = function() { paste("scatterplot_", input$variableX, "_", input$variableY, "_", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      data <- selectedData()
      h <- ggplot(data, aes_string(x = input$variableX, y = input$variableY, color = "Logement")) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = FALSE, color = "black") +
        theme_minimal() +
        labs(title = paste("Relation entre", input$variableX, "et", input$variableY), 
             x = input$variableX, 
             y = input$variableY) +
        scale_color_manual(values = c("Ancien" = "orange", "Neuf" = "blue"))
      ggsave(file, plot = h, device = "png", width = 8, height = 6)
    }
  )
  
}

# Lancer l'application
shinyApp(ui, server)
