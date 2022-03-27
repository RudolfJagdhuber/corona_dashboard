library(shiny)
library(shinydashboard)
library(shinycssloaders) # Loading animation
library(data.table)
library(curl) # fread with https connection
library(ggplot2);theme_set(theme_bw())
library(sparkline) # Sparklines in Tables
library(DT) # Responsive Tables


# Define UI for application that draws a histogram
ui = dashboardPage(
  
  dashboardHeader(title = "Corona Übersicht"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Einführung", icon = icon("info-circle"), tabName = "AB"),
      menuItem("Deutschland (RKI Daten)", tabName = "german",
        icon = icon("chart-bar")),
      menuItem("Weltweit (WHO Daten)", tabName = "inter",
        icon = icon("chart-bar"))
    )
  ),
  
  dashboardBody(tabItems(
    
    tabItem(tabName = "AB", # About section
      h1("Corona Übersicht der 7-Tage-Inzidenz"),
      p(class = "lead", "Diese Seite wurde geschaffen, als die 7-Tage",
        "Inzidenz noch nicht weit verbreitet war und ich eine flexible",
        "Plattform suchte, auch aus den weltweiten Daten diese",
        "Information zu erhalten. Inzwischen hat sich viel getan, und",
        "so ziemlich jede anständige Seite beinhaltet nun auch die",
        "7-Tage Inzidenz. Ich werde diese Plattform trotzdem weiter ",
        "aktiv lassen, da sie von einigen genutzt wird, jedoch ist ",
        "zeitlich bedingt nicht mit größeren technischen Updates",
        "zu rechnen."),
      h2("Datengrundlage"),
      p(class = "lead", "Die deutschlandweite Übersicht basiert auf",
        "Daten des Robert-Koch-Instituts. Die weltweite Übersicht",
        "nutzt offizielle Daten der World-Health-Organization. Alle",
        "Datensätze werden täglich um 6 Uhr abgerufen und aufbereitet, sodass",
        "dargestellte Ergebnisse dem tagesaktuellen Stand entsprechen."),
      p(class = "lead", "Der aktuell verwendete Datenstand ist:"),
      h3(textOutput("reloadDate", container = strong)),
      # p(class = "lead", "Falls Sie die Daten jetzt manuell aktualisieren",
      #   "möchten klicken Sie bitte hier:"),
      # actionButton("dataUpdate", "Daten aktualisieren"),
      br(),
      p("RKI-Datenquelle:", br(),
        a(href = paste0("https://www.arcgis.com/sharing/rest/content/items/", 
          "66876b81065340a4a48710b062319336/data"),
          paste0("https://opendata.arcgis.com/datasets/",
            "dd4580c810204019a7b8eb3e0b329dd6_0.csv"))),
      br(),
      p("WHO-Datenquelle:", br(),
        a(href = "https://covid19.who.int/WHO-COVID-19-global-data.csv",
          "https://covid19.who.int/WHO-COVID-19-global-data.csv")),
      h2("Bedienung"),
      p(class = "lead", "Im Menü auf der linken Seite lassen sich eine",
        "deutschlandweite, sowie eine weltweite Übersicht auswählen."),
      p(class = "lead", "Die gezeigten Tabellen sind interaktiv und erlauben",
        "mit einem Klick eine Auswahl von Zeilen. Hiernach werden alle",
        "Grafiken und Daten auf die Auswahl angepasst. Spalten lassen sich",
        "durch Klick auf den Spaltennamen auf- oder absteigend sortieren.")
    ),
    
    tabItem(tabName = "german",
      h1("Corona Übersicht Deutschland"),
      p(strong(textOutput("reloadDateD"))),
      hr(),
      
      fluidRow(
        box(width = 8, align = "center", height = 650,
          title = "Grafik des zeitlichen Verlaufs",
          solidHeader = T, status = "primary",
          textOutput("G_header", container = h4),
          plotOutput("G_plot", height = 460) %>%
            withSpinner(color = "#3c8dbc"),
          radioButtons("G_plottype", inline = TRUE,
            label = "Was soll dargestellt werden?",
            choices = c("7 Tage Inzidenz", "Neuinfektionen",
              "Todesfälle", "Gesamtinfektionen",
              "Infektion und Tod"))),
        box(width = 4, align = "center", height = 650,
          title = "Tabelle des zeitliche Verlaufs",
          solidHeader = TRUE, status = "primary",
          textOutput("G_header2", container = h4),
          dataTableOutput("G_infobox") %>%
            withSpinner(color = "#3c8dbc"))
      ),
      
      fluidRow(
        box(width = 5, title = "Spitzenreiter Bundesländer",
          solidHeader = T, status = "primary",
          dataTableOutput("G_tabBL") %>%
            withSpinner(color = "#3c8dbc")),
        box(width = 7, title = "Spitzenreiter Landkreise",
          solidHeader = T, status = "primary",
          dataTableOutput("G_tabLK") %>%
            withSpinner(color = "#3c8dbc"))
      ),
    ),
    
    tabItem(tabName = "inter",
      
      h1("Corona Übersicht International"),
      p(strong(textOutput("reloadDateI"))),
      hr(),
      
      fluidRow(
        box(width = 8, align = "center", height = 650,
          title = "Grafik des zeitlichen Verlaufs",
          solidHeader = T, status = "primary",
          textOutput("I_header", container = h4),
          plotOutput("I_plot", height = 460) %>%
            withSpinner(color = "#3c8dbc"),
          radioButtons("I_plottype", inline = TRUE,
            label = "Was soll dargestellt werden?",
            choices = c("7 Tage Inzidenz", "Neuinfektionen",
              "Todesfälle", "Gesamtinfektionen",
              "Infektion und Tod"))),
        box(width = 4, align = "center", height = 650,
          title = "Tabelle des zeitliche Verlaufs",
          solidHeader = TRUE, status = "primary",
          textOutput("I_header2", container = h4),
          dataTableOutput("I_infobox") %>%
            withSpinner(color = "#3c8dbc"))
      ),
      
      fluidRow(
        box(width = 12, title = "Länderübersicht",
          solidHeader = T, status = "primary",
          dataTableOutput("I_tab") %>%
            withSpinner(color = "#3c8dbc"))
      )
    )
  ))
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  ###### DATA STATUS #########################################################
 
  # Read the file from the github repository, where a runner updates it daily 
  D = readRDS(url(paste0("https://github.com/RudolfJagdhuber/corona_dashboard/",
    "raw/master/www/data.rds"), "rb"))
  
  
  ###### SERVER SIDE FUNCTIONALITY ###########################################
  
  ## The current file date is displayed at multiple places
  
  output$reloadDate = renderText(paste(format(D$date_updated, "%d.%m.%Y"),
    D$time_updated))
  
  output$reloadDateD = renderText(paste0(
    "Daten des Robert-Koch-Instituts (Stand: ",
    format(D$date_updated, "%d.%m.%Y "), D$time_updated, ")"))
  
  output$reloadDateI = renderText(paste0(
    "Daten der World-Health-Organization (Stand: ",
    format(D$date_updated, "%d.%m.%Y "), D$time_updated, ")"))
  
  
  ## Germany Section
  
  # top Landkreis list depends on selected Bundesland
  topLKx = reactive({
    s = input$G_tabBL_rows_selected
    if (length(s)) {
      tmp = D$topLK[Bundesland == D$topBL$Bundesland[s]]
      setorder(tmp, -New100k)
    } else return(D$topLK)
  })
  
  # The data subsetted to all selections
  dataG = reactive({
    s_LK = input$G_tabLK_rows_selected
    s_BL = input$G_tabBL_rows_selected
    # if an LK is selected, that goes first
    if (length(s_LK)) {
      tmp = D$RKI[Landkreis == topLKx()$Landkreis[s_LK]]
    } else if (length(s_BL)) {
      tmp = D$RKI[Bundesland == D$topBL$Bundesland[s_BL]]
    } else tmp = D$RKI
    
    tmp = tmp[,.(New100k = round(1e5 * sum(New7) / sum(Population), 2),
      Neuinfektionen = sum(Neuinfektionen),
      Tote = sum(NeueTote),
      Gesamtinfektionen = sum(Gesamtinfektionen)), by = Datum]
    
    return(tmp)
  })
  
  # The table displaying the selected data next to the plot
  output$G_infobox = renderDataTable({
    tab = data.frame(dataG())
    rownames(tab) = format(tab$Datum, "%d.%m.%Y")
    today = rownames(tab)[1]
    
    datatable(tab[,-1], selection = 'none', options = list(pageLength = 12,
      autoWidth = FALSE, scrollX = TRUE, dom = "tp"),
      colnames = c(" ", "7 Tage Inzidenz", "Neue Fälle", "Neue Tote",
        "Fälle Gesamt")) %>%
      formatStyle(" ", target = "row",
        fontSize = styleEqual(today, "100%", "90%"),
        backgroundColor = styleEqual(today, "#ddddff", "white"),
        fontWeight = styleEqual(today, "bold", "normal")) %>%
      formatStyle(" ", fontWeight = "bold") %>%
      formatStyle("New100k", fontWeight = "bold",
        backgroundColor = styleInterval(c(35, 50, 100, 200, 500),
          c("#62de4a", "#fce034", "#f14a26", "#800e0e", "#810067",
            "#580a99")),
        color = styleInterval(100, c("#000000", "#ffffff"))) %>%
      formatCurrency("Gesamtinfektionen", currency = "", interval = 3,
        mark = ",", digits = 0) %>%
      formatCurrency("Neuinfektionen", currency = "+", interval = 3,
        mark = ",", digits = 0) %>%
      formatCurrency("Tote", currency = "+", interval = 3,
        mark = ",", digits = 0)
    
  })
  
  # Plot and Table header
  output$G_header = output$G_header2 = renderText({
    s_LK = input$G_tabLK_rows_selected
    s_BL = input$G_tabBL_rows_selected
    paste("Daten für", ifelse(length(s_LK) == 1, topLKx()$Landkreis[s_LK],
      ifelse(length(s_BL) == 1, D$topBL$Bundesland[s_BL],
        "ganz Deutschland")))
  })
  
  # The main plot of the selected data
  output$G_plot = renderPlot({
    
    pltdat = dataG()
    today = max(pltdat$Datum)
    brks = c(10 * round(seq(-max(pltdat$Tote), 0, length.out = 5), -1),
      round(seq(0, max(pltdat$Neuinfektionen), length.out = 5), -1)[-1])
    
    switch(input$G_plottype,
      "Neuinfektionen" = {
        ggplot(pltdat, aes(x = Datum, y = Neuinfektionen)) +
          geom_bar(stat = "identity", width = 1, fill = "#3c8dbc",
            alpha = 0.5) +
          scale_x_date(date_labels = "%d.%m",
            date_breaks = "20 day") +
          labs(y = "Anzahl an Neuinfektionen") +
          geom_smooth(method = "loess", formula = y ~ x, se = F,
            span = 0.125, color = "blue4", size = 1.25) +
          coord_cartesian(ylim = c(0, max(pltdat$Neuinfektionen))) +
          theme(axis.text.x = element_text(angle = 60, size = 10,
            hjust = 1, vjust = 1),
            axis.title.x = element_blank(),
            plot.title = element_blank(),
            axis.text.y = element_text(angle = 90, hjust = 0.5))
      },
      "Todesfälle" = {
        ggplot(pltdat, aes(x = Datum, y = Tote)) +
          geom_bar(stat = "identity", width = 1, fill = "lightcoral",
            alpha = 0.5) +
          scale_x_date(date_labels = "%d.%m",
            date_breaks = "20 day") +
          labs(y = "Anzahl neuer Todesfälle") +
          geom_smooth(method = "loess", formula = y ~ x, se = F,
            span = 0.125, color = "red3", size = 1.25) +
          coord_cartesian(ylim = c(0, max(pltdat$Tote))) +
          theme(axis.text.x = element_text(angle = 60, size = 10,
            hjust = 1, vjust = 1),
            axis.title.x = element_blank(),
            plot.title = element_blank(),
            axis.text.y = element_text(angle = 90, hjust = 0.5))
      },
      "7 Tage Inzidenz" = {
        dpl_G = data.frame(x = rep(rep(range(pltdat$Datum) + c(-20, 20),
          each = 2), 6), y = c(0, 35, 35, 0, 35, 50, 50, 35, 50, 100,
            100, 50, 100, 200, 200, 100, 200, 500, 500, 200, 500,
            10000, 10000, 500), Wert = rep(c("< 35", "35 - 50",
              "50 - 100", "100 - 200", "200 - 500", "> 500"),
              each = 4))
        
        ggplot(pltdat, aes(x = Datum, y = New100k)) +
          geom_polygon(data = dpl_G, aes(x = x, y = y, fill = Wert),
            alpha = 0.7) +
          geom_area(stat = "identity", fill = "#3c8dbc") +
          geom_line(col = "blue4", size = 1.25) +
          annotate("point", x = today, col = "blue4", size = 2,
            y = pltdat[Datum == today]$New100k) +
          scale_fill_manual(values = c( # unreasonable order...
            "#62de4a", # green
            "#580a99", # purple
            "#800e0e", # darkred
            "#810067", # magenta
            "#fce034", # yellow
            "#f14a26"  # lightred
          )) +
          scale_x_date(date_labels = "%d.%m",
            date_breaks = "20 day") +
          coord_cartesian(ylim = c(0, max(pltdat$New100k)),
            xlim = range(pltdat$Datum)) +
          labs(y = "7 Tage Inzidenz") +
          theme(axis.text.x = element_text(angle = 60, size = 10,
            hjust = 1, vjust = 1), legend.position = "none",
            axis.title.x = element_blank(),
            plot.title = element_blank(),
            axis.text.y = element_text(angle = 90, hjust = 0.5))
      },
      "Gesamtinfektionen" = {
        ggplot(pltdat, aes(x = Datum, y = Gesamtinfektionen)) +
          geom_area(stat = "identity", fill = "#3c8dbc", alpha = .5) +
          geom_line(col = "blue4", size = 1.25) +
          annotate("point", x = today, col = "blue4", size = 2,
            y = pltdat[Datum == today]$Gesamtinfektionen) +
          scale_x_date(date_labels = "%d.%m",
            date_breaks = "20 day") +
          labs(y = "Infektionen Gesamt") +
          theme(axis.text.x = element_text(angle = 60, size = 10,
            hjust = 1, vjust = 1),
            axis.title.x = element_blank(),
            plot.title = element_blank(),
            axis.text.y = element_text(angle = 90, hjust = 0.5))
      },
      "Infektion und Tod" = {
        ggplot(pltdat, aes(x = Datum, y = Neuinfektionen)) +
          geom_hline(yintercept = 0) +
          scale_x_date(date_labels = "%d.%m",
            date_breaks = "20 day") +
          geom_bar(stat = "identity", width = 1, fill = "#3c8dbc",
            alpha = 0.7) +
          geom_bar(aes(y = -10 * Tote), stat = "identity", width = 1,
            fill = "lightcoral", alpha = 0.7) +
          geom_smooth(method = "loess", formula = y ~ x, se = F,
            span = 0.125, color = "blue4", size = 1.25) +
          geom_smooth(aes(y = -10 * Tote), method = "loess",
            formula = y ~ x, se = F, span = 0.125, color = "red3",
            size = 1.25) +
          annotate("text", x = min(pltdat$Datum), y = 0,
            label = "Infektionen", size = 6, hjust = -0.2,
            col = "blue4", angle = 90) +
          annotate("text", x = min(pltdat$Datum), y = 0,
            label = "Tote", size = 6, hjust = 1.35,
            col = "red3", angle = 90) +
          scale_y_continuous(breaks = brks ,
            labels = brks/c(rep(-10, 4), rep(1, 5))) +
          theme(axis.text.x = element_text(angle = 60, size = 10,
            hjust = 1, vjust = 1),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_blank())
      }
    )
  })
  
  # The table of ranking by Bundesland
  output$G_tabBL = renderDataTable({
    datatable(D$topBL, escape = F, selection = 'single',
      options = list(pageLength = 16, dom = "t", fontSize = "80%",
        autoWidth = FALSE, scrollX = TRUE,
        order = list(list(3, 'desc')), fnDrawCallback =
          htmlwidgets::JS("function(){HTMLWidgets.staticRender();}")),
      colnames = c("Bundesland", "", "7 Tage Inzidenz",
        "Neue Fälle", "Gesamt")) %>%
      formatStyle("New100k", fontWeight = "bold",
        backgroundColor = styleInterval(c(35, 50, 100, 200, 500),
          c("#62de4a", "#fce034", "#f14a26", "#800e0e", "#810067",
            "#580a99")),
        color = styleInterval(100, c("#000000", "#ffffff"))) %>%
      formatStyle("Bundesland", fontWeight = "bold") %>%
      formatCurrency("Gesamt", currency = "", interval = 3,
        mark = ",", digits = 0) %>%
      formatCurrency("Neuinfektionen", currency = "+", interval = 3,
        mark = ",", digits = 0) %>%
      spk_add_deps()
  })
  
  # The table of ranking by Landkreis
  output$G_tabLK = renderDataTable({
    datatable(topLKx(), escape = F, selection = 'single',
      options = list(pageLength = 50, dom = "ltip",
        autoWidth = FALSE, scrollX = TRUE,
        order = list(list(3, 'desc')), fnDrawCallback =
          htmlwidgets::JS("function(){HTMLWidgets.staticRender();}")),
      colnames = c("Landkreis", "", "7 Tage Inzidenz",
        "Neue Fälle", "Gesamt", "Bundesland")) %>%
      formatStyle("New100k", fontWeight = "bold",
        backgroundColor = styleInterval(c(35, 50, 100, 200, 500),
          c("#62de4a", "#fce034", "#f14a26", "#800e0e", "#810067",
            "#580a99")),
        color = styleInterval(100, c("#000000", "#ffffff"))) %>%
      formatStyle("Landkreis", fontWeight = "bold") %>%
      formatCurrency("Gesamt", currency = "", interval = 3,
        mark = ",", digits = 0) %>%
      formatCurrency("Neuinfektionen", currency = "+", interval = 3,
        mark = ",", digits = 0) %>%
      spk_add_deps()
  })
  
  
  ## International Section
  
  # The data subsetted to the selected country
  dataI = reactive({
    
    s_C = input$I_tab_rows_selected
    if (length(s_C)) {
      tmp = D$WHO[Country == D$topI$Country[s_C]]
    } else tmp = D$WHO
    
    # Compute / Subset statistics of interest
    tmp = tmp[,.(New100k = round(1e5 * sum(New7) / sum(Population), 2),
      Neuinfektionen = sum(New_cases),
      Tote = sum(New_deaths),
      Gesamtinfektionen = sum(Gesamtinfektionen)), by = Datum]
    setorder(tmp, -Datum)
    
    return(tmp)
  })
  
  # The table displaying the selected data next to the plot
  output$I_infobox = renderDataTable({
    tab = data.frame(dataI())
    rownames(tab) = format(tab$Datum, "%d.%m.%Y")
    today = rownames(tab)[1]
    
    datatable(tab[,-1], selection = 'none', options = list(pageLength = 12,
      autoWidth = FALSE, scrollX = TRUE, dom = "tp"),
      colnames = c(" ", "7 Tage Inzidenz", "Neue Fälle", "Neue Tote",
        "Fälle Gesamt")) %>%
      formatStyle(" ", target = "row",
        fontSize = styleEqual(today, "100%", "90%"),
        backgroundColor = styleEqual(today, "#ddddff", "white"),
        fontWeight = styleEqual(today, "bold", "normal")) %>%
      formatStyle(" ", fontWeight = "bold") %>%
      formatStyle("New100k", fontWeight = "bold",
        backgroundColor = styleInterval(c(35, 50, 100, 200, 500),
          c("#62de4a", "#fce034", "#f14a26", "#800e0e", "#810067",
            "#580a99")),
        color = styleInterval(100, c("#000000", "#ffffff"))) %>%
      formatCurrency("Gesamtinfektionen", currency = "", interval = 3,
        mark = ",", digits = 0) %>%
      formatCurrency("Neuinfektionen", currency = "+", interval = 3,
        mark = ",", digits = 0) %>%
      formatCurrency("Tote", currency = "+", interval = 3,
        mark = ",", digits = 0)
  })
  
  # Plot and Table header
  output$I_header = output$I_header2 = renderText({
    s_C = input$I_tab_rows_selected
    paste("Daten für", ifelse(length(s_C) == 1, D$topI$Country[s_C],
      "alle Länder der Welt"))
  })
  
  # The main plot of the selected data
  output$I_plot = renderPlot({
    pltdatI = dataI()
    today = max(pltdatI$Datum)
    brks = c(10 * round(seq(-max(pltdatI$Tote), 0, length.out = 5), -1),
      round(seq(0, max(pltdatI$Neuinfektionen), length.out = 5), -1)[-1])
    
    switch(input$I_plottype,
      "Neuinfektionen" = {
        pltI = ggplot(pltdatI, aes(x = Datum, y = Neuinfektionen)) +
          geom_bar(stat = "identity", width = 1, fill = "#3c8dbc",
            alpha = 0.5) +
          scale_x_date(date_labels = "%d.%m",
            date_breaks = "20 day") +
          labs(y = "Anzahl an Neuinfektionen") +
          geom_smooth(method = "loess", formula = y ~ x, se = F,
            span = 0.125, color = "blue4", size = 1.25) +
          coord_cartesian(ylim = c(0, max(pltdatI$Neuinfektionen))) +
          theme(axis.text.x = element_text(angle = 60, size = 10,
            hjust = 1, vjust = 1),
            axis.title.x = element_blank(),
            plot.title = element_blank(),
            axis.text.y = element_text(angle = 90, hjust = 0.5))
      },
      "Todesfälle" = {
        pltI = ggplot(pltdatI, aes(x = Datum, y = Tote)) +
          geom_bar(stat = "identity", width = 1, fill = "lightcoral",
            alpha = 0.5) +
          scale_x_date(date_labels = "%d.%m",
            date_breaks = "20 day") +
          labs(y = "Anzahl neuer Todesfälle") +
          geom_smooth(method = "loess", formula = y ~ x, se = F,
            span = 0.125, color = "red3", size = 1.25) +
          coord_cartesian(ylim = c(0, max(pltdatI$Tote))) +
          theme(axis.text.x = element_text(angle = 60, size = 10,
            hjust = 1, vjust = 1),
            axis.title.x = element_blank(),
            plot.title = element_blank(),
            axis.text.y = element_text(angle = 90, hjust = 0.5))
      },
      "7 Tage Inzidenz" = {
        # Make polygon for background color
        dpl = data.frame(x = rep(rep(range(pltdatI$Datum) + c(-20, 20),
          each = 2), 6), y = c(0, 35, 35, 0, 35, 50, 50, 35, 50, 100,
            100, 50, 100, 200, 200, 100, 200, 500, 500, 200, 500,
            10000, 10000, 500), Wert = rep(c("< 35", "35 - 50",
              "50 - 100", "100 - 200", "200 - 500", "> 500"),
              each = 4))
        
        pltI = ggplot(pltdatI, aes(x = Datum, y = New100k)) +
          geom_polygon(data = dpl, aes(x = x, y = y, fill = Wert),
            alpha = 0.7) +
          geom_area(stat = "identity", fill = "#3c8dbc") +
          geom_line(col = "blue4", size = 1.25) +
          annotate("point", x = today, col = "blue4", size = 2,
            y = pltdatI[Datum == today]$New100k) +
          scale_fill_manual(values = c( # unreasonable order...
            "#62de4a", # green
            "#580a99", # purple
            "#800e0e", # darkred
            "#810067", # magenta
            "#fce034", # yellow
            "#f14a26"  # lightred
          )) +
          coord_cartesian(ylim = c(0, max(pltdatI$New100k)),
            xlim = range(pltdatI$Datum)) +
          scale_x_date(date_labels = "%d.%m",
            date_breaks = "20 day") +
          labs(y = "7 Tage Inzidenz") +
          theme(axis.text.x = element_text(angle = 60, size = 10,
            hjust = 1, vjust = 1), legend.position = "none",
            axis.title.x = element_blank(),
            plot.title = element_blank(),
            axis.text.y = element_text(angle = 90, hjust = 0.5))
      },
      "Gesamtinfektionen" = {
        pltI = ggplot(pltdatI, aes(x = Datum, y = Gesamtinfektionen)) +
          geom_area(stat = "identity", fill = "#3c8dbc", alpha = .5) +
          geom_line(col = "blue4", size = 1.25) +
          annotate("point", x = today, col = "blue4", size = 2,
            y = pltdatI[Datum == today]$Gesamtinfektionen) +
          scale_x_date(date_labels = "%d.%m",
            date_breaks = "20 day") +
          labs(y = "Infektionen Gesamt") +
          theme(axis.text.x = element_text(angle = 60, size = 10,
            hjust = 1, vjust = 1),
            axis.title.x = element_blank(),
            plot.title = element_blank(),
            axis.text.y = element_text(angle = 90, hjust = 0.5))
      },
      "Infektion und Tod" = {
        pltI = ggplot(pltdatI, aes(x = Datum, y = Neuinfektionen)) +
          geom_hline(yintercept = 0) +
          scale_x_date(date_labels = "%d.%m",
            date_breaks = "20 day") +
          geom_bar(stat = "identity", width = 1, fill = "#3c8dbc",
            alpha = 0.7) +
          geom_bar(aes(y = -10 * Tote), stat = "identity", width = 1,
            fill = "lightcoral", alpha = 0.7) +
          geom_smooth(method = "loess", formula = y ~ x, se = F,
            span = 0.125, color = "blue4", size = 1.25) +
          geom_smooth(aes(y = -10 * Tote), method = "loess",
            formula = y ~ x, se = F, span = 0.125, color = "red3",
            size = 1.25) +
          annotate("text", x = min(pltdatI$Datum), y = 0,
            label = "Infektionen", size = 6, hjust = -0.2,
            col = "blue4", angle = 90) +
          annotate("text", x = min(pltdatI$Datum), y = 0,
            label = "Tote", size = 6, hjust = 1.35,
            col = "red3", angle = 90) +
          scale_y_continuous(breaks = brks ,
            labels = brks/c(rep(-10, 4), rep(1, 5))) +
          theme(axis.text.x = element_text(angle = 60, size = 10,
            hjust = 1, vjust = 1),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_blank())
      }
    )
    pltI
  })
  
  # The table of ranking by Country
  output$I_tab = renderDataTable({
    datatable(D$topI, escape = F, selection = 'single',
      options = list(pageLength = 200, dom = "t",
        autoWidth = FALSE, scrollX = TRUE,
        order = list(list(3, 'desc')), fnDrawCallback =
          htmlwidgets::JS("function(){HTMLWidgets.staticRender();}")),
      colnames = c("Land", "", "7 Tage Inzidenz",
        "Neue Fälle", "Fälle Gesamt", "Durchseuchung",
        "Neue Tote", "Tote Gesamt", "Einwohner")) %>%
      formatStyle("New100k", fontWeight = "bold",
        backgroundColor = styleInterval(c(35, 50, 100, 200, 500),
          c("#62de4a", "#fce034", "#f14a26", "#800e0e", "#810067",
            "#580a99")),
        color = styleInterval(100, c("#000000", "#ffffff"))) %>%
      formatStyle("Country", fontWeight = "bold") %>%
      formatCurrency("Gesamtinfektionen", currency = "", interval = 3,
        mark = ",", digits = 0) %>%
      formatCurrency("NeueTote", currency = "", interval = 3,
        mark = ",", digits = 0) %>%
      formatCurrency("ToteGesamt", currency = "", interval = 3,
        mark = ",", digits = 0) %>%
      formatCurrency("Population", currency = "", interval = 3,
        mark = ",", digits = 0) %>%
      formatCurrency("Neuinfektionen", currency = "+", interval = 3,
        mark = ",", digits = 0) %>%
      formatCurrency("Durchseuchung", currency = "%", before = FALSE,
        digits = 2) %>%
      spk_add_deps()
  })
  
}


# Run the application
shinyApp(ui = ui, server = server)
