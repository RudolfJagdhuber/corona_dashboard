library(shiny)
library(shinydashboard)
library(shinycssloaders) # Loading animation
library(data.table)
library(curl) # fread with https connection
library(ggplot2);theme_set(theme_bw())
library(sparkline) # Sparklines in Tables
library(DT) # Responsive Tables


update_data = function() {
    
    # function to compute vector of summed infections for last n days
    # ni = New infections vector sorted by date (newest first!)
    # VERIFY THAT THE DATES HAVE NO GAPS!
    new7 = function(ni, n = 7) {
        res = rep(0, length(ni))
        for (i in 1:(length(ni) - n + 1)) res[i] = sum(ni[i:(i + n - 1)])
        return(res)
    }

    ## DOWNLOAD DATA SETS
    
    withProgress(max = 3, {
        
        incProgress(1, message = "Aktualisiere RKI Daten ...")
        # Read RKI Data
        RKI = fread(paste0("https://opendata.arcgis.com/datasets/",
            "dd4580c810204019a7b8eb3e0b329dd6_0.csv"), encoding = "UTF-8",
            select = c("Meldedatum", "Bundesland", "IdLandkreis", "Landkreis",
                "AnzahlFall", "AnzahlTodesfall"))
        
        incProgress(1, message = "Aktualisiere WHO Daten ...")
        # Read WHO Data
        WHO = fread("https://covid19.who.int/WHO-COVID-19-global-data.csv",
            colClasses = c("Date", rep("character", 3), rep("integer", 4)),
            select = c("Date_reported", "Country", "New_cases", "New_deaths"))
        
        incProgress(1, detail = "fertig!")
    })

    withProgress(max = 4, message = "Daten werden verarbeitet...", {
        
        ## PROCESS AND FORMAT DATA
        
        ##############################
        ### International WHO Data ###
        ##############################
        
        colnames(WHO)[1] = "Datum"
        
        # Important: INSERT MISSING DATES WITH 0 (For 7day value consistency)
        all_vals = data.table(expand.grid(
            Datum = seq(min(WHO$Datum), max(WHO$Datum), 1),
            Country = unique(WHO$Country)))
        WHO = merge(all_vals, WHO, by = c("Datum", "Country"), all = TRUE)
        WHO[is.na(WHO)] = 0
        #WHO[is.na(New_cases) & Datum != max(Datum), New_cases := 0]
        #WHO[is.na(New_deaths) & Datum != max(Datum), New_deaths := 0]
        
        # Read Census Data and remove WHO data without population info
        pop_WHO = fread("./www/pop_WHO.csv")
        pop_WHO$Population = 1000 * pop_WHO$Population # pop is in 1k precision
        WHO = merge(WHO, pop_WHO, by = "Country")
        
        # Insert Cumulative Cases and Deaths
        setorder(WHO, Country, Datum)
        WHO[, c("Gesamtinfektionen", "GesamtTote") := .(cumsum(New_cases),
            cumsum(New_deaths)), by = Country]
        
        # Add info of relative infections within last 7 days
        setorder(WHO, Country, -Datum)
        WHO[, New7 := new7(New_cases), by = Country]
        WHO[, New100k := round(1e5 * New7 / Population, 2)]
        
        
        incProgress(1)
        # Compute Overview table worldwide
        topI = WHO[, .(Spark = spk_chr(rev(New100k)), New100k = New100k[1],
            Neuinfektionen = New_cases[1], 
            Gesamtinfektionen = Gesamtinfektionen[1],
            Durchseuchung = round(100*Gesamtinfektionen[1] / Population[1], 2),
            NeueTote = New_deaths[1], ToteGesamt = GesamtTote[1],
            Population = Population[1]), by = Country]
        setorder(topI, -New100k)
        
        incProgress(1)
        
        #######################
        ### German RKI Data ###
        #######################
        
        colnames(RKI)[1] = "Datum"
        RKI$Datum = as.Date(substring(RKI$Datum, 1, 10), "%Y/%m/%d")
        
        # Hotfix: Sum Berlin Data to one element as census data is not that detailed
        RKI[Bundesland == "Berlin",
            c("IdLandkreis", "Landkreis") := .(11000, "SK Berlin")]
        
        # Simplify by grouping days
        RKI = RKI[,.(Neuinfektionen = sum(AnzahlFall), NeueTote = sum(AnzahlTodesfall)),
            by = c("Datum", "Bundesland", "IdLandkreis", "Landkreis")]
        
        # Important: INSERT MISSING DATES WITH 0 (For 7day value consistency)
        info_LK = RKI[, .(Bundesland = Bundesland[1]), 
            by = .(IdLandkreis, Landkreis)]
        all_vals_G = data.table(expand.grid(
            Datum = seq(min(RKI$Datum), max(RKI$Datum), 1),
            IdLandkreis = unique(RKI$IdLandkreis)))
        RKI = merge(all_vals_G, RKI[,c("Datum", "IdLandkreis", "Neuinfektionen",
            "NeueTote")], by = c("Datum", "IdLandkreis"), all = TRUE)
        RKI = merge(RKI, info_LK, by = "IdLandkreis", allow.cartesian = TRUE)
        RKI[is.na(RKI)] = 0
        
        # Read Census Data by Landkreis and remove RKI data without population info
        pop_RKI = fread("./www/pop_LK.csv", select = c("ID", "Insgesamt"),
            col.names = c("IdLandkreis", "Population"))
        RKI = merge(RKI, pop_RKI, by = "IdLandkreis")
        
        # Insert Cumulative Cases and Deaths
        setorder(RKI, IdLandkreis, Datum)
        RKI[, c("Gesamtinfektionen", "GesamtTote") := .(cumsum(Neuinfektionen),
            cumsum(NeueTote)), by = IdLandkreis]
        
        # Add info of relative infections within last 7 days
        setorder(RKI, IdLandkreis, -Datum)
        RKI[, New7 := new7(Neuinfektionen), by = IdLandkreis]
        RKI[, New100k := round(1e5 * New7 / Population, 2)]
        
        
        incProgress(1)
        # Pre-Compute German wide top lists per Bundesland
        topBL = RKI[,.(New7 = sum(New7), Neuinfektionen = sum(Neuinfektionen),
            Gesamt = sum(Gesamtinfektionen), Population = sum(Population)),
            by = .(Bundesland, Datum)]
        topBL = topBL[,.(Spark = spk_chr(rev(round(1e5 * New7 / Population, 2))),
            New100k = round(1e5 * New7[1] / Population[1], 2),
            Neuinfektionen = Neuinfektionen[1], Gesamt = Gesamt[1]), by = Bundesland]
        setorder(topBL, -New100k)
        
        # Pre-Compute German wide top lists per Landkreis
        topLK = RKI[,.(Spark = spk_chr(rev(New100k)),
            New100k = New100k[1],
            Neuinfektionen = Neuinfektionen[1], Gesamt = Gesamtinfektionen[1],
            Bundesland = Bundesland[1]), by = Landkreis]
        setorder(topLK, -New100k)
        
        incProgress(1)
        
        D = list(RKI = RKI, WHO = WHO, topBL = topBL, topLK = topLK, 
            topI = topI, date_updated = Sys.Date(),
            time_updated = format(Sys.time(), "%H:%M Uhr"))
        
        saveRDS(D, file = "./www/data.rds")
        
    })
    
    return(D)
}


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
                "Information zu erhalten. Inzwischen hat sich viel getan und",
                "die 7-Tage Inzidenz ist in aller Munde. Ich werde",
                "diese Plattform trotzdem weiter auf dem Laufenden halten und",
                "aktuelle Kennzahlen versuchen einzupflegen. Falls",
                "Inhalte fehlerhaft sind oder Daten fehlen, ist dies zu 99%",
                "auf die Datenquelle zurückzuführen und löst sich meist mit",
                "der Zeit von alleine."),
            h2("Noch eine Corona Übersicht?"),
            p(class = "lead", "Neuinfektionen oder Gesamtinfektionen lassen",
                "sich auf vielen Seiten im Internet finden. Diese Maßzahlen",
                "haben jedoch klare Schwächen. Neuinfektionen können je nach ",
                "Tag stark schwanken, und Gesamtinfektionen sagen nichts über",
                "die jetzige Situation aus. Beide sind ausserdem stark",
                "abhängig von der Bevölkerungsmenge in ihrer Interpretation.",
                "In Deutschland findet man deshalb vermehrt eine",
                "andere und deutlich aussagekräftigere Maßzahl:"), 
            p(class = "lead", strong(">> Anzahl Neuinfektionen der letzten 7",
                    "Tage pro 100,000 Einwohner <<")), 
            p(class = "lead", "Meine Corona Übersicht legt den",
                "Fokus komplett auf diese Zahl und erlaubt somit eine ",
                "Einschätzung des Weltgeschehens.",
                "Sie ist die Grundlage für Maßnahmen in Deutschland. Für",
                "Bayern gelten aktuell folgende Grenzwerte:"),
            column(width = 12, img(style = 
                    'height: auto; max-width: 100%', 
                src = "corona_ampel.jpg", width = "720"), br(),
                "Quelle:", a(href = paste0("https://www.radioarabella.de/",
                    "arabella-aktuell/regierungserklaeurung-soeder-bayern/"),
                    paste0("https://www.radioarabella.de/arabella-aktuell/",
                        "regierungserklaeurung-soeder-bayern/"))),
            br(), ".", br(),
            p(class = "lead", "Alle Inzidenzen (auch die weltweiten Zahlen)", 
                "sind nach diesen Grenzwerten eingefärbt. Zusätzlich sind die",
                "Aktuell relevanten Grenzwerte 200 in lila und 500 in pink",
                "farblich gekennzeichnet. (Stand: 12.12.2020)"),
            h2("Bedienung"),
            p(class = "lead", "Im Menü auf der linken Seite lassen sich eine",
                "deutschlandweite, sowie eine weltweite Übersicht auswählen."),
            p(class = "lead", "Tabellen erlauben eine Auswahl von Zeilen, ",
                "wonach alle Grafiken und Daten auf die Auswahl angepasst",
                "werden. Spalten lassen sich durch Klick auf den Namen",
                "sortieren"),
            h2("Datengrundlage"),
            p(class = "lead", "Die deutschlandweite Übersicht basiert auf",
                "Daten des Robert-Koch-Instituts. Die weltweite Übersicht",
                "nutzt offizielle Daten der World-Health-Organization. Alle",
                "Datensätze werden täglich abgerufen und aufbereitet, sodass",
                "dargestellte Ergebnisse dem neuesten Stand entsprechen."),
            p(class = "lead", "Der aktuelle Stand ist :",
                textOutput("reloadDate", container = strong)),
            p(class = "lead", "Falls Sie die Daten jetzt manuell aktualisieren",
                "möchten klicken Sie bitte hier:"),
            actionButton("dataUpdate", "Daten aktualisieren"),
            br(), br(), br(),
            p(class = "lead", "Die Daten stammen aus folgenden Quellen:"),
            p("RKI-Datenquelle:", br(), 
            a(href = paste0("https://opendata.arcgis.com/datasets/",
                "dd4580c810204019a7b8eb3e0b329dd6_0.csv"), 
                paste0("https://opendata.arcgis.com/datasets/",
                    "dd4580c810204019a7b8eb3e0b329dd6_0.csv"))), 
            br(),
            p("WHO-Datenquelle:", br(),
            a(href = "https://covid19.who.int/WHO-COVID-19-global-data.csv",
                "https://covid19.who.int/WHO-COVID-19-global-data.csv"))
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

    # Read the currently stored data filed
    D = readRDS("./www/data.rds")
    
    # If the data is older than a day, update it
    if (D$date_updated - Sys.Date() != 0) D = update_data()
    
    # Make it reactive to identify changes when user presses update button
    Dat = reactiveVal(D)
    
    # Functionality for the update button
    observeEvent(input$dataUpdate, {
        Dat(update_data())
    })
    

    
    ###### SERVER SIDE FUNCTIONALITY ###########################################
    
    ## The current file date is displayed at multiple places
    
    output$reloadDate = renderText(paste(format(Dat()$date_updated, "%d.%m.%Y"), 
        Dat()$time_updated))
    
    output$reloadDateD = renderText(paste0(
        "Daten des Robert-Koch-Instituts (Stand: ", 
        format(Dat()$date_updated, "%d.%m.%Y "), Dat()$time_updated, ")"))
    
    output$reloadDateI = renderText(paste0(
        "Daten der World-Health-Organization (Stand: ", 
        format(Dat()$date_updated, "%d.%m.%Y "), Dat()$time_updated, ")"))
    
    
    ## Germany Section
    
    # top Landkreis list depends on selected Bundesland
    topLKx = reactive({
        s = input$G_tabBL_rows_selected
        if (length(s)) {
            tmp = Dat()$topLK[Bundesland == Dat()$topBL$Bundesland[s]]
            setorder(tmp, -New100k)
        } else return(Dat()$topLK)
    })
    
    # The data subsetted to all selections
    dataG = reactive({
        s_LK = input$G_tabLK_rows_selected
        s_BL = input$G_tabBL_rows_selected
        # if an LK is selected, that goes first
        if (length(s_LK)) {
            tmp = Dat()$RKI[Landkreis == topLKx()$Landkreis[s_LK]] 
        } else if (length(s_BL)) {
            tmp = Dat()$RKI[Bundesland == Dat()$topBL$Bundesland[s_BL]] 
        } else tmp = Dat()$RKI

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
            ifelse(length(s_BL) == 1, Dat()$topBL$Bundesland[s_BL], 
                "ganz Deutschland")))
    })
    
    # The main plot of the selected data
    output$G_plot = renderPlot({
        
        pltdat = dataG()
        today = max(pltdat$Datum)
        secax = max(pltdat$Neuinfektionen)/max(pltdat$Tote)
        
        switch(input$G_plottype,
            "Neuinfektionen" = {
                ggplot(pltdat, aes(x = Datum, y = Neuinfektionen)) +
                    geom_bar(stat = "identity", width = 1, fill = "#3c8dbc",
                        alpha = 0.5) +
                    scale_x_date(date_labels = "%d.%m", 
                        date_breaks = "17 day") +
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
                        date_breaks = "17 day") +
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
                        date_breaks = "17 day") +
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
                        date_breaks = "17 day") +
                    labs(y = "Infektionen Gesamt") +
                    theme(axis.text.x = element_text(angle = 60, size = 10,
                        hjust = 1, vjust = 1),
                        axis.title.x = element_blank(),
                        plot.title = element_blank(),
                        axis.text.y = element_text(angle = 90, hjust = 0.5))
            },
            "Infektion und Tod" = {
                ggplot(pltdat, aes(x = Datum, y = Neuinfektionen)) +
                    scale_x_date(date_labels = "%d.%m", 
                        date_breaks = "17 day") +
                    coord_cartesian(ylim = c(0, max(pltdat$Neuinfektionen))) +
                    geom_bar(aes(y = secax * Tote), stat = "identity", 
                        width = 1, fill = "lightcoral", alpha = 0.4) +
                    geom_bar(stat = "identity", width = 1, fill = "#3c8dbc", 
                        alpha = 0.4) +
                    geom_smooth(aes(y = secax * Tote), 
                        method = "loess", formula = y ~ x, se = F, 
                        span = 0.125, color = "red3", size = 1.25) + 
                    geom_smooth(method = "loess", formula = y ~ x, se = F, 
                            span = 0.125, color = "blue4", size = 1.25) + 
                    scale_y_continuous("Neuinfektionen (blau)", 
                        sec.axis = sec_axis(trans = (~./secax),  
                            name = "Todesfälle (rot)")) +
                    theme(axis.text.x = element_text(angle = 60, size = 10,
                        hjust = 1, vjust = 1),
                        axis.title.x = element_blank(),
                        plot.title = element_blank(),
                        axis.text.y.right = element_text(angle = 90, 
                            hjust = 0.5),
                        axis.text.y = element_text(angle = 90, hjust = 0.5))
            }
        )
    })
   
    # The table of ranking by Bundesland
    output$G_tabBL = renderDataTable({
        datatable(Dat()$topBL, escape = F, selection = 'single', 
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
            tmp = Dat()$WHO[Country == Dat()$topI$Country[s_C]]
        } else tmp = Dat()$WHO
        
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
        paste("Daten für", ifelse(length(s_C) == 1, Dat()$topI$Country[s_C], 
            "alle Länder der Welt"))
    })
    
    # The main plot of the selected data
    output$I_plot = renderPlot({
        pltdatI = dataI()
        today = max(pltdatI$Datum)
        secaxI = max(pltdatI$Neuinfektionen)/max(pltdatI$Tote)
        
        switch(input$I_plottype,
            "Neuinfektionen" = {
                pltI = ggplot(pltdatI, aes(x = Datum, y = Neuinfektionen)) +
                    geom_bar(stat = "identity", width = 1, fill = "#3c8dbc",
                        alpha = 0.5) +
                    scale_x_date(date_labels = "%d.%m", 
                        date_breaks = "17 day") +
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
                        date_breaks = "17 day") +
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
                        date_breaks = "17 day") +
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
                        date_breaks = "17 day") +
                    labs(y = "Infektionen Gesamt") +
                    theme(axis.text.x = element_text(angle = 60, size = 10,
                        hjust = 1, vjust = 1),
                        axis.title.x = element_blank(),
                        plot.title = element_blank(),
                        axis.text.y = element_text(angle = 90, hjust = 0.5))
            },
            "Infektion und Tod" = {
                pltI = ggplot(pltdatI, aes(x = Datum, y = Neuinfektionen)) +
                    scale_x_date(date_labels = "%d.%m", 
                        date_breaks = "17 day") +
                    coord_cartesian(ylim = c(0, max(pltdatI$Neuinfektionen))) +
                    geom_bar(aes(y = secaxI * Tote), stat = "identity", 
                        width = 1, fill = "lightcoral", alpha = 0.4) +
                    geom_bar(stat = "identity", width = 1, fill = "#3c8dbc", 
                        alpha = 0.4) +
                    geom_smooth(aes(y = secaxI * Tote), 
                        method = "loess", formula = y ~ x, se = F, 
                        span = 0.125, color = "red3", size = 1.25) + 
                    geom_smooth(method = "loess", formula = y ~ x, se = F, 
                        span = 0.125, color = "blue4", size = 1.25) + 
                    scale_y_continuous("Neuinfektionen (blau)", 
                        sec.axis = sec_axis(trans = (~./secaxI),  
                            name = "Todesfälle (rot)")) +
                    theme(axis.text.x = element_text(angle = 60, size = 10,
                        hjust = 1, vjust = 1),
                        axis.title.x = element_blank(),
                        plot.title = element_blank(),
                        axis.text.y.right = element_text(angle = 90, 
                            hjust = 0.5),
                        axis.text.y = element_text(angle = 90, hjust = 0.5))
            }
        )
        pltI
    })
    
    # The table of ranking by Country
    output$I_tab = renderDataTable({
        datatable(Dat()$topI, escape = F, selection = 'single',
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
