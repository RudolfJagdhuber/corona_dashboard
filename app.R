library(shiny)
library(shinydashboard)
library(data.table)
library(ggplot2);theme_set(theme_bw())
library(kableExtra)
library(plotly)
library(sparkline)
library(DT)


# function to compute vector of summed infections for last n days
# ni = New infections vector sorted by date (newest first!)
# ATTENTION: VERIFY THAT THE DATES HAVE NO GAPS!
new7 = function(ni, n = 7) {
    res = rep(0, length(ni))
    for (i in 1:(length(ni) - n + 1)) res[i] = sum(ni[i:(i + n - 1)])
    return(res)
}




##############################
### International WHO Data ###
##############################

WHO = fread("https://covid19.who.int/WHO-COVID-19-global-data.csv", 
    colClasses = c("Date", rep("character", 3), rep("integer", 4)), 
    drop = c("Country_code", "WHO_region"))
colnames(WHO)[1] = "Datum"

pop_WHO = fread("./www/pop_WHO.csv")
pop_WHO$Population = 1000 * pop_WHO$Population # pop is given in 1000 precision
# Remove data without pop info
WHO = merge(WHO, pop_WHO, by = "Country")

# Important: INSERT MISSING DATES WITH ZERO CASES (For 7day value consistency)
# I omit the newest date as the reporting time differs per country
all_vals = data.table(expand.grid(
    Datum = seq(min(WHO$Datum), max(WHO$Datum) - 1, 1),
    Country = unique(WHO$Country)))
WHO = merge(all_vals, WHO, by = c("Datum", "Country"), all.x = TRUE)
WHO[is.na(WHO)] = 0

# Add info of relative infections within last 7 days
setorder(WHO, Country, -Datum)
WHO[, New7 := new7(New_cases), by = Country]
WHO[, New100k := round(1e5 * New7 / Population, 2)]

# Compute Overview table worldwide
topI = WHO[, .(Spark = spk_chr(rev(New100k)), New100k = New100k[1], 
    Neuinfektionen = New_cases[1], Gesamtinfektionen = Cumulative_cases[1], 
    Durchseuchung = round(100 * Cumulative_cases[1] / Population[1], 2), 
    NeueTote = New_deaths[1], ToteGesamt = Cumulative_deaths[1],
    Population = Population[1]), by = Country]
setorder(topI, -Gesamtinfektionen)



#######################
### German RKI Data ###
#######################

# Read RKI Data
RKI = fread(paste0("https://opendata.arcgis.com/datasets/",
    "dd4580c810204019a7b8eb3e0b329dd6_0.csv"), encoding = "UTF-8", 
    select = c("Meldedatum", "Bundesland", "IdLandkreis", "Landkreis", 
        "AnzahlFall", "AnzahlTodesfall"))
colnames(RKI)[1] = "Datum"
RKI$Datum = as.Date(substring(RKI$Datum, 1, 10), "%Y/%m/%d")

# Hotfix: Sum Berlin Data to one element as census data is not that detailed 
RKI[Bundesland == "Berlin", 
    c("IdLandkreis", "Landkreis") := .(11000, "SK Berlin")]

# Simplify by grouping days
RKI = RKI[,.(Neuinfektionen = sum(AnzahlFall), NeueTote = sum(AnzahlTodesfall)),
    by = c("Datum", "Bundesland", "IdLandkreis", "Landkreis")]

# Important: INSERT MISSING DATES WITH ZERO CASES (For 7day value consistency)
# I omit the newest date as the reporting time differs per country
info_LK = RKI[, .(Bundesland = Bundesland[1]), by = .(IdLandkreis, Landkreis)]
all_vals_G = data.table(expand.grid(
    Datum = seq(min(RKI$Datum), max(RKI$Datum) - 1, 1),
    IdLandkreis = unique(RKI$IdLandkreis)))
RKI = merge(all_vals_G, RKI[,c("Datum", "IdLandkreis", "Neuinfektionen", 
    "NeueTote")], by = c("Datum", "IdLandkreis"), all.x = TRUE)
RKI[is.na(RKI)] = 0
RKI = merge(RKI, info_LK, by = "IdLandkreis")

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


# Pre-Compute German wide top lists per Landkreis 
topLK = RKI[,.(Spark = spk_chr(rev(New100k)), New100k = New100k[1], 
    Neuinfektionen = Neuinfektionen[1], Gesamt = Gesamtinfektionen[1], 
    Bundesland = Bundesland[1]), by = Landkreis]
setorder(topLK, -New100k)

# Pre-Compute German wide top lists per Bundesland 
topBL = RKI[,.(New7 = sum(New7), Neuinfektionen = sum(Neuinfektionen), 
    Gesamt = sum(Gesamtinfektionen), Population = sum(Population)), 
    by = .(Bundesland, Datum)]
topBL = topBL[,.(Spark = spk_chr(rev(round(1e5 * New7 / Population, 2))),
    New100k = round(1e5 * New7[1] / Population[1], 2), 
    Neuinfektionen = Neuinfektionen[1], Gesamt = Gesamt[1]), by = Bundesland]
setorder(topBL, -New100k)



### Dashboard Programm Begins


# Define UI for application that draws a histogram
ui = dashboardPage(
    
    dashboardHeader(title = "Rudis Corona Übersicht", titleWidth = 300),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Deutschland (RKI Daten)", tabName = "german", 
                icon = icon("map-marker-alt")),
            menuItem("International (WHO Daten)", tabName = "international", 
                icon = icon("globe"))
        )
    ),
    
    dashboardBody(tabItems(
        tabItem(tabName = "german", 
            paste0("(Stand: ", format(Sys.time(), "%d.%m.%Y %H:%M Uhr"), ")"),
            h2("Corona Übersicht der Daten des Robert-Koch-Instituts"),
            
            fluidRow(
                column(width = 4,
                    
                    box(width = 12, height = 125, title = "Auswahl der Region", 
                        solidHeader = TRUE, status = "info", 
                        column(width = 6,
                            selectInput("G_BL", 
                                label = "Bundesland", 
                                selected = "Ganz Deutschland", 
                                choices = c("Ganz Deutschland", 
                                    sort(unique(RKI$Bundesland))))),
                        column(width = 6,
                            selectInput("G_LK", 
                                label = "Landkreis", 
                                selected = "Alle Landkreise", 
                                choices = c("Alle Landkreise", 
                                    sort(unique(RKI$Landkreis)))))),
                    
                    box(width = 12, height = 425,
                        title = "Tabelle des zeitliche Verlaufs", 
                        solidHeader = TRUE, status = "primary", 
                        dataTableOutput("G_infobox")),
                
                ),
                column(width = 8,
                    
                    box(width = 6, height = 125, 
                        title = "Auswahl der Zielgröße", 
                        solidHeader = TRUE, status = "info", 
                        radioButtons("G_plottype", choices = c("Neuinfektionen",
                            "7 Tage / 100k Einwohner", "Gesamtinfektionen"),
                            label = "Was soll dargestellt werden?", 
                            selected = "7 Tage / 100k Einwohner", 
                            inline = TRUE)),
                    
                    box(width = 6, height = 125, 
                        title = "Anpassungskurve (nur bei Neuinfektionen)",
                        solidHeader = TRUE, status = "info", 
                        sliderInput("G_span", min = 0, max = 1, value = 0.125,
                            step = 0.001, label = paste("Glattheitsparameter",
                                "der Loess Kurve (span)"))),
                    
                    box(width = 12, height = 425, 
                        title = "Grafik des zeitlichen Verlaufs", 
                        solidHeader = T, status = "primary", 
                        plotlyOutput("G_plot", height = 370))
                )
            ),
            
            fluidRow(
                box(width = 5, title = "Spitzenreiter Bundesländer", 
                    solidHeader = T, status = "primary", 
                    dataTableOutput("G_tabBL")),
                
                box(width = 7, title = "Spitzenreiter Landkreise", 
                    solidHeader = T, status = "primary", 
                    dataTableOutput("G_tabLK"))
            ),
            
            
            
        ),
        
        tabItem(tabName = "international", 
            
            paste0("(Stand: ", format(Sys.time(), "%d.%m.%Y %H:%M Uhr"), ")"),
            h2("Corona Übersicht der Daten der World-Health-Organization"),
            
            fluidRow(
                box(width = 12, title = "Länder der Welt", 
                    solidHeader = T, status = "primary", 
                    dataTableOutput("I_tab"))
            ),
            
            h2("Detailansicht"),
            
            fluidRow(
                column(width = 4,
                    
                    box(width = 12, height = 125, title = "Länderauswahl", 
                        solidHeader = TRUE, status = "info", 
                        selectInput("I_country", 
                            label = "Daten anzeigen für", 
                            selected = "Weltweit", 
                            choices = c("Weltweit", 
                                    sort(unique(WHO$Country))))
                    ),
                    
                    box(width = 12, height = 425,
                        title = "Tabelle des zeitliche Verlaufs", 
                        solidHeader = TRUE, status = "primary", 
                        dataTableOutput("I_infobox")),
                    
                ),
                column(width = 8,
                    
                    box(width = 6, height = 125, 
                        title = "Auswahl der Zielgröße", 
                        solidHeader = TRUE, status = "info", 
                        radioButtons("I_plottype", choices = c("Neuinfektionen",
                            "7 Tage / 100k Einwohner", "Gesamtinfektionen"),
                            label = "Was soll dargestellt werden?", 
                            selected = "7 Tage / 100k Einwohner", 
                            inline = TRUE)),
                    
                    box(width = 6, height = 125, 
                        title = "Anpassungskurve (nur bei Neuinfektionen)", 
                        solidHeader = TRUE, status = "info", 
                        sliderInput("I_span", min = 0, max = 1, value = 0.125,
                            step = 0.001, label = paste("Glattheitsparameter",
                                "der Loess Kurve (span)"))),
                    
                    box(width = 12, height = 425, 
                        title = "Grafik des zeitlichen Verlaufs", 
                        solidHeader = T, status = "primary", 
                        plotlyOutput("I_plot", height = 370))
                )
            )
        )
    ))
)
    
 

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    ###########
    # Germany #
    ###########
    
    dataG = reactive({
        # create temporary data set of the user selection, named: tmp
        if (input$G_BL != "Ganz Deutschland") {
            tmp = RKI[Bundesland == input$G_BL] 
        } else {
            tmp = RKI  
        }
            
        if (input$G_LK != "Alle Landkreise") {
            tmp = tmp[Landkreis == input$G_LK]
        }
        
        tmp = tmp[,.(New100k = round(1e5 * sum(New7) / sum(Population), 2),
            Neuinfektionen = sum(Neuinfektionen), 
            Gesamtinfektionen = sum(Gesamtinfektionen)), by = Datum]
        
        return(tmp)
    })
    
    output$G_infobox = renderDataTable({
        tab = data.frame(dataG())
        rownames(tab) = format(tab$Datum, "%d.%m.%Y")
        
        datatable(tab[,2:4], options = list(pageLength = 7,
            dom = "tp"), colnames = c(" ",
                "7 Tage / 100k Einwohner", "Neuinfektionen", "Gesamt")) %>% 
            formatStyle(" ", fontWeight = "bold") %>%
            formatStyle("New100k", fontWeight = "bold", 
                backgroundColor = styleInterval(c(10, 20, 30, 50), c("#FFFFE0", 
                    "#F0E68C", "#FFC500", "#FF8000", "#FF0000"))) %>%
            formatCurrency("Gesamtinfektionen", currency = "", interval = 3, 
                mark = ",", digits = 0) %>%
            formatCurrency("Neuinfektionen", currency = "+", interval = 3, 
                mark = ",", digits = 0)
        
    })
    
    output$G_plot = renderPlotly({
        
        pltdat = dataG()
        
        switch(input$G_plottype,
            "Neuinfektionen" = {
                plt = ggplot(pltdat, aes(x = Datum, y = Neuinfektionen)) +
                    geom_bar(stat = "identity", width = 1, fill = "#3c8dbc") +
                    scale_x_date(date_labels = "%d.%m", 
                        date_breaks = "10 day") +
                    labs(y = "Anzahl an Neuinfektionen", title = 
                            paste("Daten für", input$G_LK, "in", input$G_BL)) +
                    geom_smooth(method = "loess", formula = y ~ x, se = F, 
                        span = input$G_span, color = "blue4") + 
                    coord_cartesian(ylim = c(0, max(pltdat$Neuinfektionen))) +
                    theme(axis.text.x = element_text(angle = 60, size = 10,
                        hjust = 1, vjust = 1),
                        axis.title.x = element_blank(),
                        axis.text.y = element_text(angle = 90, hjust = 0.5))
            },
            
            "7 Tage / 100k Einwohner" = {
                dpl_G = data.frame(x = rep(rep(range(pltdat$Datum), each = 2), 5), 
                    y = c(0, 10, 10, 0, 10, 20, 20, 10, 20, 30, 30, 20, 30, 
                        50, 50, 30, 50, 1000, 1000, 50),
                    Wert = rep(c("< 10", "< 20", "< 30", "< 50", 
                        "> 50"), each = 4))
                
                plt = ggplot(pltdat, aes(x = Datum, y = New100k)) +
                    geom_polygon(data = dpl_G, aes(x = x, y = y, fill = Wert),
                        alpha = 0.7) +
                    geom_area(stat = "identity", fill = "#3c8dbc") +
                    geom_line(col = "blue4") +
                    scale_fill_manual(values = c("#FFFFE0", "#F0E68C", 
                        "#FFC500", "#FF8000", "#FF0000")) + 
                    scale_x_date(date_labels = "%d.%m", 
                        date_breaks = "10 day") +
                    coord_cartesian(ylim = c(0, max(pltdat$New100k))) +
                    labs(y = "7 Tage / 100k Einwohner", title = 
                            paste("Daten für", input$G_LK, "in", input$G_BL)) +
                    theme(axis.text.x = element_text(angle = 60, size = 10,
                        hjust = 1, vjust = 1), legend.position = "none",
                        axis.title.x = element_blank(),
                        axis.text.y = element_text(angle = 90, hjust = 0.5))
            },
            "Gesamtinfektionen" = {
                plt = ggplot(pltdat, aes(x = Datum, y = Gesamtinfektionen)) +
                    geom_area(stat = "identity", fill = "#3c8dbc") +
                    geom_line(col = "blue4") +
                    scale_x_date(date_labels = "%d.%m", 
                        date_breaks = "10 day") +
                    labs(y = "Infektionen Gesamt", title = 
                            paste("Daten für", input$G_LK, "in", input$G_BL)) +
                    theme(axis.text.x = element_text(angle = 60, size = 10,
                        hjust = 1, vjust = 1),
                        axis.title.x = element_blank(),
                        axis.text.y = element_text(angle = 90, hjust = 0.5))
            }
        )
        
        ggplotly(plt)
    })
   
    output$G_tabLK = renderDataTable({
        datatable(topLK, escape = F, options = list(pageLength = 15, 
            dom = "ltip", order = list(list(3, 'desc')), fnDrawCallback = 
                htmlwidgets::JS("function(){HTMLWidgets.staticRender();}")), 
            colnames = c("Landkreis", "", "7 Tage / 100k Einwohner", 
                "Neuinfektionen", "Gesamt", "Bundesland")) %>%  
            formatStyle("New100k", fontWeight = "bold", 
                backgroundColor = styleInterval(c(10, 20, 30, 50), c("#FFFFE0", 
                    "#F0E68C", "#FFC500", "#FF8000", "#FF0000"))) %>%
            formatStyle("Landkreis", fontWeight = "bold") %>% 
            formatCurrency("Gesamt", currency = "", interval = 3, 
                mark = ",", digits = 0) %>%
            formatCurrency("Neuinfektionen", currency = "+", interval = 3, 
                mark = ",", digits = 0) %>% 
            spk_add_deps()
    })
    
    output$G_tabBL = renderDataTable({
        datatable(topBL, escape = F, options = list(pageLength = 16, 
            dom = "t", order = list(list(3, 'desc')), fnDrawCallback = 
                htmlwidgets::JS("function(){HTMLWidgets.staticRender();}")), 
            colnames = c("Bundesland", "", "7 Tage / 100k Einwohner", 
                "Neuinfektionen", "Gesamt")) %>% 
            formatStyle("New100k", fontWeight = "bold", 
                backgroundColor = styleInterval(c(10, 20, 30, 50), c("#FFFFE0", 
                    "#F0E68C", "#FFC500", "#FF8000", "#FF0000"))) %>%
            formatStyle("Bundesland", fontWeight = "bold") %>% 
            formatCurrency("Gesamt", currency = "", interval = 3, 
                mark = ",", digits = 0) %>%
            formatCurrency("Neuinfektionen", currency = "+", interval = 3, 
                mark = ",", digits = 0) %>% 
            spk_add_deps()
    })
    
    
    
    #################
    # International #
    #################
    
    dataI = reactive({
        # create temporary data set of the user selection, named: tmp
        if (input$I_country != "Weltweit") {
            tmp = WHO[Country == input$I_country]
        } else {
            tmp = WHO
        }
        
        # Compute / Subset statistics of interest
        tmp = tmp[,.(New100k = round(1e5 * sum(New7) / sum(Population), 2),
            Neuinfektionen = sum(New_cases), 
            Gesamtinfektionen = sum(Cumulative_cases)), by = Datum]
        setorder(tmp, -Datum)
        
        return(tmp) 
    })
    
    output$I_tab = renderDataTable({
        datatable(topI, escape = F, 
            options = list(pageLength = 10, dom = "ltip", 
                order = list(list(5, 'desc')), fnDrawCallback = 
                htmlwidgets::JS("function(){HTMLWidgets.staticRender();}")), 
            colnames = c("Land", "", "7 Tage / 100k Einwohner", 
                "Neuinfektionen", "Infektionen Gesamt", "Durchseuchung", 
                "Neue Tote", "Tote Gesamt", "Einwohner")) %>% 
            formatStyle("New100k", fontWeight = "bold", 
                backgroundColor = styleInterval(c(10, 20, 30, 50), c("#FFFFE0", 
                    "#F0E68C", "#FFC500", "#FF8000", "#FF0000"))) %>%
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
    
    output$I_infobox = renderDataTable({
        tab = data.frame(dataI())
        rownames(tab) = format(tab$Datum, "%d.%m.%Y")
        
        datatable(tab[,-1], options = list(pageLength = 7, dom = "tp"), 
            colnames = c(" ", "7 Tage / 100k Einwohner", "Neuinfektionen", 
                "Gesamt")) %>% 
            formatStyle(" ", fontWeight = "bold") %>%
            formatStyle("New100k", fontWeight = "bold", 
                backgroundColor = styleInterval(c(10, 20, 30, 50), c("#FFFFE0", 
                    "#F0E68C", "#FFC500", "#FF8000", "#FF0000"))) %>%
            formatCurrency("Gesamtinfektionen", currency = "", interval = 3, 
                mark = ",", digits = 0) %>%
            formatCurrency("Neuinfektionen", currency = "+", interval = 3, 
                mark = ",", digits = 0)
    })
    
    output$I_plot = renderPlotly({
        
        pltdatI = dataI()
        
        switch(input$I_plottype,
            "Neuinfektionen" = {
                pltI = ggplot(pltdatI, aes(x = Datum, y = Neuinfektionen)) +
                    geom_bar(stat = "identity", width = 1, fill = "#3c8dbc") +
                    scale_x_date(date_labels = "%d.%m", 
                        date_breaks = "10 day") +
                    labs(y = "Anzahl an Neuinfektionen", 
                        title = paste("Daten für", input$I_country)) +
                    geom_smooth(method = "loess", formula = y ~ x, se = F, 
                        span = input$I_span, color = "blue4") + 
                    coord_cartesian(ylim = c(0, max(pltdatI$Neuinfektionen))) +
                    theme(axis.text.x = element_text(angle = 60, size = 10,
                        hjust = 1, vjust = 1),
                        axis.title.x = element_blank(),
                        axis.text.y = element_text(angle = 90, hjust = 0.5))
            },
            "7 Tage / 100k Einwohner" = {
                # Make polygon for background color
                dpl = data.frame(x = rep(rep(range(pltdatI$Datum), each = 2), 5), 
                    y = c(0, 10, 10, 0, 10, 20, 20, 10, 20, 30, 30, 20, 30, 
                        50, 50, 30, 50, 1000, 1000, 50),
                    Wert = rep(c("< 10", "< 20", "< 30", "< 50", 
                        "> 50"), each = 4))
                
                pltI = ggplot(pltdatI, aes(x = Datum, y = New100k)) +
                    geom_polygon(data = dpl, aes(x = x, y = y, fill = Wert),
                        alpha = 0.7) +
                    geom_area(stat = "identity", fill = "#3c8dbc") +
                    geom_line(col = "blue4") +
                    scale_fill_manual(values = c("#FFFFE0", "#F0E68C", 
                        "#FFC500", "#FF8000", "#FF0000")) + 
                    coord_cartesian(ylim = c(0, max(pltdatI$New100k))) +
                    scale_x_date(date_labels = "%d.%m", 
                        date_breaks = "10 day") +
                    labs(y = "7 Tage / 100k Einwohner", 
                        title = input$I_country) +
                    theme(axis.text.x = element_text(angle = 60, size = 10,
                        hjust = 1, vjust = 1), legend.position = "none",
                        axis.title.x = element_blank(),
                        axis.text.y = element_text(angle = 90, hjust = 0.5))
            },
            "Gesamtinfektionen" = {
                pltI = ggplot(pltdatI, aes(x = Datum, y = Gesamtinfektionen)) +
                    geom_area(stat = "identity", fill = "#3c8dbc") +
                    geom_line(col = "blue4") +
                    scale_x_date(date_labels = "%d.%m", 
                        date_breaks = "10 day") +
                    labs(y = "Infektionen Gesamt", 
                        title = paste("Daten für", input$I_country)) +
                    theme(axis.text.x = element_text(angle = 60, size = 10,
                        hjust = 1, vjust = 1),
                        axis.title.x = element_blank(),
                        axis.text.y = element_text(angle = 90, hjust = 0.5))
            }
        )
        
        ggplotly(pltI)
    })
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)



