# Helper to fetch the most recent RKI and WHO data sets, format and store it

# Note: I create a custom lib.loc on github to have writing access
library(data.table, lib.loc = "./tmpLib")
library(sparkline, lib.loc = "./tmpLib")

# function to compute vector of summed infections for last n days
# ni = New infections vector sorted by date (newest first!)
# VERIFY THAT THE DATES HAVE NO GAPS!
new7 = function(ni, n = 7) {
  res = rep(0, length(ni))
  for (i in 1:(length(ni) - n + 1)) res[i] = sum(ni[i:(i + n - 1)])
  return(res)
}

# Read RKI Data
RKI = fread(paste0("https://www.arcgis.com/sharing/rest/content/items/", 
  "66876b81065340a4a48710b062319336/data"),
  encoding = "UTF-8",
  select = c("Meldedatum", "Bundesland", "IdLandkreis", "Landkreis",
    "AnzahlFall", "AnzahlTodesfall"))

# Read WHO Data
WHO = fread("https://covid19.who.int/WHO-COVID-19-global-data.csv",
  colClasses = c("Date", rep("character", 3), rep("integer", 4)),
  select = c("Date_reported", "Country", "New_cases", "New_deaths"))


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


# Compute Overview table worldwide
topI = WHO[, .(Spark = spk_chr(rev(New100k)), New100k = New100k[1],
  Neuinfektionen = New_cases[1],
  Gesamtinfektionen = Gesamtinfektionen[1],
  Durchseuchung = round(100*Gesamtinfektionen[1] / Population[1], 2),
  NeueTote = New_deaths[1], ToteGesamt = GesamtTote[1],
  Population = Population[1]), by = Country]
setorder(topI, -New100k)


#######################
### German RKI Data ###
#######################

colnames(RKI)[1] = "Datum"
RKI$Datum = as.Date(RKI$Datum)

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


D = list(RKI = RKI, WHO = WHO, topBL = topBL, topLK = topLK,
  topI = topI, date_updated = Sys.Date(),
  time_updated = format(Sys.time(), "%H:%M Uhr"))

saveRDS(D, file = "./www/data.rds")
