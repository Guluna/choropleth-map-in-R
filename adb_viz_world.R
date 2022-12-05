# Install the required packages (do this step only once)
install.packages("rgdal")
install.packages("dplyr")
install.packages("leaflet")
install.packages("htmlwidgets")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("readxl")
install.packages("RColorBrewer")
install.packages("leaflet.extras")

# Load the required libraries (do this step in every R session)
library(rgdal)
library(dplyr)
library(leaflet)
library(htmlwidgets)
library(ggplot2)        
library(tidyverse)     
library(readxl)
library(RColorBrewer)
library(leaflet.extras)

# Download the shapefile in the current folder
download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="world_shape_file.zip")

# Unzip it. You now have 4 files. One of these files is a .shp file! (TM_WORLD_BORDERS_SIMPL-0.3.shp)
unzip("world_shape_file.zip")

# Read this shape file with the rgdal library.
world_spdf <- readOGR( 
  dsn= paste0(getwd(),"") , 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)

# reading in excel file, setting default view to "data" sheet, skipping 1st row before reading data, using 1st row as column name
data_imf <- read_excel("imf-who-covid-19-vaccine-supply-tracker.xlsx",  
                       sheet="data", 
                       skip=1, 
                       col_names = TRUE)

# subsetting only relevant columns from our custom dataset
data_imf_relevant <- data_imf[, c("Countries and areas", "ISO3", "Population", "Secured Vaccine (millions of courses)", "Secured Vaccine (% of population)")]

# renaming columns
data_imf_relevant <- data_imf_relevant %>% rename( Country = `Countries and areas`,
    Vaccine_millions = `Secured Vaccine (millions of courses)`,
    Vaccine_percent = `Secured Vaccine (% of population)`)

# Merging custom dataset columns with world map dataframe on ISO3 column 
world_spdf@data = data.frame(world_spdf@data, data_imf_relevant[match(world_spdf@data[,"ISO3"], data_imf_relevant[["ISO3"]]),])

# removing rows with NAs (i.e. removing regions that are in shape file but not in imf dataset) 
world_spdf <- subset(world_spdf, ISO3 %in% data_imf_relevant$ISO3 , )

# Create a color palette for the map:
mybins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100,Inf)
mypalette <- colorBin( palette="RdYlBu", domain=world_spdf@data$Vaccine_percent, na.color='transparent', bins=mybins, pretty = TRUE)

# Prepare the text for hoverover tooltips:
mytext <- paste(
  "Country: ", world_spdf@data$Country,"<br/>", 
  "Population: ", format(world_spdf@data$Population, big.mark = ","), "<br/>", 
  "<strong>Secured Vaccine (% of Population): ", round(world_spdf@data$Vaccine_percent, 2), "</strong> <br/>",
  "Secured Vaccine (Millions of courses:) ", round(world_spdf@data$Vaccine_millions, 2),
  sep="") %>%
  lapply(htmltools::HTML)

# Final Map
final_map  <- leaflet(world_spdf) %>% 
  setView( lat=10, lng=0 , zoom=1.5) %>%
  addPolygons( 
    fillColor = ~mypalette(world_spdf@data$Vaccine_percent), 
    stroke=TRUE, 
    fillOpacity = 0.9, 
    color="black", 
    weight=0.3,
    label = mytext,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  ) %>%
  addLegend( pal=mypalette, 
             values=world_spdf@data$Vaccine_percent, 
             opacity=0.9, 
             title = "Secured Vaccine (% of Population)", 
             position = "bottomleft")

# changing map background color to white
final_map_colored <- final_map %>% setMapWidgetStyle(list(background= "white"))
final_map_colored

# save the widget in a html file
saveWidget(final_map_colored, file=paste0( getwd(), "/final_map_world.html"))

