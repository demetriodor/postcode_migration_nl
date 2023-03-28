### This script produces an interactive map of the population with migrant origin in the Netherlands


# Libraries and functions -------------------------------------------------
library(cbsodataR)
library (dplyr)
library(tidyr)

library(geojsonio)
library(rmapshaper)

library(leaflet)
library(htmltools)
library(htmlwidgets)

# Get postcode-level migration data from CBS ------------------------------
d<-cbsodataR::cbs_get_data('83503NED', Perioden='2021JJ00', Geslacht='T001038') %>%
  cbsodataR::cbs_add_label_columns() %>%
  select(Migratieachtergrond_label, Postcode_label, Bevolking_1) %>%
  filter (Migratieachtergrond_label == 'Totaal' |  Migratieachtergrond_label == 'Met migratieachtergrond' |  
            Migratieachtergrond_label == 'Niet-westerse migratieachtergrond' | Migratieachtergrond_label == 'Westerse migratieachtergrond',
          Postcode_label!='Niet in te delen', Postcode_label!='Nederland') %>% 
  tidyr::pivot_wider (names_from = Migratieachtergrond_label, values_from = Bevolking_1) %>% 
  mutate (migr_share = `Met migratieachtergrond` / Totaal * 100,
          w_migr_share = `Westerse migratieachtergrond` / Totaal * 100,
          nw_migr_share = `Niet-westerse migratieachtergrond` / Totaal * 100,
          pc4_code = Postcode_label) %>% 
  select (pc4_code, Totaal, migr_share, w_migr_share, nw_migr_share) %>% data.frame()


# Get the geo shape file --------------------------------------------------
# Get geojson shapefile from Opendatasoft. Reference: https://www.cbs.nl/nl-nl/dossier/nederland-regionaal/geografische-data/gegevens-per-postcode
# Dataset Identifier: georef-netherlands-postcode-pc4
# https://public.opendatasoft.com/explore/dataset/georef-netherlands-postcode-pc4/export/?dataChart=eyJxdWVyaWVzIjpbeyJjb25maWciOnsiZGF0YXNldCI6Imdlb3JlZi1uZXRoZXJsYW5kcy1wb3N0Y29kZS1wYzQiLCJvcHRpb25zIjp7fX0sImNoYXJ0cyI6W3siYWxpZ25Nb250aCI6dHJ1ZSwidHlwZSI6ImxpbmUiLCJmdW5jIjoiQ09VTlQiLCJzY2llbnRpZmljRGlzcGxheSI6dHJ1ZSwiY29sb3IiOiIjRkY1MTVBIn1dLCJ4QXhpcyI6InllYXIiLCJtYXhwb2ludHMiOiIiLCJ0aW1lc2NhbGUiOiJ5ZWFyIiwic29ydCI6IiJ9XSwiZGlzcGxheUxlZ2VuZCI6dHJ1ZSwiYWxpZ25Nb250aCI6dHJ1ZX0%3D&location=8,52.15708,5.29541&basemap=jawg.light

spdf <- geojsonio::geojson_read("./data/georef-netherlands-postcode-pc4.geojson",  what = "sp")
spdf <- subset (spdf, is.na(spdf@data$prov_name)==FALSE)

# Merge the two
spdf@data <- left_join(spdf@data, d, by='pc4_code')

# transform the geo data
spdf<-spTransform(spdf, CRS("+init=epsg:4326"))

# simplify the geo data to make the resulting files smaller
spdf <- rmapshaper::ms_simplify(spdf, keep = 0.1, keep_shapes = TRUE)

# Prepare the map features --------------------------------------------------

# prepare a color palette
pal <- leaflet::colorNumeric(palette='magma', reverse=FALSE, domain = c(0,100))

# prepare a variable with the popup text
spdf@data <- spdf@data %>% 
  dplyr::mutate(
    popup = paste0(
      "<b>", pc4_code, "</b> ",
      "<b>", gem_name, "</b> ",
      prov_name,
      "<br>Total population: ", "<b>", Totaal, "</b>",
      "<br><b>", round(migr_share, 0), "%", "</b>", " with migration origin, of which",
      "<br><b>", round(w_migr_share, 0), "%", "</b>", " Western and",
      "<br><b>", round(nw_migr_share, 0), "%", "</b>", " Non-Western"
      )) 

# prepare caption and title
title <- tags$div(HTML('<h3>Population with <b>migration origin</b> (born abroad or having a parent born abroad) in the Netherlands, 2021</h3>')) 
caption <- tags$div(HTML('<a href="https://dimiter.eu/">DToshkov, Data from CBS </a>'))


# Call the map --------------------------------------------------

map_leaflet <- leaflet::leaflet() %>% 
  addPolygons( color = "#222", weight = 2, opacity = 1, fillOpacity = 0.7,
               fillColor = ~pal(migr_share),
               popup = ~popup,
               label = ~lapply(pc4_code, htmltools::htmlEscape),
               labelOptions = leaflet::labelOptions(direction = "top"),
               highlight = leaflet::highlightOptions(color = "#FFF", bringToFront = TRUE),
               data = spdf,
               group = 'Total migration origin shares') %>%
  addPolygons( color = "#222", weight = 2, opacity = 1, fillOpacity = 0.7,
               fillColor = ~pal(w_migr_share),
               popup = ~popup,
               label = ~lapply(pc4_code, htmltools::htmlEscape),
               labelOptions = leaflet::labelOptions(direction = "top"),
               highlight = leaflet::highlightOptions(color = "#FFF", bringToFront = TRUE),
               data = spdf,
               group = 'Western migration origin only') %>%
  addPolygons( color = "#222", weight = 2, opacity = 1, fillOpacity = 0.7,
               fillColor = ~pal(nw_migr_share),
               popup = ~popup,
               label = ~lapply(pc4_code, htmltools::htmlEscape),
               labelOptions = leaflet::labelOptions(direction = "top"),
               highlight = leaflet::highlightOptions(color = "#FFF", bringToFront = TRUE),
               data = spdf,
               group = 'Non-Western migration origin only') %>%
  leaflet::addLegend(
    pal = pal, values = c(0,100), opacity = 0.95, title = "Migration origin share", position = "topleft") %>%
  addTiles() %>%
  addControl(title, position = "topright") %>%
  addControl(caption, position = "bottomleft") %>%
  addLayersControl(baseGroups = c("Total migration origin shares", "Western migration origin only", "Non-Western migration origin only"), options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup(c("Western migration origin only", "Non-Western migration origin only")) 

map_leaflet

# Save the map --------------------------------------------------

saveWidget(map_leaflet, file="./maps/migration_map_NL_2021_small.html")



