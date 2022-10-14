library(shiny)
library(leaflet)
library(leaflegend)
library(leaflet.minicharts)
library(rgdal)
library(nngeo)
library(sf)
library(raster)
library(colourpicker)
library(esquisse)
library(shinythemes)



source("www/create_var_map.R")
source("www/fonctions_map.R")
#load("www/rdata.RData")
#save(map1, file = "www/map1.Rds")
#save(map1, file = "www/map2.Rds")
js <- '
$(document).on("shiny:connected", function(){
  $("#map").css({
    width: window.innerWidth, 
    height: window.innerHeight
  });
  $(window).on("resize", function(e){
    if(e.target instanceof Window){
      $("#map").css({width: window.innerWidth, height: window.innerHeight});
    }
  });
})
'


ui=bootstrapPage(
  theme = shinytheme("flatly"),
  tags$head(
  tags$script(HTML(js)),
  tags$style(HTML("#controls {-ms-overflow-style: none;scrollbar-width: none;}")),
  tags$style(HTML("#controls {-ms-overflow-style: none;scrollbar-width: none;}")),
  tags$style(HTML("html,body {margin: 0; overflow: hidden;}"))),
  tags$style(HTML(".leaflet-touch .leaflet-control-layers-toggle {position:'bottomright'}")),
  leaflet::leafletOutput("map"),
  
  absolutePanel(id = "controls",
                class = "panel panel-default",
                fixed = FALSE,
                draggable = FALSE, top = 0, right = 0,
                width ="20%",
                height = "100%",
                style="opacity: 0.85;overflow-y: auto;",
  column(12,id="control_column",
           style = "padding:0;margin:0;padding:0px;",
         column(12,
                  style ="padding:15;margin:0;",
                  h4("Couches"),
                  uiOutput("initialize_map"),
                  uiOutput("voir_les_couches2"),
                  uiOutput("choix_de_la_couche"),
                  actionButton("dessiner_carte_id","Dessiner la carte",width="100%"),
                  actionButton("show_attribute_table_id","Voir la table d'attribut",width="100%"),
                  actionButton("edit_colnames_id","Editer le noms des champs",width="100%"),br(),
                  br()),
           
         column(12,
                style ="padding:15;margin:0;",
                
                  h4("Style"),
                  radioButtons("type_representation","Type de representation",c("Simple"="simple","Par classe"="classe","Gradient"="gradient", "Quantile"="quantile","Bin"="bin")),
                  uiOutput("type_representation_simple"),
                  uiOutput("var_num_ou_char_map"),
                  uiOutput("nb_quantiles_map"),
                  uiOutput("palette_carte"),
                  sliderInput("opacity", "Opactité:",min = 0, max = 1, value = 1),
                  uiOutput("epaisseur_ligne"),
                  uiOutput("icon_points"),
                  uiOutput("taille_points"),
                  uiOutput("taille_points2"),   
                  br()),
           
           
         column(12,
                style ="padding:15;margin:0;",
                  h4("Graphique-Etiquette-Légende"),
                  uiOutput("add_graphic"),
                  checkboxInput("add_graphic", "Ajouter un graphique", value = F, width = NULL),
                  uiOutput("var_add_graphic"),
                  uiOutput("var_add_graphic2"),
                  sliderInput("var_add_graphic3", "Modifier la taille :",min = 0, max = 1, value = 1),
                  palette_piker("palette_graphic_carte_id"),
                  checkboxInput("legende", "Afficher la légende", value = T, width = NULL),
                  textInput("titre_legende","Titre légende","Titre légende"),
                  uiOutput("etiquette"),
                  sliderInput("taille_etiquette","Taille des étiquettes",min=1,max=20,value=5),
                  uiOutput("graphique_selected_shape"),
                  checkboxInput("voir_la_selection", label="Voir la sélection", value = T),
                  actionButton("effacer_la_selection", label="Effacer la sélection", value = T,width="100%"),
                  #actionButton("create_map_var_id","Créer une variable",width="100%"),
                  actionButton("create_merged_layer_id","Generer une nouvelle couche par fusion",width="100%"),
                  br())
    )))
  
  
  
