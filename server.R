

server= function(input, output,session) {
  # MAP
  
  # observeEvent("",{
  #   #load("www/map1.Rds")
  #   load("www/map2.Rds")})
  n_initialize_map=reactiveVal(0)
  df_edit_colnames=reactiveVal()
  valeur_carte=reactiveValues(carte=list("Population départementale-INSEE 2018"=map1,"Population régionale-INSEE 2018"=map2),graphic_data=list(),style=NULL,minichart_=NULL,epaisseur_ligne=1,opacity=0.5,factor_pal=NULL,n_layer=2)
  
  mymap=reactiveVal(leaflet::leaflet())
  output$map=leaflet::renderLeaflet({mymap()})
  
  observeEvent("",{leaflet::leafletProxy("map")%>%initialize_proxy(valeur_carte$carte,session)})
  
  #- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # choix de la couche à modifier
  choix_de_la_couche_=eventReactive("",{selectizeInput("choix_de_la_couche","Editer la couche",choices=names(valeur_carte$carte))})
  output$choix_de_la_couche=renderUI(choix_de_la_couche_())
  
  
  
  #on stocke les données des graphiques pour chaque couche
  observeEvent(c(input$var_add_graphic,input$var_add_graphic2,input$palette_graphic_carte_id),{
    valeur_carte$graphic_data[[paste0("variables_",input$choix_de_la_couche)]]=input$var_add_graphic
    valeur_carte$graphic_data[[paste0("taille_graphique_",input$choix_de_la_couche)]]=input$var_add_graphic2
    valeur_carte$graphic_data[[paste0("palete_graphique_",input$choix_de_la_couche)]]=input$palette_graphic_carte_id
    })
    
    
  observeEvent(input$choix_de_la_couche,{
    for(i in names(valeur_carte$carte)){
      if(i==input$choix_de_la_couche){
    updateSelectizeInput(session,"var_add_graphic",selected=valeur_carte$graphic_data[[paste0("variables_",input$choix_de_la_couche)]])
    updateSelectizeInput(session,"var_add_graphic2",selected=valeur_carte$graphic_data[[paste0("taille_graphique_",input$choix_de_la_couche)]])}}
    
    })
  #pour les spatials points ::: taille des points
  taille_points_=eventReactive(input$choix_de_la_couche,{
    if(class(valeur_carte$carte[[input$choix_de_la_couche]])[1]=="SpatialPointsDataFrame"){
      selectizeInput("var_taille_points","Taille des points fonction de :",choices=c("uniforme",colnames(valeur_carte$carte[[input$choix_de_la_couche]]@data)[sapply(valeur_carte$carte[[input$choix_de_la_couche]]@data,class)=="numeric"]),multiple =F)}
  })
  output$taille_points=renderUI(taille_points_())
  
  taille_points2_=eventReactive(input$choix_de_la_couche,{
    if(class(valeur_carte$carte[[input$choix_de_la_couche]])[1]=="SpatialPointsDataFrame"){
      sliderInput("taille_points2", "Moduler la taille des points:",min = 10, max = 50, value = 20)}})
  output$taille_points2=renderUI(taille_points2_())
  
  #pour choisir l'icones des points
  icon_points_=eventReactive(input$choix_de_la_couche,{
    if(class(valeur_carte$carte[[input$choix_de_la_couche]])[1]=="SpatialPointsDataFrame"){
      shinyWidgets::pickerInput(
        inputId = "icon_points",
        label = "Icône",
        choices = c("Rectangle"="rect",
                    "Disque"="circle",
                    "Triangle"="triangle",
                    "Plus"="plus",
                    "croix"="cross",
                    "Diamant"="diamond",
                    "Etoile"="star",
                    "Stadium"="stadium",
                    "Ligne"="line",
                    "Polygone"="polygon"))}})
  
  output$icon_points=renderUI(icon_points_())
  #observeEvent(input$icon_points,{print(input$icon_points)})
  
  # epaisseur des lignes des shapes
  epaisseur_ligne_=eventReactive(input$choix_de_la_couche,{if(class(valeur_carte$carte[[input$choix_de_la_couche]])[1]=="SpatialPolygonsDataFrame"){sliderInput("opacity_line", "Epaisseur du tracé:",min = 1, max = 10, value = 1)}})
  output$epaisseur_ligne=renderUI(epaisseur_ligne_())
  
   #etiquettes sur les shapes
  etiquette_=eventReactive(input$choix_de_la_couche,{
    selectizeInput("etiquette","Etiquette",choices=c("aucune",colnames(valeur_carte$carte[[input$choix_de_la_couche]]@data)))})
  output$etiquette=renderUI(etiquette_())
  
  var_add_graphic_=eventReactive(input$choix_de_la_couche,{
    selectizeInput("var_add_graphic","Variables :",choices=colnames(valeur_carte$carte[[input$choix_de_la_couche]]@data)[sapply(valeur_carte$carte[[input$choix_de_la_couche]]@data,class)=="numeric"],multiple = TRUE)})
  output$var_add_graphic=renderUI(var_add_graphic_())
  
  
  var_add_graphic2_=eventReactive(input$choix_de_la_couche,{
    selectizeInput("var_add_graphic2","Taille en fonction de :",choices=c("uniforme",colnames(valeur_carte$carte[[input$choix_de_la_couche]]@data)[sapply(valeur_carte$carte[[input$choix_de_la_couche]]@data,class)=="numeric"]),multiple =F)})
  output$var_add_graphic2=renderUI(var_add_graphic2_())
 
  type_representation_simple_=eventReactive(input$type_representation,{
    if(input$type_representation=="simple"){color_piker("type_representation_simple_id")}
  })
  output$type_representation_simple=renderUI(type_representation_simple_())
  
  nb_quantiles_map_=eventReactive(input$type_representation,{if(input$type_representation %in%c("quantile","bin")){sliderInput(inputId = "nb_quantiles_map_id",label="Nombre de quantiles ou de classes bin",min=2,max=15,step=1,value=2)}})
  output$nb_quantiles_map=renderUI(nb_quantiles_map_())
  
  var_num_ou_char_map_=eventReactive(input$type_representation,{
    if(input$type_representation!="simple"){selectInput("var_num_ou_char_map_id","Variable :",choices=colnames(valeur_carte$carte[[input$choix_de_la_couche]]@data)[sapply(valeur_carte$carte[[input$choix_de_la_couche]]@data,class)=="numeric"])}})
  output$var_num_ou_char_map=renderUI(var_num_ou_char_map_())
  observeEvent(input$type_representation,{if(input$type_representation=="classe"){updateSelectInput(session, "var_num_ou_char_map_id", label ="Variable :", choices=colnames(valeur_carte$carte[[input$choix_de_la_couche]]@data)[sapply(valeur_carte$carte[[input$choix_de_la_couche]]@data,class)!="numeric"])}})
  
  
  palette_carte_=eventReactive(input$type_representation,{
    if(input$type_representation %in%c("gradient","quantile","bin")|input$type_representation=="classe"){palette_piker("palette_carte_id")}})
  output$palette_carte=renderUI(palette_carte_())

  observeEvent(input$show_attribute_table_id,{
    showModal(modalDialog(size="l",title=input$choix_de_la_couche,DT::renderDataTable(valeur_carte$carte[[input$choix_de_la_couche]]@data,rownames=F,options=list(scrollX = TRUE)),rownames= FALSE,footer = NULL,easyClose = T))})
  
  
  #éditer le nom des champs d'une table d'attribut
  observeEvent(input$edit_colnames_id,{
    vec=colnames(valeur_carte$carte[[input$choix_de_la_couche]]@data)
    df=data.frame(matrix("",ncol=2,nrow=length(vec)))
    df[,1]=vec
    colnames(df)=c("champs","nouveau nom")
    df_edit_colnames(df)
    
  })
  output$df_edit_colnames=DT::renderDataTable(df_edit_colnames(),rownames=F,editable = list(target = "cell", disable =list(columns = c(0))),options=list(scrollX = TRUE,dom='lt'))
  
  valider_edit_colnames_=eventReactive(input$edit_colnames_id,{actionButton("valider_edit_colnames_id","Valider",width="100%")})
  output$valider_edit_colnames=renderUI(valider_edit_colnames_())
  
  
  observeEvent(input$edit_colnames_id,{
    showModal(modalDialog(size="l",DT::dataTableOutput("df_edit_colnames"),uiOutput("valider_edit_colnames"),footer = NULL,easyClose = T))
  })
  
  observeEvent(input$df_edit_colnames_cell_edit,{
    df=df_edit_colnames()
    df[input$df_edit_colnames_cell_edit$row,input$df_edit_colnames_cell_edit$col+1]=input$df_edit_colnames_cell_edit$value
    df_edit_colnames(df)
  })
  
  
  
  observeEvent(input$valider_edit_colnames_id,{
    vec=df_edit_colnames()[,2]
    index=which(vec=="")
    vec[index]= df_edit_colnames()[,1][index]
    vec=trimws(vec)
    
    # quatre variables de la table d'attribut dont il est interdit de changer le nom
    index=which(df_edit_colnames()[,1]=="lat")
    vec[index]="lat"
    index=which(df_edit_colnames()[,1]=="long")
    vec[index]="long"
    index=which(df_edit_colnames()[,1]=="selected")
    vec[index]="selected"
    index=which(df_edit_colnames()[,1]=="id")
    vec[index]="id"
    #_ _ _ _ _ _ _
    a=table(vec)
    index=which(a>1)
    for(i in index){
      index2=which(vec==names(a)[i])
      vec[index2]=paste0(names(a)[i],1:a[i])}
    
    colnames(valeur_carte$carte[[input$choix_de_la_couche]]@data)=vec
    #mise à jour des widgets quand on change le nom des champs de la table d'attributs
    updateSelectizeInput(inputId ="var_add_graphic",label="Variables :",choices=colnames(valeur_carte$carte[[input$choix_de_la_couche]]@data)[sapply(valeur_carte$carte[[input$choix_de_la_couche]]@data,class)=="numeric"] )
    updateSelectizeInput(inputId ="var_add_graphic2",label="Taille en fonction de :",choices=c("uniforme",colnames(valeur_carte$carte[[input$choix_de_la_couche]]@data)[sapply(valeur_carte$carte[[input$choix_de_la_couche]]@data,class)=="numeric"]) )
    updateSelectInput(inputId ="etiquette",label="Etiquette",choices=c("aucune",colnames(valeur_carte$carte[[input$choix_de_la_couche]]@data)))
    if(input$type_representation!="simple"){selectInput(inputId = "var_num_ou_char_map_id",label="Variable :",choices=colnames(valeur_carte$carte[[input$choix_de_la_couche]]@data)[sapply(valeur_carte$carte[[input$choix_de_la_couche]]@data,class)=="numeric"])}
    else if(input$type_representation=="classe"){updateSelectInput(session, "var_num_ou_char_map_id", label ="Variable :", choices=colnames(valeur_carte$carte[[input$choix_de_la_couche]]@data)[sapply(valeur_carte$carte[[input$choix_de_la_couche]]@data,class)!="numeric"])}
  })
  
  #_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
  
  
  
  
  
  observeEvent("",{
    leaflet::leafletProxy("map")%>% leaflet::clearGroup(group = "Population départementale-INSEE 2018")%>%
      create_map_proxy(
        carte=valeur_carte$carte[["Population départementale-INSEE 2018"]],
        nom_de_la_couche="Population départementale-INSEE 2018",
        type_representation="bin",
        type_representation_2="Population en 2018 (princ)",
        opacity=1,
        opacity_line=1,
        etiquette="aucune",
        legende=T,
        titre_legende="Population départementale INSEE-2018",
        palette="plasma",
        taille_etiquette=5,
        taille_points=5,
        taille_points2=5,
        nombre_de_classes=4
      )})
  
  observeEvent("",{
    leaflet::leafletProxy("map")%>% leaflet::clearGroup(group = "Population régionale-INSEE 2018")%>%
      create_map_proxy(
        carte=valeur_carte$carte[["Population régionale-INSEE 2018"]],
        nom_de_la_couche="Population régionale-INSEE 2018",
        type_representation="classe",
        type_representation_2="reg_name",
        opacity=1,
        opacity_line=1,
        etiquette="aucune",
        legende=T,
        titre_legende="Les régions métropolitaines",
        palette="viridis",
        taille_etiquette=5,
        taille_points=5,
        taille_points2=5,
        nombre_de_classes=2
      )})
  
  
  
  observeEvent(input$dessiner_carte_id,{
    
    a=map_operation_validation(type_representation=input$type_representation,var_num_ou_char_map_id=input$var_num_ou_char_map_id,data=valeur_carte$carte[[input$choix_de_la_couche]]@data,nb_quantiles=input$nb_quantiles_map_id)
    if(a=="ok"){
      leaflet::leafletProxy("map",data=valeur_carte$carte[[input$choix_de_la_couche]]@data)%>% leaflet::clearGroup(group = input$choix_de_la_couche)
      if(class(valeur_carte$carte[[input$choix_de_la_couche]])[1]=="SpatialPolygonsDataFrame"){
        leaflet::leafletProxy("map")%>%create_map_proxy(
          carte=valeur_carte$carte[[input$choix_de_la_couche]],
          nom_de_la_couche=input$choix_de_la_couche,
          type_representation=input$type_representation,
          type_representation_2=if(req(input$type_representation)=="simple"){req(input$type_representation_simple_id)} else{req(input$var_num_ou_char_map_id)},
          opacity=input$opacity,
          opacity_line=input$opacity_line,
          etiquette=input$etiquette,
          legende=input$legende,
          titre_legende=input$titre_legende,
          palette=input$palette_carte_id,
          taille_etiquette=input$taille_etiquette,
          taille_points=req(input$var_taille_points),
          taille_points2=req(input$taille_points2),
          nombre_de_classes=input$nb_quantiles_map_id
        )}
      
      
      else{
        leaflet::leafletProxy("map")%>% leaflet::removeControl(layerId=paste0(input$choix_de_la_couche,"-",1:nrow(valeur_carte$carte[[input$choix_de_la_couche]]@data)))
        leaflet::leafletProxy("map")%>% leaflet::removeControl(layerId=paste0(input$choix_de_la_couche,"--",1:nrow(valeur_carte$carte[[input$choix_de_la_couche]]@data)))
        leaflet::leafletProxy("map")%>%create_map_proxy2(
          carte=valeur_carte$carte[[input$choix_de_la_couche]],
          nom_de_la_couche=input$choix_de_la_couche,
          type_representation=input$type_representation,
          type_representation_2=if(req(input$type_representation)=="simple"){req(input$type_representation_simple_id)} else{req(input$var_num_ou_char_map_id)},
          opacity=input$opacity,
          etiquette=input$etiquette,
          legende=input$legende,
          titre_legende=input$titre_legende,
          palette=input$palette_carte_id,
          taille_etiquette=input$taille_etiquette,
          taille_points=req(input$var_taille_points),
          taille_points2=req(input$taille_points2),
          symbol=input$icon_points,
          nombre_de_classes=if(req(input$type_representation)%in% c("quantile","bin")){req(input$nb_quantiles_map_id)} else{2})}
    }
    
    else{showModal(modalDialog(title="Erreur","La variable que vous avez choisi ne convient pas au type de représentation souhaité ou le nombre de quantiles/classes bin est trop grand. Vous pouvez : diminuez le nombre de quantiles,changez de variable ou bien de type de représentation",footer = NULL,easyClose = T))}
 })
  
  
  
 
  
  observeEvent(c(input$add_graphic,input$var_add_graphic,req(input$var_add_graphic2),req(input$var_add_graphic3),input$palette_graphic_carte_id),{
    
    
    for(i in names(valeur_carte$carte)){
      if(input$choix_de_la_couche==i){

      if(paste0("palete_graphique_",i) %in% names(valeur_carte$graphic_data)){
      if(valeur_carte$graphic_data[[paste0("palete_graphique_",i)]] %in% c("viridis","magma","inferno","plasma","cividis")){palette_graphic_=viridis::viridis_pal(option = valeur_carte$graphic_data[[paste0("palete_graphique_",i)]])(15)}
      else if (valeur_carte$graphic_data[[paste0("palete_graphique_",i)]] %in% c("Dark2","Greys","Greens","GnBu","BuPu","BuGn","Blues" ,"Oranges","Reds","Paired" ,"Set1")){palette_graphic_=scales::brewer_pal(palette=valeur_carte$graphic_data[[paste0("palete_graphique_",i)]])(10) }}
 
      if(input$add_graphic & !is.null(input$var_add_graphic) & input$var_add_graphic2=="uniforme"){
      leaflet::leafletProxy("map")%>%clearMinicharts()%>%leaflet.minicharts::addMinicharts(
        valeur_carte$carte[[i]]@data$long, valeur_carte$carte[[i]]@data$lat,
        type = "pie",
        chartdata = valeur_carte$carte[[i]]@data[,input$var_add_graphic ],
        legendPosition="bottomleft",
        layerId = paste0(i,"mini_chart",1:nrow(valeur_carte$carte[[i]]@data)),
        colorPalette=palette_graphic_,
        width = input$var_add_graphic3 *50
      )}
    
    else if(input$add_graphic & !is.null(input$var_add_graphic) & input$var_add_graphic2!="uniforme"){
      leaflet::leafletProxy("map")%>%clearMinicharts()%>%leaflet.minicharts::addMinicharts(
        valeur_carte$carte[[i]]@data$long, valeur_carte$carte[[i]]@data$lat,
        type = "pie",
        chartdata = valeur_carte$carte[[i]]@data[,input$var_add_graphic ],
        legendPosition="bottomleft",
        layerId = paste0(i,"mini_chart",1:nrow(valeur_carte$carte[[i]]@data)),
        colorPalette=palette_graphic_,
        width = input$var_add_graphic3 *100* (abs(valeur_carte$carte[[i]]@data[,input$var_add_graphic2])/max(valeur_carte$carte[[i]]@data[,input$var_add_graphic2]))
      )}}}
    
    
    
    #if (! input$add_graphic){leaflet::leafletProxy("map")%>%clearMinicharts()}
    
  })
 
  #_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _sélection des couches (quand les polygones s'affichent en jaune) _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
  
  # mise a jour des polygones selectionnés de la première couche de la carte en fonction de l'endroit on on click sur la carte
  observeEvent(req(input$map_shape_click),{
    if(input$voir_la_selection){
      if( class(valeur_carte$carte[[input$choix_de_la_couche[1]]])[1]=="SpatialPolygonsDataFrame"){
        p= input$map_shape_click
        if(p$group=="selected_shape" || p$group==input$choix_de_la_couche[1]){valeur_carte$carte[[input$choix_de_la_couche[1]]]@data$selected[p$id]=valeur_carte$carte[[input$choix_de_la_couche[1]]]@data$selected[p$id]+1}
          leaflet::leafletProxy("map")%>%leaflet::clearGroup("selected_point")%>%
          leaflet::clearGroup("selected_shape")%>%
          leaflet::addPolygons(data=subset(valeur_carte$carte[[input$choix_de_la_couche[1]]],valeur_carte$carte[[input$choix_de_la_couche[1]]]@data$selected%%2!=0),
                                                                                                                                            color = "#eded1f",
                                                                                                                                            weight = 3,
                                                                                                                                            smoothFactor = 0.5,
                                                                                                                                            opacity =1 ,
                                                                                                                                             fillOpacity = 0,
                                                                                                                                            fillColor = "#FFFFFF",
                                                                                                                                            layerId = ~id,
                                                                                                                                            group="selected_shape",
                                                                                                                                            highlightOptions = leaflet::highlightOptions(color = "#eded1f", weight = 4,bringToFront = T))}}
  })
  
  #affichier les polygones selectionnés en fonction de la première couche sur la carte
  observeEvent(input$choix_de_la_couche,{
    if(input$voir_la_selection){
      if( class(valeur_carte$carte[[input$choix_de_la_couche[1]]])[1]=="SpatialPolygonsDataFrame"){
        leaflet::leafletProxy("map")%>%leaflet::clearGroup("selected_point")%>%leaflet::clearGroup(group = "selected_shape")%>%leaflet::addPolygons(data=subset(valeur_carte$carte[[input$choix_de_la_couche[1]]],valeur_carte$carte[[input$choix_de_la_couche[1]]]@data$selected%%2!=0),
                                                                                                                                                    color = "#eded1f", weight = 2, smoothFactor = 0.5,opacity =1 ,
                                                                                                                                                    fillOpacity = 0,
                                                                                                                                                    fillColor = "#FFFFFF",
                                                                                                                                                    layerId = ~id,
                                                                                                                                                    group="selected_shape",
                                                                                                                                                    #options = pathOptioclickable = T),
                                                                                                                                                    highlightOptions = leaflet::highlightOptions(color = "white", weight = 2,bringToFront = F))}}
    
  })
  
  
  # mise a jour des points selectionnés de la première couche de la carte en fonction de l'endroit on on click sur la carte
  observeEvent(req(input$map_marker_click),{
    if(input$voir_la_selection){
      if( class(valeur_carte$carte[[input$choix_de_la_couche[1]]])[1]=="SpatialPointsDataFrame"){
        p= input$map_marker_click
        if(p$group=="selected_point" || p$group==input$choix_de_la_couche[1]){valeur_carte$carte[[input$choix_de_la_couche[1]]]@data$selected[p$id]=valeur_carte$carte[[input$choix_de_la_couche[1]]]@data$selected[p$id]+1}
        data_=subset(valeur_carte$carte[[input$choix_de_la_couche[1]]],valeur_carte$carte[[input$choix_de_la_couche[1]]]@data$selected%%2!=0)
        sizes=sizeNumeric(valeur_carte$carte[[input$choix_de_la_couche[1]]]@data[[input$var_taille_points]], baseSize = input$taille_points2)
        sizes=subset(sizes,valeur_carte$carte[[input$choix_de_la_couche[1]]]@data$selected%%2!=0)
        
        if(length(data_)>0){
          pal_=leaflet::colorFactor("#eded1f",rep(input$titre_legende,nrow(data_)))
          if(input$var_taille_points=="uniforme"){sizes=sizeNumeric(rep(1,nrow(data_)), baseSize = input$taille_points2)}
          else{
            sizes=sizeNumeric(valeur_carte$carte[[input$choix_de_la_couche[1]]]@data[[input$var_taille_points]], baseSize = input$taille_points2)
            sizes=subset(sizes,valeur_carte$carte[[input$choix_de_la_couche[1]]]@data$selected%%2!=0)}
          symbols= Map(makeSymbol,shape = input$icon_points,color =pal_(rep(input$titre_legende,nrow(data_))),width = sizes,height = sizes,opacity = 0.5)
          
          leaflet::leafletProxy("map")%>%leaflet::clearGroup(group = "selected_shape")%>%leaflet::clearGroup("selected_point") %>%addMarkers(data = data_,
                                                                                                                                             icon = icons(iconUrl = symbols),
                                                                                                                                             lat = ~data_@data$lat,
                                                                                                                                             lng = ~data_@data$long,
                                                                                                                                             layerId = ~data_$id,
                                                                                                                                             group="selected_point",
                                                                                                                                             #clusterOptions = markerClusterOptio spiderLegPolylineOptions = list(weight = 1.5, color = "blue", opacity = 0.5))
                                                                                                                                             options = pathOptions())
        }}}
    
    
    
    
    
  })
  #afficher les points selectionnés en fonction de la première couche sur la carte
  observeEvent(input$choix_de_la_couche,{
    if(input$voir_la_selection){
      if( class(valeur_carte$carte[[input$choix_de_la_couche[1]]])[1]=="SpatialPointsDataFrame"){
        data_=subset(valeur_carte$carte[[input$choix_de_la_couche[1]]],valeur_carte$carte[[input$choix_de_la_couche[1]]]@data$selected%%2!=0)
        
        
        if(length(data_)>0){
          pal_=leaflet::colorFactor("#eded1f",rep(input$titre_legende,nrow(data_)))
          if(input$var_taille_points=="uniforme"){sizes=sizeNumeric(rep(1,nrow(data_)), baseSize = input$taille_points2)}
          else{
            sizes=sizeNumeric(valeur_carte$carte[[input$choix_de_la_couche[1]]]@data[[input$var_taille_points]], baseSize = input$taille_points2)
            sizes=subset(sizes,valeur_carte$carte[[input$choix_de_la_couche[1]]]@data$selected%%2!=0)}
          symbols= Map(makeSymbol,shape = input$icon_points,color =pal_(rep(input$titre_legende,nrow(data_))),width = sizes,height = sizes,opacity = 0.5)
          leaflet::leafletProxy("map")%>%leaflet::clearGroup(group = "selected_shape")%>%leaflet::clearGroup("selected_point")%>%  addMarkers(data = data_,
                                                                                                                                              icon = icons(iconUrl = symbols),
                                                                                                                                              lat = ~data_@data$lat,
                                                                                                                                              lng = ~data_@data$long,
                                                                                                                                              layerId = ~data_$id,
                                                                                                                                              group="selected_point",
                                                                                                                                              #clusterOptions = markerClusterOptio spiderLegPolylineOptions = list(weight = 1.5, color = "blue", opacity = 0.5))
                                                                                                                                              options = pathOptions())}}}
  })
  
  
  
  
  #afficher la couche selectionnée
  observeEvent(input$voir_la_selection,{
    if(!input$voir_la_selection){leaflet::leafletProxy("map")%>%leaflet::hideGroup("selected_point")%>%leaflet::hideGroup("selected_shape")}
    else(leaflet::leafletProxy("map")%>%leaflet::showGroup("selected_point")%>%leaflet::showGroup("selected_shape"))
  })
  
  # effacer la couche selectionnées
  observeEvent(input$effacer_la_selection,{
    valeur_carte$carte[[input$choix_de_la_couche[1]]]@data$selected=0
    leaflet::leafletProxy("map")%>%leaflet::clearGroup("selected_point")%>%leaflet::clearGroup("selected_shape")})
  
  #_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
  # pour les graphiques qui s'affichent lorsqu'on selectionne les différents polygones du shapefiles
  # var_selected_shape_=eventReactive(input$choix_de_la_couche,{selectizeInput("var_selected_shape_id","Variable :",multiple=T,choices=colnames(valeur_carte$carte[[input$choix_de_la_couche]]@data)[sapply(valeur_carte$carte[[input$choix_de_la_couche]]@data,class)=="numeric"])})
  # output$var_selected_shape=renderUI(var_selected_shape_())
  # 
  # label_selected_shape_=eventReactive(input$choix_de_la_couche,{selectizeInput("label_selected_shape_id","Label :",multiple=F,choices=colnames(valeur_carte$carte[[input$choix_de_la_couche]]@data)[sapply(valeur_carte$carte[[input$choix_de_la_couche]]@data,class)!="numeric"])})
  # output$label_selected_shape=renderUI(label_selected_shape_())
  # 
  # titre_graphique_selected_shape_=eventReactive(input$choix_de_la_couche,{textInput("titre_graphique_selected_shape_id", "Titre du graphique", value = "MON JOLI GRAPHIQUE",width="100%",placeholder = NULL)})
  # output$titre_graphique_selected_shape=renderUI(titre_graphique_selected_shape_())
  # 
  # titre_axe_x_titre_graphique_selected_shape_=eventReactive("",{textInput("titre_axe_x_titre_graphique_selected_shape_id", "Titre de l'Axe x", value ="Axe y", width="100%", placeholder = NULL)})
  # output$titre_axe_x_titre_graphique_selected_shape=renderUI(titre_axe_x_titre_graphique_selected_shape_())
  # 
  # titre_axe_y_titre_graphique_selected_shape_=eventReactive("",{textInput("titre_axe_y_titre_graphique_selected_shape_id", "Titre de l'Axe y", value ="Axe y", width="100%", placeholder = NULL)})
  # output$titre_axe_y_titre_graphique_selected_shape=renderUI(titre_axe_y_titre_graphique_selected_shape_())
  # 
  
  # camembert
  # graphique_selected_shape_1_=eventReactive(c(input$var_selected_shape_id,input$map_shape_click,input$titre_graphique_selected_shape_id),{graphique_selected_shape_1(data=subset(valeur_carte$carte[[input$choix_de_la_couche]]@data,valeur_carte$carte[[input$choix_de_la_couche]]@data$selected%%2!=0),
  #                                                                                                                                                                    selected_var=input$var_selected_shape_id,
  #                                                                                                                                                                    titre = input$titre_graphique_selected_shape_id,
  # ))
  # output$graphique_selected_shape_1=plotly::renderPlotly(
  #   if(length(input$var_selected_shape_id)>1 & length(valeur_carte$carte[[input$choix_de_la_couche]]@data$selected%%2!=0)>0 ){ # s'il y a au moins deux variables selectionnées et que des polygones sont selectionnés
  #     graphique_selected_shape_1_()}
  #   
  # )
  
  # diagramme barres empilées
  # graphique_selected_shape_2_=eventReactive(
  #   c(input$var_selected_shape_id,
  #     input$map_shape_click,
  #     input$titre_graphique_selected_shape_id,
  #     input$label_selected_shape_id,
  #     input$titre_axe_x_titre_graphique_selected_shape_id,
  #     input$titre_axe_y_titre_graphique_selected_shape_id
  #   ),
  #   
  #   {graphique_selected_shape_2(
  #     data=subset(valeur_carte$carte[[input$choix_de_la_couche]]@data,valeur_carte$carte[[input$choix_de_la_couche]]@data$selected%%2!=0),
  #     input$var_selected_shape_id,
  #     label=input$label_selected_shape_id,
  #     titre_x=input$titre_axe_x_titre_graphique_selected_shape_id,
  #     titre_y=input$titre_axe_y_titre_graphique_selected_shape_id
  #   )})
  # output$graphique_selected_shape_2=plotly::renderPlotly(
  #   if(length(input$var_selected_shape_id)>0 & length(valeur_carte$carte[[input$choix_de_la_couche]]@data$selected%%2!=0)>0 ){ # s'il y a au moins une variable selectionnée et que des polygones sont selectionnés
  #     graphique_selected_shape_2_()})
  # 
  # 
  
  
  
   
  
  
  #- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  #LIEN TABLEAU CROISE ET MAP
  
  # tableau_resume_()
  # input$valider_analyse_4_id
  # names(valeur_carte$carte)
  
  # join_data_to_this_map_=eventReactive(input$valider_analyse_4_id,{selectizeInput(inputId ="join_data_to_this_map_id",label="Joindre les données à cette couche", choices=names(valeur_carte$carte),multiple=F)})
  # output$join_data_to_this_map=renderUI( if(nmap() & input$radio_analyse=="TABLEAU CROISE" ){if(type_tableau()%in% c("moyenne","proportion") & ngroup()<3 |ngroup()==1 |ngroup()==2 & nnumericval()==1 ){join_data_to_this_map_()}})
  # 
  # ID_data_=eventReactive(input$valider_analyse_4_id,{selectizeInput(inputId ="ID_data_id",label="ID de la table", choices=input$var_group_3_id,multiple=F)})
  # output$ID_data=renderUI(if(nmap() & input$radio_analyse=="TABLEAU CROISE" ){if(type_tableau() %in% c("moyenne","proportion") & ngroup()<3 | ngroup()==1 |ngroup()==2 & nnumericval()==1 ){ID_data_()}})
  # 
  # ID_map_=eventReactive(input$join_data_to_this_map_id,{selectizeInput(inputId ="ID_map_id",label="ID de la carte", choices=colnames(valeur_carte$carte[[input$join_data_to_this_map_id]]@data)  ,multiple=F )      })
  # output$ID_map=renderUI(if(nmap() & input$radio_analyse=="TABLEAU CROISE" ){if(type_tableau()%in% c("moyenne","proportion") & ngroup()<3 |ngroup()==1|ngroup()==2 & nnumericval()==1 ){ID_map_()}})
  # 
  # go_join_data_to_this_map_=eventReactive(input$valider_analyse_4_id,{ actionButton(inputId = "go_join_data_to_this_map_id",label="Joindre les données",width="100%")})
  # output$go_join_data_to_this_map=renderUI(if(nmap() & input$radio_analyse=="TABLEAU CROISE" ){if(type_tableau()%in% c("moyenne","proportion") & ngroup()<3 |ngroup()==1|ngroup()==2 & nnumericval()==1 ){go_join_data_to_this_map_()}})
  # 
  # observeEvent(input$go_join_data_to_this_map_id,{
  #   
  #   progress= shiny::Progress$new()
  #   on.exit(progress$close())
  #   progress$set(message = "Jointure des données", value = 0)
  #   progress$inc(2/10, detail = "Mise en forme des données" )
  #   
  #   
  #   print(head(valeur_carte$carte[[input$join_data_to_this_map_id]]@data))
  #   vec=input$ID_data_id
  #   names(vec)=input$ID_map_id
  #   
  #   if(type_tableau()=="general"){
  #     if(ngroup()==1){
  #       valeur_carte$carte[[input$join_data_to_this_map_id]]@data=dplyr::left_join(valeur_carte$carte[[input$join_data_to_this_map_id]]@data,tableau_resume_(),by=vec)
  #       print(head(valeur_carte$carte[[input$join_data_to_this_map_id]]@data))
  #       progress$inc(5/10, detail = "Mise en forme des données" )
  #     }
  #     
  #     else if(ngroup()==2 & nnumericval()==1){
  #       df=tableau_resume_()%>% tidyr::pivot_wider(names_from =setdiff(input$var_group_3_id,input$ID_data_id)[1], values_from =input$num_var_3_id, values_fill = 0,names_sep = "_")
  #       colnames(df)[2:ncol(df)]=paste0(setdiff(input$var_group_3_id,input$ID_data_id)[1],"_",colnames(df)[2:ncol(df)])
  #       print(df)
  #       valeur_carte$carte[[input$join_data_to_this_map_id]]@data=dplyr::left_join(valeur_carte$carte[[input$join_data_to_this_map_id]]@data,df,by=vec)
  #       print(head(valeur_carte$carte[[input$join_data_to_this_map_id]]@data))
  #       progress$inc(5/10, detail = "Mise en forme des données" )
  #     }}
  #   
  #   else if (type_tableau()=="moyenne" & ngroup()<3){
  #     if(ngroup()==1){
  #       valeur_carte$carte[[input$join_data_to_this_map_id]]@data=dplyr::left_join(valeur_carte$carte[[input$join_data_to_this_map_id]]@data,tableau_resume_()[,c(1,3)],by=vec)
  #       progress$inc(5/10, detail = "Mise en forme des données" )}
  #     else if(ngroup()==2){
  #       noms=colnames(tableau_resume_())
  #       df=tableau_resume_()[,c(1,2,4)]%>% tidyr::pivot_wider(names_from =noms[2], values_from =noms[4], values_fill = 0,names_sep = "_")
  #       colnames(df)[2:ncol(df)]=paste0(noms[4],"_",noms[2],"_",colnames(df)[2:ncol(df)])
  #       print(df)
  #       valeur_carte$carte[[input$join_data_to_this_map_id]]@data=dplyr::left_join(valeur_carte$carte[[input$join_data_to_this_map_id]]@data,df,by=vec)
  #       progress$inc(5/10, detail = "Mise en forme des données")
  #     }
  #   }
  #   
  #   
  #   else if (type_tableau()=="proportion" & ngroup()<3){
  #     if(ngroup()==1){
  #       valeur_carte$carte[[input$join_data_to_this_map_id]]@data=dplyr::left_join(valeur_carte$carte[[input$join_data_to_this_map_id]]@data,tableau_resume_()[,c(1,3)],by=vec)
  #       progress$inc(5/10, detail = "Mise en forme des données" )}
  #     else if (ngroup()==2){
  #       noms=colnames(tableau_resume_())
  #       df=tableau_resume_()[,c(1,2,4)]%>% tidyr::pivot_wider(names_from =noms[2], values_from =noms[4], values_fill = 0,names_sep = "_")
  #       colnames(df)[2:ncol(df)]=paste0(noms[4],"_",noms[1],"_",noms[2],"_",colnames(df)[2:ncol(df)])
  #       print(df)
  #       valeur_carte$carte[[input$join_data_to_this_map_id]]@data=dplyr::left_join(valeur_carte$carte[[input$join_data_to_this_map_id]]@data,df,by=vec)
  #       progress$inc(5/10, detail = "Mise en forme des données")
  #     }
  #   }
  #   progress$inc(10/10, detail = "Jointure terminée")
  # })
  
  #- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  #CREER UNE NOUVELLE COUCHE GEOGRAPHIQUE PAR FUSION
  
  create_layer_from_layer_=eventReactive("",{
    vec=sapply(valeur_carte$carte,class)
    index=which(vec=="SpatialPolygonsDataFrame")
    selectizeInput(inputId ="create_layer_from_layer_id",label="fusionner les polygones à partir de la couche ",choices=names(valeur_carte$carte)[index],multiple=F )})
  output$create_layer_from_layer=renderUI(create_layer_from_layer_())
  
  merge_polygon_from_var_=eventReactive(input$create_layer_from_layer_id,{selectizeInput(inputId ="merge_polygon_from_var_id",label="fusionner les polygones à partir de la variable",choices=colnames(valeur_carte$carte[[input$create_layer_from_layer_id]]@data),multiple=F )})
  output$merge_polygon_from_var=renderUI(merge_polygon_from_var_())
  
  valider_merged_layer_=eventReactive("",{actionButton(inputId = "valider_merged_layer_id",label="Valider")})
  output$valider_merged_layer=renderUI(valider_merged_layer_())
  
  name_of_merged_layer_=eventReactive("",{ textInput(inputId = "name_of_merged_layer_id",label="Nom de la couche",width="100%")})
  output$name_of_merged_layer=renderUI(name_of_merged_layer_())
  
  
  merge_layer_with_this_function_=eventReactive("",{ radioButtons(inputId = "merge_layer_with_this_function_id",label="Aggreger les variables numériques par la fonction",choices=c("somme"=1,"max"=2,"min"=3,"moyenne"=4,"moyenne pondérée par la surface"=5)
  )})
  output$merge_layer_with_this_function=renderUI(merge_layer_with_this_function_())
  
  
  
  
  observeEvent(input$create_merged_layer_id,{
    showModal(modalDialog(size="l",
                          uiOutput("create_layer_from_layer"),
                          uiOutput("merge_polygon_from_var"),
                          uiOutput("name_of_merged_layer"),
                          uiOutput("merge_layer_with_this_function"),
                          uiOutput("valider_merged_layer"),
                          footer = NULL,
                          easyClose = T))
  })
  
  #création de la couche par fusion
  observeEvent(input$valider_merged_layer_id,{
    valeur_carte$carte[[input$name_of_merged_layer_id]]=dissolve_shp(valeur_carte$carte[[input$create_layer_from_layer_id]],input$merge_polygon_from_var_id,fonction=input$merge_layer_with_this_function_id)
    print(valeur_carte$carte[[input$name_of_merged_layer_id]]@data)
    updateSelectizeInput(inputId = "choix_de_la_couche",label="Editer la couche",choices=names(valeur_carte$carte))
    leaflet::leafletProxy("map")%>%leaflet::addPolygons(data=valeur_carte$carte[[input$name_of_merged_layer_id]],
                                                        color = "#000000", weight = 2, smoothFactor = 0.5,opacity =1 ,
                                                        fillOpacity = 0,
                                                        fillColor = "#FFFFFF",
                                                        layerId = ~valeur_carte$carte[[input$name_of_merged_layer_id]]@data$id,
                                                        group=input$name_of_merged_layer_id,
                                                        highlightOptions = leaflet::highlightOptions(color = "#000000", weight = 3,bringToFront = F))%>%
      leaflet::addLayersControl(
        overlayGroups =names(valeur_carte$carte),
        baseGroups =c("OSM (default)","Positron","Toner Lite"),
        options = leaflet::layersControlOptions(collapsed = T,autoZIndex = F))

  })
  
  
  #initialiser le widget qui permet de changer l'ordre des couches
  voir_les_couches_2_=eventReactive(
    c("",input$valider_merged_layer_id),{
      check_box_rank_list=function(nom_des_couches,text,input_id=input_id){
        labels=list()
        for(i in 1:length(nom_des_couches)){labels[[i]]=shinyWidgets::prettyCheckbox(nom_des_couches[i], inputId=nom_des_couches[i],T)}
        return(sortable::rank_list(text=text,labels=labels,input_id=input_id))}
      
      if(n_initialize_map()==1){check_box_rank_list(nom_des_couches=names(valeur_carte$carte),text="Afficher la/les couche(s):",input_id="voir_les_couches_2")}
      else{
        valeur_carte$carte=valeur_carte$carte[1:valeur_carte$n_layer]
        check_box_rank_list(nom_des_couches=names(valeur_carte$carte)[1:valeur_carte$n_layer],text="Afficher la/les couche(s):",input_id="voir_les_couches_2")}
      
    })
  output$voir_les_couches2=renderUI(voir_les_couches_2_())
  
  # #changer l'ordre des couches
  observeEvent(c(input$dessiner_carte_id,
                 input$voir_les_couches_2,
                 input$valider_merged_layer_id,
                 input_check_box_rank(names(valeur_carte$carte),input,input$voir_les_couches_2)$list_check_box_input),{
    
    
    vec=input_check_box_rank(names(valeur_carte$carte),input,input$voir_les_couches_2)$valeurs_cochees
    leaflet::leafletProxy("map")%>%leaflet::hideGroup("selected_point")%>%leaflet::hideGroup("selected_shape")
    for(i in names(valeur_carte$carte)){leaflet::leafletProxy("map")%>%leaflet::hideGroup(i)}
    if(length(vec>0)){for(i in length(vec):1 ){leaflet::leafletProxy("map")%>%leaflet::showGroup(vec[i])}}
    leaflet::leafletProxy("map")%>%leaflet::showGroup("selected_point")%>%leaflet::showGroup("selected_shape")})
  
  
  
  #- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  #CREER UNE VARIABLE DANS UNE TABLE D'ATTRIBUT
  
  # type_map_var_=eventReactive(input$valider22,{
  #   radioButtons(inputId = "type_map_var_id",label = "Créer une variable",choices = c("Créer une variable catégorielle conditionnelle"=1,"Créer une variable numérique conditionnelle"=2,"Crée une variable à partir d'une formule"=3,"Créer une variable binaire à partir d'une variable catégorielle"=4))
  # })
  # output$type_map_var=renderUI(type_map_var_())
  # 
  # 
  # 
  # observeEvent(req(input$type_map_var_id),{
  #   if(input$type_map_var_id==3){
  #     shinyjs::hide("map_df_condition")
  #     shinyjs::hide("map_nb_condition")
  #     shinyjs::hide("map_conditional_var_from_var")
  #     shinyjs::hide("map_valider_nb_condition")
  #     shinyjs::hide("map_one_value_from")
  #     shinyjs::show("map_formule")
  #     shinyjs::hide("map_create_binary_var_from_var")
  #     
  #   }
  #   
  #   else if (input$type_map_var_id %in% c(1,2)){
  #     shinyjs::show("map_df_condition")
  #     shinyjs::show("map_nb_condition")
  #     shinyjs::show("map_conditional_var_from_var")
  #     shinyjs::show("map_valider_nb_condition")
  #     shinyjs::hide("map_one_value_from")
  #     shinyjs::hide("map_formule")
  #     shinyjs::hide("map_create_binary_var_from_var")
  #     
  #   }
  #   
  #   else{
  #     shinyjs::hide("map_df_condition")
  #     shinyjs::hide("map_nb_condition")
  #     shinyjs::hide("map_conditional_var_from_var")
  #     shinyjs::hide("map_valider_nb_condition")
  #     shinyjs::hide("map_formule")
  #     shinyjs::show("map_one_value_from")
  #     shinyjs::show("map_create_binary_var_from_var")
  #     
  #   }
  #   
  # })
  # 
  # # # - - - - - - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # df_table_attribut=reactiveVal()
  # table_attribut_=eventReactive("",{selectizeInput(inputId ="table_attribut_id",label="Créer une variable de la table d'attribut:",choices=names(valeur_carte$carte),multiple=F )})
  # output$table_attribut=renderUI(table_attribut_())
  # 
  # 
  # observeEvent(input$table_attribut_id,{
  #   df_table_attribut(valeur_carte$carte[[input$table_attribut_id]]@data)
  #   
  # })
  # 
  # 
  # # # - - - - - - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # 
  # map_aide_formule_=eventReactive(input$valider22,{shinyWidgets::dropdown(inputId = "map_aide_formule_id",label = "Aides",width = "100%",
  #                                                                         strong("Les opérateurs pour vérifier des conditions"),
  #                                                                         tags$ul(
  #                                                                           tags$li("A,B et C sont des variables dans une fiche choisie"),
  #                                                                           tags$li("Vous souhaitez créer la condition  A est égal à B : A==B"),
  #                                                                           tags$li("A est supérieur B : A>B"),
  #                                                                           tags$li("A est supérieur ou égal B : A>=B"),
  #                                                                           tags$li("A est inférieur B : A<B"),
  #                                                                           tags$li("A est inférieur ou égal B : A<=B"),
  #                                                                           tags$li("A est inférieur ou égal B ou B est égal à racine carrée de C : A<=B | B==sqrt(C)"),
  #                                                                           tags$li("A est inférieur ou égal B et C est inférieur à 4 : A<=B & C<4")),
  #                                                                         strong("Fonctions utiles pour manipuler/créer des variables numériques"),
  #                                                                         tags$ul(tags$li("vous souhaitez créer une nouvelle variable NEWVAR qui est le logarithme nepérien de la variable VAR qui existe dans la fiche choisie : log(VAR)"),
  #                                                                                 tags$li("logarithme base 10 : log10(VAR)"),
  #                                                                                 tags$li("logarithme base 2 : log10(VAR)"),
  #                                                                                 tags$li("exponentielle : exp(VAR)"),
  #                                                                                 tags$li("cosinus, sinus,tangente : cos(VAR), sin(var),tan(VAR)"),
  #                                                                                 tags$li("puissance n : VAR**n"),
  #                                                                                 tags$li("moyenne : mean(VAR) (la nouvelle variable créée sera une constante)"),
  #                                                                                 tags$li("maximum : max(VAR) (la nouvelle variable créée sera une constante)"),
  #                                                                                 tags$li("minimum : min(VAR) (la nouvelle variable créée sera une constante)"),
  #                                                                                 tags$li("écart-type : sd(VAR) (la nouvelle variable créée sera une constante)"),
  #                                                                                 tags$li("Créer une variable aléatoire qui est un tirage avec remise de nombres compris entre 1 et 5 : sample(A:5,replace=TRUE)")
  #                                                                         ),
  #                                                                         strong("Fonctions utiles pour manipuler/créer des variables caractères"),
  #                                                                         tags$ul(tags$li("A et B sont des variables de type caractère"),
  #                                                                                 tags$li("Vous souhaitez extraitre le texte entre de la troisième position à la septième position de A : substr(A,3,7)"),
  #                                                                                 tags$li("Vous souhaitez concatener A et B avec un espace entre les deux variables : paste(A,' ',B)"),
  #                                                                                 tags$li("Vous souhaitez concatener A et B avec un tiret entre les deux variables : paste(A,'-',B)"),
  #                                                                                 tags$li("Vous souhaitez concatener A et le texte entre la première position et la troisième position de B avec un espace : paste(A,' ',substr(B,1,3))"),
  #                                                                                 tags$li("Vous souhaitez remplacer le patterne 'DTIR' par 'Secteur de tirage' dans la variable A : gsub('DTIR','Secteur de tirage',A)"),
  #                                                                                 tags$li("Vous souhaitez mettre au majuscule tous les caractères de la variable A : toupper(A)"),
  #                                                                                 tags$li("Vous souhaitez mettre au minuscule tous les caractères de la variable A : tolower(A)"),
  #                                                                                 tags$li(strong("Les fonctions pour manipuler les variables caractère fonctionnent également sur les variables de type numérique"))),
  #                                                                         strong("Conversion de variable"),
  #                                                                         tags$ul(tags$li("A est une variable numérique et B est une Variable caractère"),
  #                                                                                 tags$li("convertir A en une variable caractère : as.character(A)"),
  #                                                                                 tags$li("convertir B en une variable numerique : as.numerique(B)"))#,
  #                                                                         #tags$a(href="http://revue.sesamath.net/IMG/pdf/RCarte_Commandes-R.pdf", "Voir documentation")
  #                                                                         
  #                                                                         #url = a("Voir documentation", href="https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/quantile")
  # )})
  # output$map_aide_formule=renderUI(map_aide_formule_())
  # 
  # # - - - - - - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  #
  # # - - - - - - - - - - - - - - - - - - - - - - - - - - créer une variable binaire à partir d'une variable catégorielle - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  # map_create_binary_var_from_var_=eventReactive(input$table_attribut_id,{selectizeInput(inputId = "map_create_binary_var_from_var_id",label="variable",choices=colnames(valeur_carte$carte[[input$table_attribut_id]]@data)[sapply(valeur_carte$carte[[input$table_attribut_id]]@data,can_be_treated_as_categorical)],multiple=F)})
  # output$map_create_binary_var_from_var=renderUI(map_create_binary_var_from_var_())
  # 
  # map_one_value_from_=eventReactive(input$map_create_binary_var_from_var_id,{selectizeInput(inputId = "map_one_value_from_id",label="Valeur 1 pour les classes : ",choices=unique(valeur_carte$carte[[input$table_attribut_id]]@data[[input$map_create_binary_var_from_var_id]]),multiple=T)})
  # output$map_one_value_from=renderUI(map_one_value_from_())
  
  # #  - - - - - - - - - - - - - - - - - - - - - - - - Créer une variable conditionnelle/ à partir d'une formule- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  
  # map_df_condition=reactiveVal()
  # map_conditional_var_from_var_=eventReactive(input$table_attribut_id,{
  #   selectizeInput(inputId ="map_conditional_var_from_var_id",label="Valeurs de base à partir de :",choices=c("aucun",colnames(valeur_carte$carte[[input$table_attribut_id]]@data)) ,multiple=F )})
  # output$map_conditional_var_from_var=renderUI(map_conditional_var_from_var_())
  # 
  # 
  # map_name_of_var_=eventReactive("",{ textInput(inputId = "map_name_of_var_id",label="Nom de la variable",width="100%")})
  # output$map_name_of_var=renderUI(map_name_of_var_())
  # 
  # map_nb_condition_=eventReactive("",{numericInput(inputId="map_nb_condition_id",label="Nombre de conditions",value=2,min = 2,max = 100,step = 1,width = "100%")})
  # output$map_nb_condition=renderUI(map_nb_condition_())
  # 
  # map_valider_nb_condition_=eventReactive("",{actionButton(inputId = "map_valider_nb_condition_id",label="Valider",width="100%")})
  # output$map_valider_nb_condition=renderUI(map_valider_nb_condition_())
  # 
  # map_formule_=eventReactive("",textInput(inputId = "map_formule_id",label="Formule",width = "100%"))
  # output$map_formule=renderUI(map_formule_())
  # 
  # map_create_var_=eventReactive("",{actionButton(inputId = "map_create_var_id",label="Créer la variable",width="100%")})
  # output$map_create_var=renderUI(map_create_var_())
  # 
  # 
  # observeEvent(input$map_valider_nb_condition_id,{
  #   df=data.frame(matrix(c(rep("Entrez une condition ici",input$map_nb_condition_id),rep("Entrez une formule ici",input$map_nb_condition_id) ),ncol=2,nrow=input$map_nb_condition_id))
  #   colnames(df)=c("Condition","Valeur")
  #   map_df_condition(df)
  # })
  # 
  # output$map_df_condition=DT::renderDataTable(map_df_condition(),editable = list(target = "cell"),rownames=FALSE)
  # observeEvent(input$map_df_condition_cell_edit,{
  #   print(map_df_condition())
  #   print(input$map_df_condition_cell_edit$row)
  #   print(input$map_df_condition_cell_edit$col)
  #   print(input$map_df_condition_cell_edit$value)
  #   
  #   df=map_df_condition()
  #   df[input$map_df_condition_cell_edit$row,input$map_df_condition_cell_edit$col+1]=input$map_df_condition_cell_edit$value
  #   map_df_condition(df)
  # })
  # 
  # 
  # observeEvent(input$map_create_var_id,{
  #   progress= shiny::Progress$new()
  #   on.exit(progress$close())
  #   
  #   print("map_create_var_id_1")
  #   print(input$map_name_of_var_id)
  #   if (nchar(input$map_name_of_var_id)==0){showModal(modalDialog(title="Erreur","vous n'avez pas nommé la nouvelle variable",footer = NULL,easyClose = T))}
  #   else if(input$map_name_of_var_id %in% colnames(valeur_carte$carte[[input$table_attribut_id]]@data)){showModal(modalDialog(title="Erreur","Une variable du même nom est déjà présente dans la fiche choisie",footer = NULL,easyClose = T))}
  #   else{
  #     
  #     progress$set(message = paste0("Création de la variable ",input$map_name_of_var_id), value = 0)
  #     
  #     
  #     if(input$type_map_var_id==1){
  #       print("map_create_var_id_2")
  #       m=ncol(valeur_carte$carte[[input$table_attribut_id]]@data)+1
  #       df=f_conditional_var(data=valeur_carte$carte[[input$table_attribut_id]]@data,df_condition=map_df_condition(),modif_from_var=input$map_conditional_var_from_var_id,name_of_conditional_var=input$map_name_of_var_id,type="categorielle")
  #       if(class(df)=="character"){showModal(modalDialog(title="Erreur",df,footer = NULL,easyClose = T))}
  #       else{
  #         valeur_carte$carte[[input$table_attribut_id]]@data=df
  #         n=ncol(valeur_carte$carte[[input$table_attribut_id]]@data)
  #         valeur_carte$carte[[input$table_attribut_id]]@data[,m:n]=valeur_carte$carte[[input$table_attribut_id]]@data[,m:n]
  #         progress$inc(5/10, detail ="Travail en cours ")}}
  #     
  #     else if(input$type_map_var_id==2){
  #       print("map_create_var_id_2.5")
  #       m=ncol(valeur_carte$carte[[input$table_attribut_id]]@data)+1
  #       print(map_df_condition())
  #       df=f_conditional_var(data=valeur_carte$carte[[input$table_attribut_id]]@data,df_condition=map_df_condition(),modif_from_var=input$map_conditional_var_from_var_id,name_of_conditional_var=input$map_name_of_var_id,type="")
  #       print(df)
  #       print(class(df))
  #       print("map_create_var_id_2.6")
  #       if(class(df)=="character"){showModal(modalDialog(title="Erreur",df,footer = NULL,easyClose = T))}
  #       else{
  #         progress$inc(5/10, detail ="Travail en cours ")
  #         n=ncol(valeur_carte$carte[[input$table_attribut_id]]@data)
  #         valeur_carte$carte[[input$table_attribut_id]]@data=df
  #         valeur_carte$carte[[input$table_attribut_id]]@data[,m:n]=valeur_carte$carte[[input$table_attribut_id]]@data[,m:n]
  #       }}
  #     
  #     else if (input$type_map_var_id==3) {
  #       m=ncol(valeur_carte$carte[[input$table_attribut_id]]@data)+1
  #       print("map_create_var_id_3")
  #       df=f_create_var(data=valeur_carte$carte[[input$table_attribut_id]]@data,formule=input$map_formule_id,name_of_var=input$map_name_of_var_id)
  #       if(class(df)=="character"){showModal(modalDialog(title="Erreur",df,footer = NULL,easyClose = T))}
  #       else{
  #         progress$inc(5/10, detail ="Travail en cours ")
  #         valeur_carte$carte[[input$table_attribut_id]]@data=df
  #         n=ncol(valeur_carte$carte[[input$table_attribut_id]]@data)
  #         valeur_carte$carte[[input$table_attribut_id]]@data[,m:n]=valeur_carte$carte[[input$table_attribut_id]]@data[,m:n]
  #         
  #       }}
  #     
  #     else {
  #       print("map_create_var_id_4")
  #       var=valeur_carte$carte[[input$table_attribut_id]]@data[[input$map_create_binary_var_from_var_id]]
  #       print("map_create_var_id_5")
  #       index=which(var %in% input$map_one_value_from_id)
  #       print("map_create_var_id_6")
  #       var[index]=1
  #       print("map_create_var_id_7")
  #       var[var!=1]=0
  #       print("map_create_var_id_8")
  #       var=as.numeric(var)
  #       print("map_create_var_id_9")
  #       valeur_carte$carte[[input$table_attribut_id]]@data[[input$map_name_of_var_id]]=var
  #       #valeur_carte$carte[[input$table_attribut_id]]@data[[input$map_name_of_var_id]]=var
  #       
  #       print("map_create_var_id_10")
  #       progress$inc(5/10, detail ="Travail en cours ")
  #     }}
  #   
  #   progress$inc(10/10, detail ="Terminé ")})
  # 
  # 
  # observeEvent(input$create_map_var_id,{
  #   showModal(modalDialog(size="l",
  #                         uiOutput("table_attribut"),
  #                         uiOutput("type_map_var"),
  #                         uiOutput("map_name_of_var"),
  #                         uiOutput("map_create_binary_var_from_var"),
  #                         uiOutput("map_one_value_from"),
  #                         uiOutput("map_conditional_var_from_var"),
  #                         uiOutput("map_nb_condition"),
  #                         uiOutput("map_valider_nb_condition"),
  #                         uiOutput("map_aide_formule"),
  #                         DT::dataTableOutput("map_df_condition"),
  #                         uiOutput("map_formule"),
  #                         uiOutput("map_create_var"),
  #                         footer = NULL,easyClose = T))})
  
  
  
}