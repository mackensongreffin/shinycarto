# map1=rgdal::readOGR("data","DEP", use_iconv = TRUE, encoding = "UTF-8")
# map1@data$id=1:nrow(map1@data)
# map1@data[,c("long","lat")]=sp::coordinates(map1)
# map1@data$selected=rep(0,nrow(map1@data))
# head(map1@data)
# df= read_excel("data/base-ic-evol-struct-pop-2018.xlsx")
# colnames(df)=df[4,]
# df=df[-c(1,2,3,4,5),]
# df=df%>%dplyr::mutate_at(colnames(df)[13:50],as.numeric)
# a=df%>%dplyr::group_by(Département)%>%dplyr::summarise_if(is.numeric, sum, na.rm = TRUE)
# map1@data=dplyr::left_join(map1@data,a,by=c("dep_code"="Département"))
# 
# map2=rgdal::readOGR("data","REG", use_iconv = TRUE, encoding = "UTF-8")
# map2@data$id=1:nrow(map2@data)
# map2@data[,c("long","lat")]=sp::coordinates(map2)
# map2@data$selected=rep(0,nrow(map2@data))
# a=df%>%dplyr::group_by(Région)%>%dplyr::summarise_if(is.numeric, sum, na.rm = TRUE)
# map2@data=dplyr::left_join(map2@data,a,by=c("reg_code"="Région"))
# map2=subset(map2,map2@data$reg_name!="Mayotte")
# save.image("C:/Users/GREFFIN Mackenson/Desktop/shinycarto/data/rdata.RData")


f_popups=function(data_row,column_names){
  str=""
  for(i in 1:length(column_names)){
    str=paste0(str,"<b>",column_names[i]," : <b>",data_row[i],"<br>")
  }
  return(str)
}

# map1@data$content=f_popups(map1@data[1,],colnames(map1@data))
# map2@data$content=f_popups(map2@data[1,],colnames(map2@data))

a=apply(map1@data,1,f_popups,column_names=colnames(map1@data)  )

map_operation_validation=function(type_representation,var_num_ou_char_map_id,data,nb_quantiles){
  if(type_representation=="quantile"){a=class(try(leaflet::colorQuantile("viridis",data[[var_num_ou_char_map_id]],nb_quantiles)(data[[var_num_ou_char_map_id]]),T))}
  else if( type_representation=="bin"){a=class(try(leaflet::colorBin("viridis",data[[var_num_ou_char_map_id]],nb_quantiles)(data[[var_num_ou_char_map_id]]),T))}
  else{a="ok"}
  if(a!="try-error"){a="ok"}
  return(a)
}

fix_zoom=function(map){
  htmlwidgets::onRender(map,jsCode=
                          "function(el, x) {
          L.control.zoom({
            position:'bottomleft'
          }).addTo(this);
        }",data=NULL)
  
}
  


  
retour_a_la_ligne=function(text){return(HTML(gsub("(.{21,}?)\\s", "\\1\\<br/>",text)))}





create_map_proxy=function(map,carte,nom_de_la_couche,type_representation,type_representation_2,opacity,opacity_line,etiquette,legende,titre_legende,palette=NULL,taille_etiquette=10,taille_points=1,taille_points2=1,nombre_de_classes=2){

  if(class(carte)[1]=="SpatialPolygonsDataFrame"){
    n=nrow(carte@data)
      if(type_representation=="simple"){pal_=leaflet::colorFactor(type_representation_2,rep(titre_legende,n))}
    else if (type_representation=="classe"){pal_=leaflet::colorFactor(palette=palette,domain=carte@data[[type_representation_2]])}
    else if (type_representation=="gradient"){pal_=leaflet::colorNumeric(palette=palette,domain=carte@data[[type_representation_2]])}
    else if (type_representation=="quantile"){pal_=leaflet::colorQuantile(palette=palette,domain=carte@data[[type_representation_2]],n=nombre_de_classes,probs=seq(0, 1, length.out = nombre_de_classes + 1))}
    else if (type_representation=="bin"){pal_=leaflet::colorBin(palette=palette,domain=carte@data[[type_representation_2]],bins=nombre_de_classes,pretty=TRUE)}
    if (type_representation %in% c("gradient","quantile","bin","classe") & type_representation_2 %in% colnames(carte@data) ){fillColor=pal_(carte@data[,type_representation_2])}
    else{fillColor=pal_(rep(titre_legende,n))}

    leaflet::addPolygons(map,data=carte,
                color = "#444444",
                fillOpacity = opacity,
                weight = opacity_line,
                smoothFactor = 0.5,
                opacity =1.0 ,
                layerId = ~carte@data$id,
                group=nom_de_la_couche,
                fillColor = ~fillColor,
                popup = ~carte@data$content,
                popupOptions=leaflet::popupOptions(maxHeight = 300,opacity = 0.95),
                highlightOptions = leaflet::highlightOptions(color = "#444444", weight = 3,bringToFront = F))#%>%
      #leaflet::addPopups(carte@data$long, carte@data$lat, "content",options = popupOptions(permanent=F,closeButton = TRUE,sticky = TRUE))
    
    if(etiquette!="aucune"){leaflet::leafletProxy("map")%>%leaflet::clearGroup(paste0("label",nom_de_la_couche))%>%leaflet::addCircleMarkers(carte@data$long, carte@data$lat,group=paste0("label",nom_de_la_couche), opacity = 0, fillOpacity = 0,label =  as.character(carte@data[,etiquette]),labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = F,textsize = paste(taille_etiquette,"px",sep="")))}
    else{leaflet::leafletProxy("map")%>%leaflet::clearGroup(paste0("label",nom_de_la_couche))}


    if(legende && type_representation %in% c("gradient","classe","bin")){
      leaflet::addLegend(map,
                "topleft",
                pal = pal_,
                values = carte@data[[type_representation_2]],
                title=retour_a_la_ligne(titre_legende),
                group=nom_de_la_couche,
                layerId = paste0("legende_",nom_de_la_couche),
                labFormat = leaflet::labelFormat(prefix = ""),
                opacity = 1)}
    
    else if(legende && type_representation =="quantile"){
    leaflet::addLegend(
      map,
      "topleft",
      pal = pal_,
      values = carte@data[[type_representation_2]],
      title=retour_a_la_ligne(titre_legende),
      group=nom_de_la_couche,
      layerId = paste0("legende_",nom_de_la_couche),
      opacity = 1,
      
      labFormat = function(type, cuts, p) {
        n = length(cuts)
        p = paste0(round(p * 100), '%')
        cuts = paste0(p[-n],'-',p[-1],' ','(',formatC(cuts[-n])," - ", formatC(cuts[-1]),')')
        paste0('<span title="', p[-n], " - ", p[-1], '">', cuts,'</span>')}
    )}
    
    else if(legende && type_representation=="simple"){
      leaflet::addLegend(map,"topleft",
                colors = type_representation_2,
                labels =titre_legende,
                #title =titre_legende,
                title=retour_a_la_ligne(titre_legende),
                values = ~ rep(titre_legende,n),
                group=nom_de_la_couche,
                layerId = paste0("legende_",nom_de_la_couche),
                labFormat = leaflet::labelFormat(prefix = ""),
                #labelStyle = 'margin: auto;',
                opacity = 1)}
    
    
     
    

    
    
    
  }}


create_map_proxy2=function(map,carte,nom_de_la_couche,type_representation,type_representation_2,opacity,etiquette,legende,titre_legende,palette=NULL,taille_etiquette=10,taille_points="uniforme",taille_points2=1,symbol="plus",nombre_de_classes=9){
  #taille_points
  if(class(carte)[1]=="SpatialPointsDataFrame"){
    
    n=nrow(carte@data)
    if(taille_points=="uniforme"){sizes=leaflegend::sizeNumeric(rep(1,n), baseSize = taille_points2)}
    else{sizes=leaflegend::sizeNumeric(carte@data[[taille_points]], baseSize = taille_points2)}
    
    if(type_representation=="simple"){pal_=leaflet::colorFactor(type_representation_2,rep(titre_legende,n))}
    else if (type_representation=="classe"){pal_=leaflet::colorFactor(palette=palette,domain=carte@data[[type_representation_2]])}
    else if (type_representation=="gradient"){pal_=leaflet::colorNumeric(palette=palette,domain=carte@data[[type_representation_2]])}
    else if (type_representation=="quantile"){pal_=leaflet::colorQuantile(palette=palette,domain=carte@data[[type_representation_2]],n=nombre_de_classes,probs=seq(0, 1, length.out = nombre_de_classes + 1))}
    else if (type_representation=="bin"){pal_=leaflet::colorBin(palette=palette,domain=carte@data[[type_representation_2]],bins=nombre_de_classes,pretty=F)}
    
    
    if(type_representation=="simple"){symbols= Map(leaflegend::makeSymbol,shape = symbol,color =pal_(rep(titre_legende,n)),width = sizes,height = sizes,opacity = opacity)}
    else {symbols= Map(leaflegend::makeSymbol,shape = symbol,color =pal_(carte@data[[type_representation_2]]),width = sizes,height = sizes, opacity = opacity)}
    
    
    leaflet::addMarkers(map,data = carte,
               icon = leaflet::icons(iconUrl = symbols),
               lat = ~carte@data$lat,
               lng = ~carte@data$long,
               layerId = ~carte@data$id,
               group=nom_de_la_couche,
               #clusterOptions = markerClusterOptions( spiderLegPolylineOptions = list(weight = 1.5, color = "blue", opacity = 0.5))
               options = leaflet::pathOptions())
    
    if(legende & type_representation=="gradient"){
      leaflegend::addLegendNumeric(map,
                                   pal = pal_,
                                   values = carte@data[[type_representation_2]],
                                   group=nom_de_la_couche,
                                   #layerId=paste0(nom_de_la_couche,"-",1:n),
                                   layerId = paste0("legende_",nom_de_la_couche),
                                   #title = titre_legende,
                                   title=retour_a_la_ligne(titre_legende)
                                   )}

    else if(legende &  type_representation=="quantile"){
      leaflegend::addLegendQuantile(map,
                                    pal = pal_,
                                    opacity = opacity,
                                    values =carte@data[[type_representation_2]],
                                    group=nom_de_la_couche,
                                    #title = titre_legende,
                                    title=retour_a_la_ligne(titre_legende),
                                    labelStyle = 'margin: auto;',
                                    shape = c(symbol),
                                    #layerId=paste0(nom_de_la_couche,"-",1:n),
                                    layerId = paste0("legende_",nom_de_la_couche),
                                    orientation = c('vertical', 'horizontal'))}
    else if(legende & type_representation=="bin"){
      leaflegend::addLegendBin(map,
                   pal = pal_,
                   opacity = opacity,
                   values =carte@data[[type_representation_2]],
                   group=nom_de_la_couche,
                   #title = titre_legende,
                   title=retour_a_la_ligne(titre_legende),
                   #layerId=paste0(nom_de_la_couche,"-",1:n),
                   layerId = paste0("legende_",nom_de_la_couche),
                   labelStyle = 'margin: auto;',
                   shape = c(symbol),
                   orientation = c('vertical', 'horizontal'))}

    else if(legende  & type_representation %in% c("classe","simple")){
      if(type_representation=="simple"){
        values=rep(titre_legende,n)
        #titre_legende=""
        }
      else{values=carte@data[[type_representation_2]]}
      leaflegend::addLegendFactor(map,
                      pal = pal_,
                      opacity = opacity,
                      #layerId=paste0(nom_de_la_couche,"-",1:n),
                      layerId = paste0("legende_",nom_de_la_couche),
                      values =values,
                      group=nom_de_la_couche,
                      #title = titre_legende,
                      title=retour_a_la_ligne(titre_legende),
                      labelStyle = 'margin: auto;',
                      shape = c(symbol),
                      orientation = c('vertical', 'horizontal'))}
  
  
  
  if(legende & taille_points!="uniforme"){leaflegend::addLegendSize(
                                             map,
                                             pal = pal_,
                                             opacity = opacity,
                                             values =carte@data[[taille_points]],
                                             group=nom_de_la_couche,
                                             #title = titre_legende,
                                             title=retour_a_la_ligne(titre_legende),
                                             labelStyle = 'margin: auto;',
                                             shape = c(symbol),
                                             layerId=paste0(nom_de_la_couche,"--",1:n),
                                             orientation = c('vertical', 'horizontal'),
                                             breaks = 5)}
  
  
  }}





fix_legend=function(map){
  htmlwidgets::onRender(map,jsCode="
      function(el, x) {
         var updateLegend = function () {
            var selectedGroup = document.querySelectorAll('input:checked')[0].nextSibling.innerText.substr(1);

            document.querySelectorAll('.legend').forEach(a => a.hidden=true);

            /*document.querySelectorAll('.legend').forEach(l => {if (l.classList.contains(selectedGroup)) l.hidden=true;});*/
         };
         updateLegend();
         /*this.on('baselayerchange', el => updateLegend());*/
      }",data=NULL)
  
  
}


























graphique_selected_shape_1=function(data,selected_var,titre="MON JOLI GRAPHIQUE",palette="viridis",color="black"){
  m = list(
    l = 0,
    r = 60,
    b = 0,
    t = 60,
    pad = 0
  )
  df=data.frame(colSums(data[,selected_var]))
  df$Var1=rownames(df)
  colnames(df)[1]="Freq"
  rownames(df)=NULL
  #df$Freq=df$Freq*100/sum(df$Freq)
  N=nrow(df)

  attach(df)
  
  titre=paste("<br>","<b>",titre,"<b>",sep="")
  t=list(family ="'Oswald', sans-serif",size = 12,color ="#051937")
  if(palette %in% c("viridis","magma","inferno","plasma","cividis")){palette_=viridis::viridis_pal(option = palette)(N)}
  else if(palette %in% c("Greys","Greens","GnBu","BuPu","BuGn","Blues" ,"Oranges","Reds","Paired" ,"Set1")){palette_=scales::brewer_pal(palette=palette)(N)}
  
  color2=col2rgb(color, alpha = F)[,1]
  color2=paste0('rgba(',color2[1],',',color2[2],',',color2[3],',',0.65,')')
  # Camembert
  fig=plotly::plot_ly() %>%plotly::add_trace(df, labels=~Var1,values=~Freq,type = 'pie',marker = list(colors =palette_))%>%
    plotly::layout(title = titre,
           xaxis = list(showgrid = F, zeroline = TRUE, showticklabels = TRUE),
           yaxis = list(showgrid = F, zeroline = TRUE, showticklabels = TRUE),
           font=t) %>%plotly::layout(margin=m)
  return(fig)
}



graphique_selected_shape_2=function(data,selected_var,label=NULL,titre="MON JOLI GRAPHIQUE",palette="viridis",color="black",titre_x,titre_y){
  m = list(
    l = 0,
    r = 0,
    b = 0,
    t = 0,
    pad = 0
  )
  df=data%>% tidyr::pivot_longer(selected_var)
  df$name[is.na(df$name)]=0
  fig=plotly::plot_ly()%>% plotly::add_trace(df,
                             x=~rep(data[[label]],each=length(selected_var)),
                             y=~df$value,
                             color=~as.character(df$name),
                             text = df$percent,
                             textposition = 'auto',
                             textfont = list(color = '#000000', size = 16),
                             type="bar",
                             textposition = 'auto',
                             colors = palette)%>%plotly::layout(xaxis = list(title=label,showgrid = F),
                                                        yaxis = list(title=titre_y,showgrid = T),
                                                        
                                                        barmode = 'stack',margin=m)
  
  return(fig)
  
  
}
#______________________________________________________________________________________________________________________________
#chemin="C:/Users/GREFFIN Mackenson/Desktop/navbar_lexem/valencienne"
read_all_shape_in_folder=function(session,chemin){
  chemins=list.files(path=chemin, pattern=".shp", all.files=T,full.names=T)
  
  progress= shiny::Progress$new()
  on.exit(progress$close()) 
  progress$set(message = "Chargement des données", value = 0)
   
  f=function(string){
    return(gsub(".shp","",tail(stringr::str_split(string,"/")[[1]],n=1)))}
  
  noms=sapply(chemins, f)

  L=list()
  for(i in 1:length(noms)){
    vec=unique(sapply(c(paste0(chemin,"/",noms[i],".dbf"),paste0(chemin,"/",noms[i],".shx"),paste0(chemin,"/",noms[i],".prj")),file.exists))
    if(length(vec)==1 & vec[1]){
    j=try(rgdal::readOGR(chemins[i],layer =noms[i],verbose = F,use_iconv = TRUE, encoding = "UTF-8"),silent=TRUE)
    if(class(j)!="try-error"){
      if(length(grep("unknown",raster::crs(j)@comment))==0){
      L[[noms[i]]]=j
      L[[noms[i]]]@data$id=1:nrow(L[[noms[i]]]@data)#paste0(noms[i],"_",1:nrow(L[[noms[i]]]@data))
      L[[noms[i]]]@data$selected=0
      L[[noms[i]]]@data[,c("long","lat")]=sp::coordinates(L[[noms[i]]])
      progress$inc(5/10, detail = paste0("Lecture de la couche ",noms[i]))}
    }}}
  progress$inc(10/10, detail = paste0("Chargement terminé",noms[i]))
  return(L)
  
}



# initialiser la carte dans shiny avec les shapefiles dans le dossier importé
initialize_proxy=function(map,L,session){
  leaflet::clearControls(map)%>%leaflet::clearShapes()%>%leaflet.minicharts::clearMinicharts()
  progress= shiny::Progress$new()
  on.exit(progress$close()) 
  progress$set(message = "Ajout des couches sur la carte", value = 0)
  max_bbox=0
  index_max_bbox=0
  for(i in 1:length(L)){
    if(length(grep("unknown",raster::crs(L[[i]])@comment))==0){
    if(class(L[[i]])[1]=="SpatialPolygonsDataFrame"){
      if(length(grep("WGS 84",raster::crs(L[[i]])@comment))==1){
      bbox_i=sf::st_bbox(L[[i]])
      if(bbox_i[3]-bbox_i[1]+ bbox_i[4]-bbox_i[2] > max_bbox ){
        max_bbox=bbox_i[3]-bbox_i[1]+ bbox_i[4]-bbox_i[2] 
        index_max_bbox=i}}
      leaflet::addPolygons(map,data=L[[i]],
                  color = "#000000", weight = 2, smoothFactor = 0.5,opacity =1 ,
                  fillOpacity = 0,
                  fillColor = "#FFFFFF",
                  layerId = ~L[[i]]@data$id,
                  group=names(L)[i],
                  highlightOptions = leaflet::highlightOptions(color = "#000000", weight = 3,bringToFront = F))
      progress$inc(5/10, detail =paste0("Ajout de la couche ",names(L)[i]))}
    else {
      if(length(grep("WGS 84",raster::crs(L[[i]])@comment))==1){
        bbox_i=sf::st_bbox(L[[i]])
        if(bbox_i[3]-bbox_i[1]+ bbox_i[4]-bbox_i[2] > max_bbox ){
          max_bbox=bbox_i[3]-bbox_i[1]+ bbox_i[4]-bbox_i[2] 
          index_max_bbox=i}}
      pal_=leaflet::colorFactor("blue",rep("0",nrow(L[[i]]@data)))
      sizes=leaflegend::sizeNumeric(rep(1,nrow(L[[i]]@data)), baseSize =20)
      symbols= Map(leaflegend::makeSymbol,shape ="rect",color =pal_(rep("0",nrow(L[[i]]@data))),width = sizes,height = sizes,opacity = 0.5)
      leaflet::addMarkers(map,
                 data = L[[i]],
                 icon = leaflet::icons(iconUrl = symbols),
                 lat = ~L[[i]]@data$lat,
                 lng = ~L[[i]]@data$long,
                 layerId = ~L[[i]]@data$id,
                 group=names(L)[i],
                 #clusterOptions = markerClusterOptions( spiderLegPolylineOptions = list(weight = 1.5, color = "blue", opacity = 0.5))
                 options = leaflet::pathOptions())
      
      progress$inc(5/10, detail =paste0("Ajout de la couche ",names(L)[i]))
    }}}
    
    
  if(length(L)>0){
  bbox1=as.vector(L[[index_max_bbox]]@bbox) # on ititialise le zoom de la carte avec la couche la plus large en dimension

  leaflet::addProviderTiles(map,leaflet::providers$CartoDB.Positron, group = "Positron") %>%
  leaflet::addTiles(group = "OSM (default)")%>%
    leaflet::addProviderTiles(leaflet::providers$Stamen.TonerLite, group = "Toner Lite") %>%leaflet::addLayersControl(
      position ="bottomleft",
      overlayGroups =names(L),
      baseGroups =c("Positron","OSM (default)","Toner Lite"),
      options = leaflet::layersControlOptions(collapsed = T,autoZIndex = F)) %>% fix_zoom()
  
  if(max_bbox!=0){leaflet::fitBounds(map,bbox1[1],bbox1[2],bbox1[3],bbox1[4])}
  progress$inc(8/10, detail =paste0("Ajout des fonds de carte "))
  progress$inc(10/10, detail =paste0("Ajout des couches terminé"))}

} 

# ui = fluidPage(
#   leaflet::leafletOutput('map')
# )
# server= function(input, output,session) {
#   L=reactiveValues(carte=read_all_shape_in_folder(session,"C:/Users/Mackenson.Greffin/Desktop/04a-Fichiers Semaine/GIS"))
#   mymap=reactiveVal(leaflet::leaflet())
#   output$map=leaflet::renderLeaflet({mymap()})
#   observeEvent("",{
#     print("fcydtdgyyctrfc")
#     print(length(L$carte))
#     print(names(L$carte))
#     leaflet::leafletProxy("map")%>%initialize_proxy(L$carte,session)})
# }
# shinyApp(ui=ui, server=server)

# library(rgdal)
# shp=readOGR("C:/Users/Mackenson.Greffin/Desktop/04a-Fichiers Semaine/GIS/GIS2","zf_2005_2")
# shp2=readOGR("C:/Users/Mackenson.Greffin/Desktop/04a-Fichiers Semaine/GIS/GIS2","zf_2005")
# shp3=readOGR("C:/Users/Mackenson.Greffin/Desktop/04a-Fichiers Semaine/GIS","zf_2")
# shp4=readOGR("C:/Users/Mackenson.Greffin/Desktop/04a-Fichiers Semaine/GIS","centroides_zf_2005")
# shp5=readOGR("C:/Users/Mackenson.Greffin/Desktop/04a-Fichiers Semaine/GIS","D2_Reims")
input_check_box_rank=function(nom_des_couches,input,ordre_des_couches){
  l=list()
  f=function(i){return(input[[i]])}
  g=function(i){return(input[[i]]==T)}
  l$list_check_box_input=sapply(nom_des_couches,f)
  l$valeurs_cochees=ordre_des_couches[unlist(sapply(ordre_des_couches,g))]
  return(l)
}


#- - -- - - - - - - - - - - - - - - - - - - - - -


# dissolve polygons of shapefile

# library(rgeos)
# library(maptools)
# library(sp)
# library(rgdal)
# library(sf)
# library(nngeo)
# library(s2)
# library(raster)
# # 
# shp=rgdal::readOGR("C:/Users/mackenson.greffin/Desktop/FICHES_VALENCIENNES/GIS/valenciennes_DTIR.shp",
#                      layer ="valenciennes_DTIR",verbose = F,use_iconv = TRUE, encoding = "UTF-8")
# shp=rgdal::readOGR("C:/Users/Mackenson.Greffin/Desktop/01_Fichiers_CSV/GIS/valencienne.shp",
#                   layer ="valencienne",verbose = F,use_iconv = TRUE, encoding = "UTF-8")
#shp$DTIR=as.numeric(sample(1:5,nrow(shp@data),replace=T))
# shp=as(shp, "sf")
# 
# shp$area=sf::st_area(shp)

dissolve_shp=function(shp,var,fonction=1){
      #c("somme"=1,"max"=2,"min"=3,"moyenne"=4,"moyenne pondérée par la surface"=5
      if(fonction==1){f=sum}
      else if(fonction==2){f=max}
      else if(fonction==3){f=min}
      else if(fonction==4){f=mean}

      noms=colnames(shp@data)[sapply(shp@data,class)!="character"]
      print(noms)
      df=shp@data
      shp= rgeos::gSimplify(shp, tol = 0.00001)
      shp=as(shp, "sf")
      shp=as(shp, "Spatial")
      shp=as(shp, "SpatialPolygonsDataFrame")
      shp@data=data.frame(df[,noms])
      colnames(shp@data)=noms
      shp=as(shp, "sf")
      shp$area=sf::st_area(shp)
      shp$area=as.numeric(gsub("[m^2]","",shp$area))
      area=min(shp$area)

      if(fonction!=5){shp=raster::aggregate(shp,by=list(df[[var]]),dissolve=T,FUN=f)}
      else{shp=raster::aggregate(shp,by=list(df[[var]]),dissolve=T,FUN=sum,areaWeighted = TRUE)}
      
      sf::sf_use_s2(FALSE)
      shp=nngeo::st_remove_holes(sf::st_as_sf(shp),max_area=area)
      shp=as(shp, "Spatial")
      shp=as(shp, "SpatialPolygonsDataFrame")
      index=which(colnames(shp@data)==var)
      if(length(index)==1){shp@data=shp@data[,-index]}
      colnames(shp@data)[1]=var
      shp@data[,c("long","lat")]=sp::coordinates(shp)
      shp@data$selected=0
      shp@data$id=1:nrow(shp@data)
      return(shp)
}

# shp=dissolve_shp(shp,"NOM_DE10")
# shp=as(shp,"sf")
# plot(shp)
palette_piker=function(inputId,label="Palette :"){
  return(
    esquisse::palettePicker(
      inputId =inputId , 
      label =label ,
      width  = "100%",
      choices = list(
        "Brewer" = list(
          "Dark2"= scales::brewer_pal(palette = "Dark2")(8),
          "Greys" = scales::brewer_pal(palette = "Greys")(8),
          "Greens" = scales::brewer_pal(palette = "Greens")(8),
          "GnBu" = scales::brewer_pal(palette = "GnBu")(8),
          "BuPu" = scales::brewer_pal(palette = "BuPu")(8),
          "BuGn" = scales::brewer_pal(palette = "BuGn")(8),
          "Blues" = scales::brewer_pal(palette = "Blues")(8),
          "Oranges" = scales::brewer_pal(palette = "Oranges")(8),
          "Reds" = scales::brewer_pal(palette = "Reds")(8),
          "Paired" = scales::brewer_pal(palette = "Paired")(8),
          "Set1" = scales::brewer_pal(palette = "Set1")(8)
          
        ),
        
        # "Wesanderson"=list(
        #   "Darjeeling1"=wesanderson::wes_palette("Darjeeling1",type = "continuous",n=10),
        #   "BottleRocket1"=wesanderson::wes_palette("BottleRocket1",type = "continuous",n=10),
        #   "BottleRocket2"=wesanderson::wes_palette("BottleRocket2",type = "continuous",n=10),
        #   "Rushmore1"=wesanderson::wes_palette("Rushmore1",type = "continuous",n=10),
        #   "Royal1"=wesanderson::wes_palette("Royal1",type = "continuous",n=10),
        #   "Royal2"=wesanderson::wes_palette("Royal2",type = "continuous",n=10),
        #   "Zissou1"=wesanderson::wes_palette("Zissou1",type = "continuous",n=10),
        #   "Darjeeling2"=wesanderson::wes_palette("Darjeeling2",type = "continuous",n=10),
        #   "Chevalier1"=wesanderson::wes_palette("Chevalier1",type = "continuous",n=10),
        #   "FantasticFox1"=wesanderson::wes_palette("FantasticFox1",type = "continuous",n=10),
        #   "Moonrise1"=wesanderson::wes_palette("Moonrise1",type = "continuous",n=10),
        #   "Moonrise2"=wesanderson::wes_palette("Moonrise2",type = "continuous",n=10),
        #   "Moonrise3"=wesanderson::wes_palette("Moonrise3",type = "continuous",n=10),
        #   "Cavalcanti1"=wesanderson::wes_palette("GrandBudapest1",type = "continuous",n=10),
        #   "GrandBudapest1"=wesanderson::wes_palette("GrandBudapest1",type = "continuous",n=10),
        #   "GrandBudapest2"=wesanderson::wes_palette("GrandBudapest2",type = "continuous",n=10)
        # ),
        "Viridis" = list(
          "viridis" = viridis::viridis_pal(option = "viridis")(100),
          "magma" = viridis::viridis_pal(option = "magma")(100),
          "inferno" = viridis::viridis_pal(option = "inferno")(100),
          "plasma" = viridis::viridis_pal(option = "plasma")(100)
          #"cividis" = viridis_pal(option = "cividis")(100)
        )
        
        
      ), 
      textColor = c(rep("white", 5), rep("black", 4))))}

#Blackbody,Bluered,Blues,Cividis,Earth,Electric,Greens,Greys,Hot,Jet,Picnic,Portland,Rainbow,RdBu,Reds,Viridis,YlGnBu,YlOrRd

color_piker=function(inputId,label="Choisissez une couleur:"){
  
  return(
    
    colourpicker::colourInput(inputId, label, value = "white", showColour = "both", palette = "limited", c("#E64B35FF", "#4DBBD5FF", "#00A087FF", "#3C5488FF", "#F39B7FFF", "#8491B4FF","#91D1C2FF","#DC0000FF", "#7E6148FF","#B09C85FF"),
                              allowTransparent = FALSE))
  
}



can_be_treated_as_categorical=function(x){
  if(length(unique(x))<=100) return(T)
  else(return(F))
}



