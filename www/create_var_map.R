




# uiOutput(ns("fiche_of_var")),
# uiOutput(ns("type_var")),
# uiOutput(ns("name_of_var")),
# uiOutput(ns("create_binary_var_from_var")),
# uiOutput(ns("one_value_from")),
# uiOutput(ns("conditional_var_from_var")),
# uiOutput(ns("nb_condition")),
# uiOutput(ns("valider_nb_condition")),
# DT::dataTableOutput(ns("df_condition")),
# uiOutput(ns("formule")),
# uiOutput(ns("create_var"))



# type_map_var_=eventReactive(input$valider22,{
#   radioButtons(inputId = ns("type_map_var_id"),label = "Créer une variable",choices = c("Créer une variable catégorielle conditionnelle"=1,"Créer une variable numérique conditionnelle"=2,"Crée une variable à partir d'une formule"=3,"Créer une variable binaire à partir d'une variable catégorielle"=4))
# })
# output$type_map_var=renderUI(if(ndata()){type_map_var_()})
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
# table_attribut_=eventReactive(input$initialize_map,{selectizeInput(inputId =ns("table_attribut_id"),label="Créer une variable de la table d'attribut:",choices=names(valeur_carte$carte),multiple=F )})
# output$table_attribut=renderUI(if(ndata()){table_attribut_()})
# 
# 
# observeEvent(input$table_attribut_id,{
#   df_table_attribut(valeur_carte$carte[[input$table_attribut_id]]@data)
#   
# })
# 
# 
# # # - - - - - - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# #
# # # - - - - - - - - - - - - - - - - - - - - - - - - - - créer une variable binaire à partir d'une variable catégorielle - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 
# map_create_binary_var_from_var_=eventReactive(input$table_attribut_id,{selectizeInput(inputId = ns("map_create_binary_var_from_var_id"),label="variable",choices=colnames(valeur_carte$carte[[input$table_attribut_id]]@data)[sapply(valeur_carte$carte[[input$table_attribut_id]]@data,can_be_treated_as_categorical)],multiple=F)})
# output$map_create_binary_var_from_var=renderUI(map_create_binary_var_from_var_())
# 
# map_one_value_from_=eventReactive(input$map_create_binary_var_from_var_id,{selectizeInput(inputId = ns("map_one_value_from_id"),label="Valeur 1 pour les classes : ",choices=unique(valeur_carte$carte[[input$table_attribut_id]]@data[[input$map_create_binary_var_from_var_id]]),multiple=T)})
# output$map_one_value_from=renderUI(map_one_value_from_())
# 
# # #  - - - - - - - - - - - - - - - - - - - - - - - - Créer une variable conditionnelle/ à partir d'une formule- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 
# 
# map_df_condition=reactiveVal()
# map_conditional_var_from_var_=eventReactive(input$table_attribut_id,{
#   selectizeInput(inputId =ns("map_conditional_var_from_var_id"),label="Valeurs de base à partir de :",choices=c("aucun",colnames(valeur_carte$carte[[input$table_attribut_id]]@data)) ,multiple=F )})
# output$map_conditional_var_from_var=renderUI(map_conditional_var_from_var_())
# 
# 
# map_name_of_var_=eventReactive(input$initialize_map,{ textInput(inputId = ns("map_name_of_var_id"),label="Nom de la variable",width="100%")})
# output$map_name_of_var=renderUI(if(nmap()){map_name_of_var_()})
# 
# map_nb_condition_=eventReactive(input$initialize_map,{numericInput(inputId=ns("map_nb_condition_id"),label="Nombre de conditions",value=2,min = 2,max = 100,step = 1,width = "100%")})
# output$map_nb_condition=renderUI(if(nmap()){map_nb_condition_()})
# 
# map_valider_nb_condition_=eventReactive(input$initialize_map,{actionButton(inputId = ns("map_valider_nb_condition_id"),label="Valider",width="100%")})
# output$map_valider_nb_condition=renderUI(map_valider_nb_condition_())
# 
# map_formule_=eventReactive(input$initialize_map,textInput(inputId = ns("map_formule_id"),label="Formule",width = "100%"))
# output$map_formule=renderUI(if(nmap()){map_formule_()})
# 
# map_create_var_=eventReactive(input$initialize_map,{actionButton(inputId = ns("map_create_var_id"),label="Créer la variable",width="100%")})
# output$map_create_var=renderUI(if(nmap()){map_create_var_()})
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
#   print("create_var_id_1")
#   if (nchar(input$map_name_of_var_id)==0){showModal(modalDialog(title="Erreur","vous n'avez pas nommé la nouvelle variable",footer = NULL,easyClose = T))}
#   else if(input$map_name_of_var_id %in% colnames(valeur_carte$carte[[input$table_attribut_id]]@data)){showModal(modalDialog(title="Erreur","Une variable du même nom est déjà présente dans la fiche choisie",footer = NULL,easyClose = T))}
#   else{
#     
#     progress$set(message = paste0("Création de la variable ",input$name_of_var_id), value = 0)
#     
#     
#     if(input$type_var_id==1){
#       print("create_var_id_2")
#       m=ncol(valeur_carte$carte[[input$table_attribut_id]]@data)+1
#       df=f_conditional_var(data=valeur_carte$carte[[input$table_attribut_id]]@data,df_condition=map_df_condition(),modif_from_var=input$conditional_var_from_var_id,name_of_conditional_var=input$name_of_var_id,type="categorielle")
#       if(class(df)=="character"){showModal(modalDialog(title="Erreur",df,footer = NULL,easyClose = T))}
#       else{
#         valeur_carte$carte[[input$table_attribut_id]]@data=df
#         n=ncol(valeur_carte$carte[[input$table_attribut_id]]@data)
#         valeur_carte$carte[[input$table_attribut_id]]@data[,m:n]=valeur_carte$carte[[input$table_attribut_id]]@data[,m:n]
#         progress$inc(5/10, detail ="Travail en cours ")}}
#     
#     else if(input$type_var_id==2){
#       print("create_var_id_2.5")
#       m=ncol(valeur_carte$carte[[input$table_attribut_id]]@data)+1
#       df=f_conditional_var(data=valeur_carte$carte[[input$table_attribut_id]]@data,df_condition=map_df_condition(),modif_from_var=input$conditional_var_from_var_id,name_of_conditional_var=input$name_of_var_id,type="")
#       print(df)
#       print(class(df))
#       print("create_var_id_2.6")
#       if(class(df)=="character"){showModal(modalDialog(title="Erreur",df,footer = NULL,easyClose = T))}
#       else{
#         progress$inc(5/10, detail ="Travail en cours ") 
#         n=ncol(valeur_carte$carte[[input$table_attribut_id]]@data)
#         valeur_carte$carte[[input$table_attribut_id]]@data=df
#         valeur_carte$carte[[input$table_attribut_id]]@data[,m:n]=valeur_carte$carte[[input$table_attribut_id]]@data[,m:n]
#       }}
#     
#     else if (input$type_var_id==3) {
#       m=ncol(valeur_carte$carte[[input$table_attribut_id]]@data)+1
#       print("create_var_id_3")
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
#       print("create_var_id_4")
#       var=valeur_carte$carte[[input$table_attribut_id]]@data[[input$create_binary_var_from_var_id]]
#       print("create_var_id_5")
#       index=which(var %in% input$map_one_value_from_id)
#       print("create_var_id_6")
#       var[index]=1
#       print("create_var_id_7")
#       var[var!=1]=0
#       print("create_var_id_8")
#       var=as.numeric(var)
#       print("create_var_id_9")
#       valeur_carte$carte[[input$table_attribut_id]]@data[[input$map_name_of_var_id]]=var
#       #valeur_carte$carte[[input$table_attribut_id]]@data[[input$map_name_of_var_id]]=var
#       
#       print("create_var_id_10")
#       progress$inc(5/10, detail ="Travail en cours ")            
#     }
#     
#     if(input$type_var_id %in% c(1,4) | can_be_treated_as_categorical(valeur_carte$carte[[input$table_attribut_id]]@data[[input$map_name_of_var_id]]) ){
#       print("create_var_id_14")
#       categorical_vars(c(categorical_vars(),input$map_name_of_var_id))
#       
#       # if(is_there_libel()){
#       #   vec=c(input$map_name_of_var_id,input$map_name_of_var_id,"txt",0,rep(NA,ncol(FICHES$VAR_LIBEL)-4))
#       #   FICHES$VAR_LIBEL=rbind(FICHES$VAR_LIBEL,vec)}
#       #print("create_var_id_15")
#       #nom_long_vars(c(nom_long_vars(),input$map_name_of_var_id))
#       #nom_court_vars(c(nom_court_vars(),input$map_name_of_var_id))
#       #print("create_var_id_16")
#       #updateSelectInput(session,inputId ="var_long_libel_id",choices = nom_long_vars())
#       #updateSelectInput(session,inputId ="var_court_libel_id",choices = nom_court_vars())
#     }
#   }
#   progress$inc(10/10, detail ="Terminé ")})
# 
# 
# 
# 
# 
# 
# 
# 
# observeEvent(input$create_map_var_id,{
#   showModal(modalDialog(size="l",
#                         uiOutput(ns("table_attribut")),
#                         uiOutput(ns("type_map_var")),
#                         uiOutput(ns("map_name_of_var")),
#                         uiOutput(ns("map_create_binary_var_from_var")),
#                         uiOutput(ns("map_one_value_from")),
#                         uiOutput(ns("map_conditional_var_from_var")),
#                         uiOutput(ns("map_nb_condition")),
#                         uiOutput(ns("map_valider_nb_condition")),
#                         DT::dataTableOutput(ns("map_df_condition")),
#                         uiOutput(ns("map_formule")),
#                         uiOutput(ns("map_create_var")),
#                         footer = NULL,easyClose = T))
# })

#  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#
#
#
#
#
#
#
#
#
#
#   #- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   # formula_=eventReactive(input$valider22,{textInput("formule", "Formule", NULL)})
#   # output$formule=renderUI(formula_())
#   #
#   # nom_new_var_=eventReactive(input$valider22,{textInput("nom_new_var", "Ma variable", NULL)})
#   # output$nom_new_var=renderUI(nom_new_var_())
#   #
#   # valider_new_var_=eventReactive(input$valider22,{actionButton("valider_new_var","Valider",width="100%")})
#   # output$valider_new_var=renderUI(valider_new_var_())
#
