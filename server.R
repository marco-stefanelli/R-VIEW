require("RPostgreSQL")
require("DT")
require("openair")
require("reshape")
require("tidyverse")
library('ggplot2')


source("funzioni_db.R")





#########################################################################################
#########################################################################################







#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################


function(input, output, session) {
  
  
 
  
  
  
  #######################################################################################
  #######################################################################################
  # generazione dinamica select stazione ed analizzatore
  
  
  output$titolo_grafico_parziale<- renderText({
    titolo_grafico_parziale()
  })
  
  
  output$netcd<- renderText({
    input$ui_select_rete
  })
  
  
  output$cor_netcd<- renderText({
    input$cor_ui_select_rete
  })
  
  output$nox_stz_netcd<- renderText({
    input$nox_ui_select_rete
  })
  
  
  output$statcd<- renderText({
    req(input$ui_select_stazione)
    input$ui_select_stazione
  })
  
  
  output$nox_stz_statcd<- renderText({
    req(input$nox_ui_select_stazione)
    input$nox_ui_select_stazione
  })
  
  
  output$cor_statcd<- renderText({
    req(input$ui_cor_select_stazione)
    input$ui_cor_select_stazione
  })
  
  
  output$ptid<- renderText({
    req(input$ui_select_rete, input$ui_select_stazione, input$ui_select_analizzatore)
    input$ui_select_analizzatore
  })
  
  
  output$K_ppb_microg_20<- renderText({
    req(input$ui_select_rete, input$ui_select_stazione, input$ui_select_analizzatore)
    
   
    
    netcd   <-input$ui_select_rete
    statcd  <-input$ui_select_stazione
    ptid    <-input$ui_select_analizzatore
    
    k=get_k_conversione(netcd,statcd,ptid) 
    
  
    k
    
  })
  
  
  
  output$cor_ptid<- renderText({
    req(input$cor_ui_select_rete, input$ui_cor_select_stazione, input$ui_cor_select_analizzatore)
    
    reset_grafici_corr()
    
    input$ui_cor_select_analizzatore
  })
  
  
  output$cor_K_ppb_microg_20<- renderText({
    req(input$cor_ui_select_rete, input$ui_cor_select_stazione, input$ui_cor_select_analizzatore)
    
    netcd   <-input$cor_ui_select_rete
    statcd  <-input$ui_cor_select_stazione
    ptid    <-input$ui_cor_select_analizzatore
    
    k=get_k_conversione(netcd,statcd,ptid) 
    
    k
    
  })
  
  
  
  # PRIMA PAGINA: list dinamica scelta stazione
  ################################################################################################
  output$ui_select_stazione <- renderUI({
    
    req(input$ui_select_rete)
    
    netcd<-input$ui_select_rete
    
    # PASSO TRUE per avere solo lel stazioni IN scansione
    lista_staz<-list_select_stazione(netcd,TRUE)
    
    selectInput("ui_select_stazione", "Scelta stazione", choices = lista_staz)
    
    
  })
  
  
  #########################################################################################
  # list dinamica scelta stazione per analisi NOX
  output$nox_ui_select_stazione <- renderUI({
    
    netcd<-input$nox_ui_select_rete
    
    sql_txt=paste("
                  select teanapt.netcd, teanapt.statcd, teanapt.anaptid , testat.statnm, teanapt.ana1 
                  from teanapt, testat
                  where testat.netcd=teanapt.netcd
                  and testat.statcd=teanapt.statcd
                  and testat.netcd=",netcd," and teanapt.paramcd=2
                  and ana1='NOX'
                  ")
    
    cat('nox_ui_select_stazione sql=', sql_txt)
    
    #esegue query
    ds_elencostaz <- query_postgres(sql_txt)
    
    
    
    #trasformo in lista
    lista_staz <- split(paste(ds_elencostaz$netcd,"_",ds_elencostaz$statcd,"_",ds_elencostaz$anaptid,sep=''),ds_elencostaz$statnm)
    #lista_staz <- with(ds_elencostaz, split(statcd,statnm))
    
    
    selectInput("nox_ui_select_stazione", "Scelta stazione", choices = lista_staz)

  })
  
  
  
  
  #########################################################################################
  # list dinamica scelta stazione per analisi stazione
  output$staz_ui_select_stazione <- renderUI({
    
    netcd<-input$staz_ui_select_rete
    
    check_in_scansione=input$staz_in_scansione
    
    if(check_in_scansione==TRUE)
      sql_txt=paste("select  netcd, statcd, statnm from testat   where netcd=",netcd," and statstcd=1  ")
    else  
      sql_txt=paste("select  netcd, statcd, statnm from testat   where netcd=",netcd)
    
    cat(paste('staz_ui_select_stazione sql=',sql_txt))
    
    #esegue query
    ds_elencostaz <- query_postgres(sql_txt)
    
    #trasformo in lista
    lista_staz <- split(paste(ds_elencostaz$netcd,"_",ds_elencostaz$statcd,"_",ds_elencostaz$statnm,sep=''),ds_elencostaz$statnm)
    
    #lista_staz <- with(ds_elencostaz, split(statcd,statnm))
    
    
    selectInput("staz_ui_select_stazione", "Scelta stazione", choices = lista_staz)
    
    
  })
  
  
  #########################################################################################
  # list dinamica scelta stazione per analisi correlazione
  output$cor_ui_select_rete <- renderUI({
    
    req(input$ui_select_rete)
    
    netcd<-input$staz_ui_select_rete
    
    check_in_scansione=input$staz_in_scansione
    
    lista_staz<-list_select_stazione(netcd,check_in_scansione)
    
    selectInput("cor_ui_select_rete", "Scelta stazione", choices = lista_staz)
    
    
  })
  
  
  
  
 
  
  
  #########################################################################################
  # click su pulsante button_dati_staz - estrazione dati stazione
  matrice_dati_stazione<-eventReactive(input$button_dati_staz, {
    
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Elaborazione in corso...", value = 0)
    
    
    coord_nox <-input$staz_ui_select_stazione
    
    list_coord_nox=unlist(strsplit(coord_nox, split="_"))
    
    data1<-paste(input$date_oss[1])
    data2<-paste(input$date_oss[2])
    
    netcd=list_coord_nox[1]
    statcd=list_coord_nox[2]
    
    matrice_dati_stazione <- get_serie_dati_stazione(data1,data2,netcd,statcd,"orari")
    
    #write.csv(matrice_dati_stazione, file = "dati_stazione.csv")
    matrice_dati_stazione
    
    #if(nrow(matrice_dati_stazione)>0){
        #cat("\n matrice_dati_stazione:",nrow(matrice_dati_stazione),"\n")
        
    #    write.csv(matrice_dati_stazione, file = "dati_stazione_na.csv")
        
        
        # RIMUOVO VALORI NA
        #matrice_dati_stazione<-na.omit(matrice_dati_stazione)
        
      
        #rimuovo colonna X
        #matrice_dati_stazione$X<-NULL
        
        #cat("\n 2 matrice_dati_stazione:",nrow(matrice_dati_stazione),"\n")
    #}else{    
    
  
    #}
    
  })
  
  #########################################################################################
  
  
  
  #######################################################################################
  # produce dati pivot per stazione
  
  output$dati_stazione_staz_tabella_dati<-renderDataTable({
    matrice_dati_stazione()
  })
  #######################################################################################
  
  
  
  
  #######################################################################################
  # download dati NOx
  output$download_staz_tabella_dati.csv<- downloadHandler(
    
    filename = function() { 
      paste(input$nox_ui_select_stazione, '_NOX.csv', sep='') 
    },
    content = function(file) {
      write.csv(matrice_dati_stazione(), file)
    }
  )  
  #######################################################################################
  
  
  
  
  
  #########################################################################################
  # PAIRS STAZONE DATI  STAZIONE
  # grafico analisi bivariata su ogni analizzatore della stazione
  
  output$pairs_plot_staz <- renderPlot({
    
    matrice_dati_stazione<-matrice_dati_stazione()
    
    if(nrow(matrice_dati_stazione)>0){
           
        numero_campi=length(names( matrice_dati_stazione))
        seq_num_campi=seq(1:numero_campi)
        
        pairs(matrice_dati_stazione[sample(1:nrow(matrice_dati_stazione), 100), seq_num_campi],
              lower.panel = panel.smooth,
              upper.panel = NULL,
              col = "skyblue3")
    }else{
       plot(1, type="n", axes=F, xlab="", ylab="",  main="Dati non presenti nel DB Postgres")
      
    }  
    
    
  })
  
  
  
  
  #########################################################################################
  # time plot
  # grafico su ogni serire di dati presente nella stazione
  
  output$time_plot <- renderPlot({
    
    matrice_dati_stazione<-matrice_dati_stazione()
    
    if(nrow(matrice_dati_stazione)>0){
    
        # lista inquinanti...
        a_inquinanti=names(matrice_dati_stazione)
        
        # tolgo NOX
        #a_inquinanti<-a_inquinanti[x != "NOX"]; # without elements that are "b"
        #a_inquinanti$NOX<-NULL
        
        # tolgo date
        a_inquinanti<-a_inquinanti[-1]; # via primo elemento
        
        # tolgo na
        #a_inquinanti<-na.omit(a_inquinanti)
        
        
        #cat("\n a_inquinanti:",a_inquinanti,"\n")
        
        timePlot(matrice_dati_stazione, a_inquinanti)
    }else{
      plot(1, type="n", axes=F, xlab="", ylab="",  main="Dati non presenti nel DB Postgres")
      
    }     
    
  })
  
  
  
  
  
  #######################################################################################
  # genero grafico time variation
  
  output$dati_stazione_plot_timevariation <- renderPlot({
    
    matrice_dati_stazione<-matrice_dati_stazione()
    
    if(nrow(matrice_dati_stazione)>0){
        
        a_inquinanti=names(matrice_dati_stazione)
        
        # tolgo prima colonna..
        a_inquinanti<-a_inquinanti[-1]
      
        
        #rinomino prima colonna..
        colnames(matrice_dati_stazione)[1]<-'date'
        
        timeVariation(matrice_dati_stazione,a_inquinanti, normalise = TRUE)
        #timeVariation(matrice_dati_stazione,a_inquinanti)
      
    }else{
          plot(1, type="n", axes=F, xlab="", ylab="",  main="Dati non presenti nel DB Postgres")
    }        
    
  })
  #######################################################################################
  
  
  #######################################################################################
  # genero DATI grafico time variation
  
  output$dati_plot_timevariation <-renderPrint({
    
    matrice_dati_stazione<-matrice_dati_stazione()
    
    if(nrow(matrice_dati_stazione)>0){
    
      a_inquinanti=names(matrice_dati_stazione)
      
      # tolgo prima colonna..
      a_inquinanti<-a_inquinanti[-1]
      
      
      #rinomino prima colonna..
      colnames(matrice_dati_stazione)[1]<-'date'
      
      dati<-timeVariation(matrice_dati_stazione,a_inquinanti, normalise = TRUE)
      
      dati
    }else{
        
      data.frame();  
    }  
    
  })
  #######################################################################################
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #########################################################################################
  # sommario statisitco dei dati stazione
  output$dati_stazione_sommario_statistico <- renderPrint({
    
    matrice_dati_stazione<-matrice_dati_stazione()
    
    if(nrow(matrice_dati_stazione)>0){
        summary(matrice_dati_stazione)
    }else{
      paste('Dati non presenti nel DB Postgres')
    }  
    
  })
  

  
  
  #######################################################################################
  # genero grafico rosa dei venti
  
  output$ds_plot_rosadeiventi <- renderPlot({
    
    
    nome_stazione<-input$staz_ui_select_stazione
    
    data1<-paste(input$date_oss[1])
    data2<-paste(input$date_oss[2])
    
    titolo_grafico=paste("Rosa dei venti stazione ",nome_stazione, " dal ", data1, " al ", data2)
      
    matrice_dati_stazione<-matrice_dati_stazione()
    
    if(nrow(matrice_dati_stazione)>0){
      
      a_inquinanti=names(matrice_dati_stazione)
      
      # tolgo prima colonna..
      a_inquinanti<-a_inquinanti[-1]
      
      
      #rinomino prima colonna..
      colnames(matrice_dati_stazione)[1]<-'date'
      
      windRose(matrice_dati_stazione,ws='VVP', wd='DVP', main=titolo_grafico)
      
    }else{
      plot(1, type="n", axes=F, xlab="", ylab="",  main="Dati non presenti nel DB Postgres")
    }        
    
  })
  #######################################################################################
  
  
  #######################################################################################
  # genero grafico rosa direzione NOX
  
  output$ds_plot_direzionenox <- renderPlot({
    
    matrice_dati_stazione<-matrice_dati_stazione()
    
    
    nome_stazione<-input$staz_ui_select_stazione
    
    data1<-paste(input$date_oss[1])
    data2<-paste(input$date_oss[2])
     
    
    
    
    if(nrow(matrice_dati_stazione)>0){
      
      a_inquinanti=names(matrice_dati_stazione)
      
      # tolgo prima colonna..
      a_inquinanti<-a_inquinanti[-1]
      
      
      #rinomino prima colonna..
      colnames(matrice_dati_stazione)[1]<-'date'
      
      #se esiste nox..
      if("NOx" %in% colnames(matrice_dati_stazione)){
        titolo_grafico=paste("Rosa provenienza NOx ",nome_stazione, " dal ", data1, " al ", data2)
        pollutionRose(matrice_dati_stazione,ws='VVP', wd='DVP', pollutant='NOx', type = "month", main=titolo_grafico)
        
      
      }else{
        titolo_grafico=paste("Rosa provenienza H2s ",nome_stazione, " dal ", data1, " al ", data2)
        pollutionRose(matrice_dati_stazione,ws='VVP', wd='DVP', pollutant='H2S',type = "month", main=titolo_grafico)
        
      }  
      
    }else{
      plot(1, type="n", axes=F, xlab="", ylab="",  main="Dati non presenti nel DB Postgres")
    }        
    
  })
  #######################################################################################
  
  
  
  #######################################################################################
  # genero grafico rosa PERCENTILE direzione NOX
  
  output$ds_plot_percentile_direzionenox <- renderPlot({
    
    matrice_dati_stazione<-matrice_dati_stazione()
    
    
    nome_stazione<-input$staz_ui_select_stazione
    
    data1<-paste(input$date_oss[1])
    data2<-paste(input$date_oss[2])
    
    
    
    
    if(nrow(matrice_dati_stazione)>0){
      
      a_inquinanti=names(matrice_dati_stazione)
      
      # tolgo prima colonna..
      a_inquinanti<-a_inquinanti[-1]
      
      
      #rinomino prima colonna..
      colnames(matrice_dati_stazione)[1]<-'date'
      
      #se esiste nox..
      if("NOx" %in% colnames(matrice_dati_stazione)){
        titolo_grafico=paste("Rosa provenienza NOx PERCENTILE ",nome_stazione, " dal ", data1, " al ", data2)
        percentileRose(matrice_dati_stazione,ws='VVP', wd='DVP', pollutant='NOx', type = "month", main=titolo_grafico)
        
      }else{
        titolo_grafico=paste("Rosa provenienza H2s PERCENTILE ",nome_stazione, " dal ", data1, " al ", data2)
        percentileRose(matrice_dati_stazione,ws='VVP', wd='DVP', pollutant='H2S',type = "month", main=titolo_grafico)
        
      }  
      
    }else{
      plot(1, type="n", axes=F, xlab="", ylab="",  main="Dati non presenti nel DB Postgres")
    }        
    
  })
  #######################################################################################
  
  
  
  
  
  
  
  #########################################################################################
  # list dinamica scelta analizzatore
  output$ui_select_analizzatore <- renderUI({
    
    netcd<-input$ui_select_rete
    statcd<-input$ui_select_stazione
    
    sql_txt=paste("select anaptid,ana1 from teanapt where netcd=",netcd," and statcd=", statcd, "and ana999=1 order by anaptid")
    
    
    #esegue query
    ds_elencoanaliz <- query_postgres(sql_txt)
    
    
    #trasformo in lista
    lista_analiz <- split(ds_elencoanaliz$ana1,ds_elencoanaliz$anaptid)
    lista_analiz <- with(ds_elencoanaliz, split(anaptid,ana1))
    
    selectInput("ui_select_analizzatore", "Scelta Analizzatore", choices = lista_analiz)
    
    
  })
  #######################################################################################
  
  
  
  react_dati_analisi_nox <-eventReactive(input$button_dati_an_nox, {
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Elaborazione in corso...", value = 0)
    
    
    coord_nox <-input$nox_ui_select_stazione
    
    list_coord_nox=unlist(strsplit(coord_nox, split="_"))
    netcd=list_coord_nox[1]
    statcd=list_coord_nox[2]
    ptid_nox=list_coord_nox[3]
    
    
    # devo trovare le coordinate dell'analizzatore di NO..
    sql_txt=paste("select anaptid,paramcd from teanapt where netcd=",netcd, " and statcd=",statcd," and ana1='NO' and istanzacd=1")
    
    cat(paste('react_dati_analisi_nox sql=',sql_txt))
    #eseguo query..
    ds_dati <- query_postgres(sql_txt)
    ptid_no<-ds_dati$anaptid
    
    
    # devo trovare le coordinate dell'analizzatore di NO2
    sql_txt=paste("select anaptid,paramcd from teanapt where netcd=",netcd, " and statcd=",statcd," and ana1='NO2' and istanzacd=1")
    
    #eseguo query..
    ds_dati <- query_postgres(sql_txt)
    ptid_no2<-ds_dati$anaptid
    
    #trovo il nome della stazione
    sql_txt=paste("select statnm from testat where netcd=",netcd," and statcd=",statcd)
    #eseguo query..
    ds_dati <- query_postgres(sql_txt)
    nome_stazione=ds_dati$statnm
    
    
    
    data1<-paste(input$date_oss[1])
    data2<-paste(input$date_oss[2])
    
    # imposto intervallo date in standard R
    # dd/mm/yyyy
    r_data1<-as.POSIXct(data1,format="%Y-%m-%d" , tz = 'UTC')
    r_data2<-as.POSIXct(data2,format="%Y-%m-%d" , tz = 'UTC')
    
    
    #creo sequenza
    all.dates <- seq(r_data1, r_data2, by="hour" , tz = 'UTC')
    
    #creo data frame con campo data_ora
    all.dates.frame <- data.frame(list(data_ora=all.dates))
    
    serie_nox<-get_serie_dati_bypost(data1,data2,netcd,statcd,ptid_nox,'orari')
    
    if(nrow(serie_nox)>0){
      
        serie_no <-get_serie_dati_bypost(data1,data2,netcd,statcd,ptid_no,'orari')
        serie_no2<-get_serie_dati_bypost(data1,data2,netcd,statcd,ptid_no2,'orari')
        
        #le serie hanno tutte un capo data_ora in fomrato R, le posso combinare
        dati_c1 <- merge(serie_nox, all.dates.frame, all=T)
        dati_c2 <- merge(serie_no, dati_c1, all=T)
        dati_c3 <- merge(serie_no2, dati_c2,  all=T)
        
        
        # copio il dataframe..
        dati_c4<-dati_c3
        
        # elimino NA
        #dati_c4<-na.omit(dati_c4)
        
        k_no<-1.247
        k_nox<-1.912
        k_no2<-1.912
        
        # NO2 colonna 2
        # NO  colonna 3
        # NOx colonna 4
        
        # calcolo il campo diif_ppb
        #dati_c4$diff_ppb<- (dati_c4[,4] / k_no2) +(dati_c4[,3] / k_no) - (dati_c4[,2] / k_nox)
       
        # diff = nox - (no+no2)  
        dati_c4$diff_ppb<- (dati_c4[,4] / k_nox ) -(dati_c4[,3] /k_no) - (dati_c4[,2] /k_no2)
      
        
        # normalizzo dato e ora
        dati_c5 <- merge(dati_c4, all.dates.frame, all=T)
    }else{
       data.frame()
    }    
    
  
    
    
    #dati_c5<-na.omit(dati_c5)
    #serie_nox
    #all.dates.frame
    
  })  
  #######################################################################################
  
  output$nox_tabella_dati <-DT::renderDataTable({
    DT::datatable(react_dati_analisi_nox())
  })
  
  #######################################################################################
  
  
  
  output$nox_diff_tripla<-DT::renderDataTable({
  
    dati_nox<-react_dati_analisi_nox()
    
    #elimino colonna diff_ppb
    dati_nox$diff_ppb<-NULL
    
    na_tripla<-dati_nox[ (rowSums(is.na(dati_nox)) ==2 | rowSums(is.na(dati_nox)) ==1)  ,]
    
    #na_tripla2<-dati_nox[ (rowSums(is.na(dati_nox)) ==2 )  ,]
    #na_tripla1<-dati_nox[ (rowSums(is.na(dati_nox)) ==1 )  ,]
    
    #na_tripla= merge(na_tripla1,na_tripla2)
    
    
    #write.csv(dati_nox, file = "nox_diff_tripla.csv")
    
    DT::datatable(na_tripla)
    
  })
  #######################################################################################
  
  
  
  
  
  #######################################################################################
  #######################################################################################
  #######################################################################################
  
  
  
  
  
  
  #######################################################################################
  # genero html per visualizzare attuale sql
  output$sql <- renderText({ 
    sql_txt()
  })
  
  #######################################################################################
  
  
  #######################################################################################
  # genero html per creare la tabella dati
  
  output$tabella_dati <-DT::renderDataTable({
    
    #genero tabella
    DT::datatable(dati_by_postgres ())
  })
  #######################################################################################
  
  #######################################################################################
  # genero dati per download dati orari singolo analizzatore
  
  output$download_dati_orari.csv <- downloadHandler(
  
      filename = function() { 
      paste(input$dataset, '.csv', sep='') 
    },
    content = function(file) {
      write.csv(dati_by_postgres (), file)
    }
  )
  #######################################################################################
  
  
  
  
  
  
  #######################################################################################
  # funzione che genera sommario statistico
  
  output$summary <- renderPrint({
    dati<-dati_by_postgres()
    summary(dati[,2])
  })
  #######################################################################################
  
  
  
  
  
  
  
  #######################################################################################
  #######################################################################################
  #######################################################################################
  
  # genero dati  giorno settimanale tipo
  dati_giorno_settimanale_tipo <- reactive({
    analizzatore<-cur_nome_analizzatore()
    
    
    df_postgres<-dati_by_postgres()
    medie_g_sett_tipo <- aggregate(df_postgres[,2], format(df_postgres["data_ora"],"%w-%H"),  mean, na.rm = TRUE)
    #medie_g_sett_tipo<-na.omit(medie_g_sett_tipo)
    medie_g_sett_tipo
  })  
  
  
  #######################################################################################
  # genero dati per download dati  giorno settimanale tipo
  
  output$download_dati_g_sett_tipo.csv <- downloadHandler(
    
    filename = function() { 
      paste(input$dataset, '.csv', sep='') 
    },
    content = function(file) {
      write.csv( dati_giorno_settimanale_tipo(), file)
    }
  )
  #######################################################################################
  
  
  
  
  
  #######################################################################################
  # pubblico tabella dati  giorno settimanale tipo
  output$tabella_giorno_settimanale_tipo <-DT::renderDataTable({
    
    #genero tabella dati giorno 24h tipo
    DT::datatable( dati_giorno_settimanale_tipo (),
                   options = list(
                     pageLength = 7
                   )
    )
  })
  
  
  #######################################################################################
  #######################################################################################
  #######################################################################################
  # genero dati giorno 24h tipo
  dati_giorno_24h_tipo <- reactive({
    analizzatore<-input$ui_select_analizzatore
    
    df_postgres<-dati_by_postgres()
    medie_gtipo <- aggregate(df_postgres[,2], format(df_postgres["data_ora"],"%H"),  mean, na.rm = TRUE)
    #medie_gtipo<- na.omit(medie_gtipo )
    
    
    
    medie_gtipo
  })  
  
  
  
  #######################################################################################
  # genero dati per download dati giorno 24h tipo
  
  output$download_dati_g_24h_tipo.csv <- downloadHandler(
    
    filename = function() { 
      paste(input$dataset, '.csv', sep='') 
    },
    content = function(file) {
      write.csv(  dati_giorno_24h_tipo(), file)
    }
  )
  #######################################################################################
  
  
  
  
  #######################################################################################
  # pubblico tabella dati giorno 24h tipo
  output$tabella_giorno_tipo_24h <-DT::renderDataTable({
    
    #genero tabella dati giorno 24h tipo
    DT::datatable(dati_giorno_24h_tipo (),
                  options = list(
                    pageLength = 24
                  )
    )
  })
  
  #######################################################################################
  
  
  
  
  
  #######################################################################################
  #######################################################################################
  
  
  
  
  
  #######################################################################################
  #######################################################################################
  #######################################################################################
  # genero dati medie mensili
  dati_medie_mensili <- reactive({
    analizzatore<-input$ui_select_analizzatore
    
    df_postgres<-dati_by_postgres()
    medie_mensili <- aggregate(df_postgres[,2], format(df_postgres["data_ora"],"%m"),  mean, na.rm = TRUE)
    
    medie_mensili
  })  
  
  
  #######################################################################################
  # genero dati per download medie mensili
  
  output$download_dati_mensili.csv <- downloadHandler(
    
    filename = function() { 
      paste(input$dataset, '.csv', sep='') 
    },
    content = function(file) {
      write.csv(dati_medie_mensili(), file)
    }
  )
  #######################################################################################
  
  
  
  
  
  #######################################################################################
  # pubblico tabella dati giorno 24h tipo
  output$tabella_medie_mensili <-DT::renderDataTable({
    
    #genero tabella dati medie_mensili
    DT::datatable(dati_medie_mensili (),
                  options = list(
                    pageLength = 24
                  )
    )
  })
  
  #######################################################################################
  #######################################################################################
  #######################################################################################
  
  
  
  
  #######################################################################################
  # genero titolo del grafico parziale base alle scelte delle select, interrogando Postgres
  titolo_grafico_parziale <- reactive({
    
    netcd   <-input$ui_select_rete
    statcd  <-input$ui_select_stazione
    ptid    <-input$ui_select_analizzatore
    
    data1<-paste(input$date_oss[1])
    data2<-paste(input$date_oss[2])
    
    # nome stazione 
    sql_txt=paste("select statnm from testat where netcd=",netcd," and statcd=",statcd)
    str_stazione <- query_postgres(sql_txt)
    
    
    data_range<-paste("dal ",data1," al ",data2)
    titolo_parziale =paste("Stazione ",str_stazione , " ",cur_nome_analizzatore(), " ",data_range)
    titolo_parziale[1]
    
  })  
  
  #######################################################################################
  
  
  
  
  
  #######################################################################################
  # genero stringa nome analizzatore correntemente sleezionato
  cur_nome_analizzatore <- reactive({
    
    # stop finche' non sono istanziate tutte le coordinate
    req(input$ui_select_rete,input$ui_select_stazione,input$ui_select_analizzatore)
    
    netcd   <-input$ui_select_rete
    statcd  <-input$ui_select_stazione
    ptid    <-input$ui_select_analizzatore
    
    
    # nome analizzatore
    sql_txt=paste("select ana1,ana2 from teanapt where netcd=",netcd," and statcd=",statcd," and anaptid=",ptid)
    
    
    ds <- query_postgres(sql_txt)
    
    str_analizzatore=paste(ds$ana1)
    
    str_analizzatore
    
  })  
  
  #######################################################################################
  
  
  
  
  
  
  
  
 
  
  
  
  #######################################################################################
  # genero dinamicamente unit di misura
  react_u_misura<- reactive({
    analizzatore<-input$ui_select_analizzatore
    
    if(analizzatore=="CO")
      u_misura='mg/mc'
    else
      u_misura='microg/mc'
    
    u_misura
    
  })  
  #######################################################################################
  
  
  
  
  #######################################################################################
  #click pulsante download dataset g_sett_tipo 
  
  output$downloadData_g_sett_tipo<- downloadHandler(
    filename ='giorno_settimanale_tipo.csv' ,
    content = function(file) {
      write.csv(dati_giorno_settimanale_tipo(), file)
    }
  )
  #######################################################################################
  
  
  #######################################################################################
  #click pulsante download dataset g_sett_tipo 
  
  output$downloadData_g_24h_tipo<- downloadHandler(
    filename = 'giorno_24h_tipo.csv' ,
    content = function(file) {
      write.csv(dati_giorno_settimanale_tipo(), file)
    }
  )
  #######################################################################################
  
  #######################################################################################
  #click pulsante download dataset analizzatore
  
  output$download_Dataset<- downloadHandler(
    filename = 'dataset_analizzatore.csv' ,
    content = function(file) {
      dati<-dati_by_postgres()
      # sostituisco NA con blank
      dati[is.na(dati)] <- " "
      
      dati_out<-data.frame(dati)
      write.csv(dati, file)
    }
  )
  #######################################################################################
  
  
  
  
  #######################################################################################
  # click su pulsante button_dati_sing_an, procedo con estrazione dati singolo analizzatore
  dati_by_postgres<-eventReactive(input$button_dati_sing_an, {
    
    req(input$ui_select_rete,input$ui_select_stazione,input$ui_select_analizzatore)
    
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Elaborazione in corso...", value = 0)
    
    
    data1=paste(input$date_oss[1])
    data2=paste(input$date_oss[2])
    
    
    netcd   <-input$ui_select_rete
    statcd  <-input$ui_select_stazione
    ptid    <-input$ui_select_analizzatore
    
    
    df<-get_serie_dati_bypost(data1,data2,netcd,statcd,ptid,'orari')
    
    
    # controllo se devo eliminare i valori maggiori di...
    if(input$filtra_maggiore_di==TRUE){
      # elimino valori maggiori di ....
      limite= input$valore_maggiore_di
      
      # applico a tutte le colonne eccetto la prima che contiene la data
      numero_colonne<-ncol(df)
      
      #salvo colonna data
      colonna_data<-df$data_ora
      
      # elimino dati maggiori del limite
      df[df <limite]<-NA
      
      #aggiungo colonna date
      df$data_ora<-colonna_data
      
    }
    
    progress$inc(1, detail = "Dataset pronto!")
    
    df
    
  
  })#
  #######################################################################################
  
  
  
  #######################################################################################
  # click su pulsante button_dati_sing_an, procedo con estrazione dati singolo analizzatore
  dati_by_postgres_giornalieri<-eventReactive(input$button_dati_sing_an, {
    
    req(input$ui_select_rete,input$ui_select_stazione,input$ui_select_analizzatore)
    
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Elaborazione in corso...", value = 0)
    
    
    data1=paste(input$date_oss[1])
    data2=paste(input$date_oss[2])
    
    
    netcd   <-input$ui_select_rete
    statcd  <-input$ui_select_stazione
    ptid    <-input$ui_select_analizzatore
    
    
    df<-get_serie_dati_bypost(data1,data2,netcd,statcd,ptid,'giornalieri')
    
    progress$inc(1, detail = "Dataset pronto!")
    
    df
    
    
  })#
  #######################################################################################
  
  
  
  
  #######################################################################################
  # genero grafico timevariation su singolo inquinante
  
  output$plot_si_timevariation <- renderPlot({
    
    matrice_dati<-dati_by_postgres()
    
    titolo_grafico<-paste(titolo_grafico_parziale()," - Analisi Variazione nel tempo statistica")
    
    analizzatore<-cur_nome_analizzatore()
    
    u_misura<-react_u_misura()
    
    a_inquinanti=names(matrice_dati)
    
    a_inquinanti=a_inquinanti[-1]
    
    #rinomino prima colonna..
    colnames(matrice_dati)[1]<-'date'
    
    timeVariation(matrice_dati,a_inquinanti,  col = "blue", normalize=FALSE)
    #timeVariation(matrice_dati_stazione,a_inquinanti)
    
    
  })
  #######################################################################################
  
  
  
  
  
  
  
  #######################################################################################
  # genero grafico timevariation statistica su singolo inquinante
  
  output$plot_si_timevariation_stat <- renderPlot({
    
    matrice_dati<-dati_by_postgres()
    
    titolo_grafico<-paste(titolo_grafico_parziale()," - Analisi Variazione nel tempo statistica")
    
    analizzatore<-cur_nome_analizzatore()
    
    u_misura<-react_u_misura()
    
    a_inquinanti=names(matrice_dati)
    
    a_inquinanti=a_inquinanti[-1]
    
    #rinomino prima colonna..
    colnames(matrice_dati)[1]<-'date'
    
    timeVariation(matrice_dati,a_inquinanti, statistic = "median", col = "firebrick")
    #timeVariation(matrice_dati_stazione,a_inquinanti)
    
    
  })
  #######################################################################################
  
  
  #######################################################################################
  # genero grafico dati orari
  
  output$plot_medieorarie <- renderPlot({
    
    df_postgres<-dati_by_postgres()
    
    titolo_grafico<-paste(titolo_grafico_parziale()," - Medie Orarie")
    
    analizzatore<-cur_nome_analizzatore()
    
    u_misura<-react_u_misura()
    
    
    #cat("\n----------------------") 
    #cat(as.character(df_postgres$data_ora), "\n")
    #cat("\n") 
    #cat(as.character(df_postgres[,2]), "\n")
    #cat("\n----------------------") 
    
    #preparo il corpo del grafico..
    plot(df_postgres$data_ora,
         df_postgres[,2], type="l", 
         xlab = "Data", 
         ylab = paste(analizzatore," (",u_misura,")"),  
         main=titolo_grafico, 
         col="blue"
    )
    
    grid()
    
    
  })
  #######################################################################################
  
  
  
  #######################################################################################
  # genero grafico dati GIORNALIERI
  
  output$plot_mediegiornaliere <- renderPlot({
    
    df_postgres<-dati_by_postgres_giornalieri()
    
    titolo_grafico<-paste(titolo_grafico_parziale()," - Medie Giornaliere")
    
    analizzatore<-cur_nome_analizzatore()
    
    u_misura<-react_u_misura()
    
    
    #cat("\n----------------------") 
    #cat(as.character(df_postgres$data_ora), "\n")
    #cat("\n") 
    #cat(as.character(df_postgres[,2]), "\n")
    #cat("\n----------------------") 
    
    #preparo il corpo del grafico..
    plot(df_postgres$data_ora,
         df_postgres[,2], type="l", 
         xlab = "Data", 
         ylab = paste(analizzatore," (",u_misura,")"),  
         main=titolo_grafico, 
         col="blue"
    )
    
    grid()
    
    
  })
  #######################################################################################
  
  
  #######################################################################################
  # genero istogramma distribuzione delle frequenze
  output$plot_istogramma <- renderPlot({
    
    dati=dati_by_postgres()
    
    valori=dati[,2]
    
    titolo_grafico<-paste(titolo_grafico_parziale()," - Istogramma frequenze di valori")
    
    hist(valori,
         
         xlab="Conc. in microg./mc", 
         border="blue", 
         col="green",
         las=1,
         main=titolo_grafico
    )
    
  })
  #######################################################################################
  
  
  #######################################################################################
  # genero grafico Giorno settimanale tipo
  
  output$plot_giornotipo <- renderPlot({
    
    analizzatore<-cur_nome_analizzatore()
    
    u_misura<-react_u_misura()
    
    medie_gtipo <- dati_giorno_settimanale_tipo()
    titolo_grafico<-paste(titolo_grafico_parziale()," - Giorno settimanale tipo")
    
    #preparo il corpo del grafico..
    plot(medie_gtipo$x, xaxt = "n", type = "n", xlab = "Giorno settimanale"
         ,ylab = paste(analizzatore,"  (",u_misura,")")
         ,   main=titolo_grafico
    )
    
    # add segni dei giorni su asse x
    axis(1, at = seq(1, 169, 24), labels = FALSE)
    
    # preparo vettore con le stringhe dei giorni...
    days = c("Dom", "Lun", "Mar", "Mer", "Gio", "Ven", "Sab")
    
    loc.days = seq(13, 157, 24) # location of labels on x-axis
    
    #Scrivo i giorni su asse x
    mtext(days, side = 1, line = 1, at = loc.days)
    
    # add some grid lines verticali
    abline(v = seq(1, 169, 24), col = "grey85")
    
    # plotto la media
    lines(medie_gtipo$x, col = "darkorange2", lwd = 2)
    
    
  })
  #######################################################################################
  
  
  #######################################################################################
  # genero grafico giorno 24h tipo
  
  output$plot_giorno_24h_tipo <- renderPlot({
    
    medie_gtipo=dati_giorno_24h_tipo()
    
    u_misura<-react_u_misura()
    analizzatore<-cur_nome_analizzatore()
    
    titolo_grafico<-paste(titolo_grafico_parziale()," - Giorno tipo")
    
    #preparo il corpo del grafico..
    plot(medie_gtipo$x, xaxt = "n", type = "n", xlab = "Giorno tipo"
         ,ylab = paste(analizzatore," (",u_misura,")")
         ,   main=titolo_grafico,
         panel.first=grid()
    )
    
    # add segni dei giorni su asse x
    axis(1, at = seq(1, 169, 24), labels = FALSE)
    
    # preparo vettore con le stringhe dei giorni...
    days = c("0","1", "2", "3", "4", "5", "6", "7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23")
    
    loc.days = seq(1, 24, 1) # location of labels on x-axis
    
    #Scrivo i giorni su asse x
    mtext(days, side = 1, line = 1, at = loc.days)
    
    #griglia automatica
    #grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
    
    # griglia verticale
    abline(v = seq(1, 24, 1), col = "grey85")
    
    
    # plotto la media
    lines(medie_gtipo$x, col = "darkorange2", lwd = 2)
    
    
  })
  #######################################################################################
  
  
  
  
  #######################################################################################
  # genero grafico medie mensili
  
  output$plot_medie_mensili <- renderPlot({
    
    medie_mensili=dati_medie_mensili()
    
    u_misura<-react_u_misura()
    analizzatore<-cur_nome_analizzatore()
    
    titolo_grafico<-paste(titolo_grafico_parziale()," - Medie mensili")
    
    #preparo il corpo del grafico..
    plot(medie_mensili$x, xaxt = "n", type = "n", xlab = "Medie mensili"
         ,ylab = paste(analizzatore," (",u_misura,")")
         ,   main=titolo_grafico,
         panel.first=grid()
    )
    
    
    
    
    # add segni dei giorni su asse x
    axis(1, at = seq(1, 169, 24), labels = FALSE)
    
    # preparo vettore con le stringhe dei giorni...
    days = c("Gen","Feb", "Mar", "Apr", "Mag", "Giu", "Lug", "Ago","Set","Ott","Nov","Dic")
    
    loc.days = seq(1, 12, 1) # location of labels on x-axis
    
    #Scrivo i giorni su asse x
    mtext(days, side = 1, line = 1, at = loc.days)
    
    #griglia automatica
    #grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
    
    # griglia verticale
    abline(v = seq(1, 12, 1), col = "grey85")
    
    
    # plotto la media
    lines(medie_mensili$x, col = "green", lwd = 2)
    
    
  })
  #######################################################################################
  
  
  
  #######################################################################################
  # grafico scatter NOx- NO
  
  output$plot_nox_no_scatter <- renderPlot({
    
    req(input$nox_ui_select_rete, input$nox_ui_select_stazione)
    dati<-react_dati_analisi_nox()
    
    if(nrow(dati) >0){
      
      data1<-paste(input$date_oss[1])
      data2<-paste(input$date_oss[2])
      
      nome_serie1=names(dati[2])
      nome_serie2=names(dati[3])
      
      titolo_grafico=paste("Correlazione tra " , nome_serie1 , " e ", nome_serie2 , " dal ", data1, " al ",data2)
    
      dati_x<-dati[,2]
      dati_y<-dati[,3]
     
      
      plot(dati_x, dati_y,  xlab= nome_serie1, ylab=nome_serie2,  col= "blue", border = "grey", xbin = 15, main=titolo_grafico)
      grid()
      
    }  else {
      
      plot(1, type="n", axes=F, xlab="", ylab="",  main="Dati non presenti nel DB Postgres")
    }
    
  })
  #######################################################################################
  
  #######################################################################################
  # grafico scatter NO- NO2
  
  output$plot_no_no2_scatter <- renderPlot({
    
    
    req(input$nox_ui_select_rete, input$nox_ui_select_stazione)
    dati<-react_dati_analisi_nox()
    
    if(nrow(dati)>0){
      
      
        data1<-paste(input$date_oss[1])
        data2<-paste(input$date_oss[2])
        
        nome_serie1=names(dati[3])
        nome_serie2=names(dati[4])
        
        titolo_grafico=paste("Correlazione tra " , nome_serie1 , " e ", nome_serie2 , " dal ", data1, " al ",data2)
        
    
        dati_x<-dati[,3]
        dati_y<-dati[,4]
        
        
        plot(dati_x, dati_y,  xlab= nome_serie1, ylab=nome_serie2,  col= "red", border = "grey", xbin = 15, main=titolo_grafico)
        grid()
    }else{
      plot(1, type="n", axes=F, xlab="", ylab="",  main="Dati non presenti nel DB Postgres")
      
    }    
    
    
  })
  #######################################################################################
  
  
  #######################################################################################
  # grafico scatter NOx - NO2
  output$plot_nox_no2_scatter <- renderPlot({
    
    req(input$nox_ui_select_rete, input$nox_ui_select_stazione)
    dati<-react_dati_analisi_nox()
    
    if(nrow(dati)>0){
      
        data1<-paste(input$date_oss[1])
        data2<-paste(input$date_oss[2])
        
        nome_serie1<-names(dati[2])
        nome_serie2<-names(dati[4])
        
        titolo_grafico=paste("Correlazione tra " , nome_serie1 , " e ", nome_serie2 , " dal ", data1, " al ",data2)
        
      
        dati_x<-dati[,2]
        dati_y<-dati[,4]
        
        
        plot(dati_x, dati_y,  xlab= nome_serie1, ylab=nome_serie2,  col= "green", border = "grey", xbin = 15, main=titolo_grafico)
        grid()
    }else{
      plot(1, type="n", axes=F, xlab="", ylab="",  main="Dati non presenti nel DB Postgres")
    }  
    
  })
  #######################################################################################
  
  
  #######################################################################################
  output$plot_nox_diff <- renderPlot({
    
    req(input$nox_ui_select_rete, input$nox_ui_select_stazione)
    
    dati<-react_dati_analisi_nox()
    if(nrow(dati)>0){
      
      data1<-paste(input$date_oss[1])
      data2<-paste(input$date_oss[2])
      
      
      nome_serie2<-names(dati[5])
      
      titolo_grafico=paste("Differenze nella tripla NOX=NO+NO2  dal ", data1, " al ", data2)
      
    
      dati_x<-dati[,1]
      dati_y<-dati[,5]
      
      plot(dati_x, dati_y , xlab="Data", ylab=nome_serie2, main=titolo_grafico)
      grid()
      
    }else{
      plot(1, type="n", axes=F, xlab="", ylab="",  main="Dati non presenti nel DB Postgres")
    }   
    
  })
  #######################################################################################
  
  
  #######################################################################################
  # download dati NOx
  output$nox_download_Dataset.csv<- downloadHandler(
    
    filename = function() { 
      paste(input$nox_ui_select_stazione, '_NOX.csv', sep='') 
    },
    content = function(file) {
      write.csv(react_dati_analisi_nox(), file)
    }
  )  
    #######################################################################################
  
  
  
  
  
  
  #######################################################################################
  # Pagina CORRELAZIONI
  #######################################################################################
  
  
  
    #########################################################################################
    # list dinamica scelta stazione per analisi CORRELAZIONI
    output$ui_cor_select_stazione <- renderUI({
      
      req(input$cor_ui_select_rete)
      
      netcd<-input$cor_ui_select_rete
      
      check_in_scansione=TRUE
      
      lista_staz<-list_select_stazione(netcd,check_in_scansione)
      
      selectInput("ui_cor_select_stazione", "Scelta stazione", choices = lista_staz)
      
    })
    
    
  
  
  #########################################################################################
  # list dinamica scelta analizzatore per analisi CORRELAZIONI
  output$ui_cor_select_analizzatore <- renderUI({
    
    req(input$cor_ui_select_rete,input$ui_cor_select_stazione)
    
    netcd<-input$cor_ui_select_rete
    statcd<-input$ui_cor_select_stazione
    
    lista_analiz<-list_select_analizzatore(netcd,statcd)
    
    selectInput("ui_cor_select_analizzatore", "Scelta Analizzatore", choices = lista_analiz)
    
    
  })
  #######################################################################################

  
  
  
  #######################################################################################
  # click sl pulsante 'Estrazione Dati' nella pagina Analisi correlazione
  
  ac_dati_orari<-eventReactive(input$button_estrai_correlazione, {
    
    req(input$cor_coord_analiz_1,input$cor_coord_analiz_2)
    
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Elaborazione in corso...", value = 0)
   
    data1=paste(input$date_oss[1])
    data2=paste(input$date_oss[2])
   
   
     # estraggo serie x....
    list_coord=unlist(strsplit(input$cor_coord_analiz_1, split="_"))
    
    netcd=list_coord[1]
    statcd=list_coord[2]
    ptid=list_coord[3]
    
    data_x<-get_serie_dati_bypost(data1,data2,netcd,statcd,ptid,'orari')
    
    
    # estraggo serie y....
    list_coord=unlist(strsplit(input$cor_coord_analiz_2, split="_"))
    
    netcd=list_coord[1]
    statcd=list_coord[2]
    ptid=list_coord[3]
    
    data_y<-get_serie_dati_bypost(data1,data2,netcd,statcd,ptid,'orari')
    
    #unisco i dataset...
    df_out <- merge(data_x, data_y, by='data_ora', all=TRUE)
    
    progress$inc(1, detail = "Dataset pronto!")
    
    df_out
    
  })#
  #######################################################################################
  
  
  
  #######################################################################################
  # GRAFICO ANALISI CORRELAZIONE genero grafico dati orari
  
  output$ac_plot_medieorarie <- renderPlot({
    
    df_ac<-ac_dati_orari()
    
    titolo_grafico=ac_titolo_grafico_parziale()
    
    
    u_misura<-'microg/mc'
    
    test_data=df_ac
    
    
    ggplot(test_data, aes(test_data[,1])) + 
      geom_line(aes(y = test_data[,2], colour = names(test_data[2]))) + 
      geom_line(aes(y = test_data[,3], colour = names(test_data[3]))) +
      theme(legend.title=element_blank()) +
      ggtitle(titolo_grafico)+
      ylab("microg/m3")+ 
      xlab("Data")  
    
    
    
  })
  #######################################################################################
  
  
  
  #######################################################################################
  # grafico scatter ANALISI CORRELAZIONE MEDIE ORARIE
  output$ac_plot_scatter_medieorarie <- renderPlot({
    
    
    titolo_grafico=ac_titolo_grafico_parziale()
    
    
    dati<-ac_dati_orari()
    
    
    if(nrow(dati)>0){
      
      write.csv(dati,file = "ac_dati_orari_scatter.csv")
      
      dati_x<-dati[,2]
      dati_y<-dati[,3]
      
      label_x<-names(dati[2])
      label_y<-names(dati[3])
      
      
      ggplot(dati, aes(x=dati_x, y=dati_y)) +
        geom_point(shape=1) +    # Use hollow circles
        geom_smooth(method=lm) +      # Add linear regression line 
        ggtitle(titolo_grafico)+
        xlab(label_x)+ 
        ylab(label_y)
      
      
      
    }else{
      plot(1, type="n", axes=F, xlab="", ylab="",  main="Dati non presenti nel DB Postgres")
    }  
    
  })
  #######################################################################################
  
  
  #######################################################################################
  output$coefficienti_dati_orari<- renderText({
    # calcola la regressione linerare visualizzata nel grafico ac_plot_scatter, pubblica i coefficienti
    dati<-ac_dati_orari()
    dati_x<-dati[,2]
    dati_y<-dati[,3]
    reg<-lm(dati_y ~ dati_x,data=dati)
    
    paste("Coefficienti: ",reg$coefficients)
    
  })
  
  #######################################################################################
  
  
  
  
  #######################################################################################
  # genero titolo del grafico parziale base alle scelte delle select, interrogando Postgres
  ac_titolo_grafico_parziale <- reactive({
    
    req(input$cor_coord_analiz_1,input$cor_coord_analiz_2)
    
    data1<-paste(input$date_oss[1])
    data2<-paste(input$date_oss[2])
    
    
    list_coord=unlist(strsplit(input$cor_coord_analiz_1, split="_"))
    
    netcd1=list_coord[1]
    statcd1=list_coord[2]
    ptid1=list_coord[3]
    
    # nome stazione 1
    sql_txt=paste("select statnm from testat where netcd=",netcd1," and statcd=",statcd1)
    str_stazione1 <- query_postgres(sql_txt)
    
    
    
    
    list_coord=unlist(strsplit(input$cor_coord_analiz_2, split="_"))
    
    netcd2=list_coord[1]
    statcd2=list_coord[2]
    ptid2=list_coord[3]
    
    
    # nome stazione 2
    sql_txt=paste("select statnm from testat where netcd=",netcd2," and statcd=",statcd2)
    str_stazione2 <- query_postgres(sql_txt)
    
    
    nome_analizzatore_1=ac_cur_nome_analizzatore(netcd1,statcd1,ptid1)
    nome_analizzatore_2=ac_cur_nome_analizzatore(netcd2,statcd2,ptid2)
    
    titolo_parziale =paste("Analisi correlazione tra ",nome_analizzatore_1 , " ", str_stazione1 , " e ",nome_analizzatore_2 , " ", str_stazione2, " - Dal ",data1, " al ",data2 )
    
    
    titolo_parziale[1]
  })  
  
  #######################################################################################
  
  
  
  
  
  
  
  
  #######################################################################################
  # calcolo correlazione Pearson ORARIE
  
  output$ac_pearson_orari <-  renderText({
    
    dati<-ac_dati_orari()
    
    cor(dati[,2],dati[,3], method='pearson', use="complete.obs")
    
  })
  #######################################################################################
  
  
  #######################################################################################
  # calcolo correlazione kendall ORARIE
  
  output$ac_kendall_orari <-  renderText({
    
    dati<-ac_dati_orari()
    
    cor(dati[,2],dati[,3], method='kendall', use="complete.obs")
    
  })
  #######################################################################################
  
  
  
  
  
  
  
  
  #######################################################################################
  #######################################################################################
  # questo serve a cambiare dinamicamente lato server il contenuto delle text input
  # cor_coord_analiz_1 e cor_coord_analiz_2
  # in funzione della scelta corrente sulla radio radio_corr_analizzatore viene 
  # aggiornato la text box corrispondente con netcd-statcd-ptid
  
  observe({
    
    req(input$cor_ui_select_rete,input$ui_cor_select_stazione,input$ui_cor_select_analizzatore)
    
    netcd<-input$cor_ui_select_rete
    statcd<-input$ui_cor_select_stazione
    ptid<-input$ui_cor_select_analizzatore
    
    cur_analiz=input$radio_corr_analizzatore
    coordinate=paste(netcd,'_',statcd,"_",ptid, sep='')
    
    if(cur_analiz==1){
       # aggiorno coordinate primo analizzatore
      updateTextInput(session, "cor_coord_analiz_1",value =coordinate)
    }else{
      updateTextInput(session, "cor_coord_analiz_2",value =coordinate)
    }
    
    # reset grafici
    #output$ac_plot_medieorarie <-NA
    #output$ac_plot_scatter_medieorarie <-NA
    
  })  
  
  #######################################################################################
  #######################################################################################
  
  
  
  #########################################################################################################
  # FINE Pagina CORRELAZIONI
  #########################################################################################################
  
  
  
  
  
  
  
  
  
  
  #########################################################################################################
  # Pagina PROVENIENZA
  #########################################################################################################
  
  output$prov_ui_origine_dvvv <- renderUI({
    # Rende dinamicamente al gestore di interfaccia il formato di input vv-dv scelto dall'utente
    
    if (is.null(input$prov_select_input_vvdv))
      return()
    
      if(input$prov_select_input_vvdv=="Seleziona un copia di sensori DV-VV della rete"){
        
        ########################
        # Lista stazioni con sensori DV-VV attivi
        
        sql_txt="
        SELECT DISTINCT ON (TESTAT.STATNM)  UPPER(TEANAPT.ANA1), TESTAT.STATNM, TEANAPT.NETCD, TEANAPT.STATCD, TEANAPT.PARAMCD
        FROM TEANAPT,TESTAT
        WHERE TEANAPT.NETCD=TESTAT.NETCD
        AND TEANAPT.STATCD=TESTAT.STATCD
        AND TESTAT.STATSTCD=1
        AND TEANAPT.ANA999=1
        AND (TEANAPT.PARAMCD=12 OR TEANAPT.PARAMCD=13)
        
        ORDER BY TESTAT.STATNM
        
        "
        
        cat(paste('Lista stazioni con DV-VV Attivi sql=',sql_txt))
        
        #esegue query
        ds_elenco_analiz <- query_postgres(sql_txt)
        
        # trasformo in lista
        
        lista_stazioni_dvvv <- split(paste(ds_elenco_analiz$netcd,"_",ds_elenco_analiz$statcd,sep=''),ds_elenco_analiz$statnm)
        
        #######################
        
        
          
        input_vvdv<- wellPanel(
          selectInput("prov_stazioni_dvvv_attivo", "Stazioni con sensori DV-VV", choices = lista_stazioni_dvvv)
          , verbatimTextOutput("prov_coord_dvvv")
        )
                
        
      }else{
          input_vvdv<-fileInput('file1', 'Choose CSV File', accept=c('text/csv',  'text/comma-separated-values,text/plain',  '.csv'))
      }
    
     input_vvdv  
     
  })  
  ############################
  
  
  
  #######################################################################################
  output$prov_coord_analiz<- renderText({
    # aggiorno con le coordinate dell'analizzatore scelto
    
    req(input$prov_select_analiz)
    
    cur_misura<- input$prov_select_analiz
    
    list_misura=unlist(strsplit(cur_misura, split="_"))
    
    cur_analiz        =list_misura[1]
    cur_nome_stazione =list_misura[2]
    
    # determino coordinate...
    
    sql_txt=paste("
    SELECT    TEANAPT.NETCD, TEANAPT.STATCD, TEANAPT.ANAPTID, TEANAPT.PARAMCD
    FROM TEANAPT,TESTAT
    WHERE TEANAPT.NETCD=TESTAT.NETCD
    AND TEANAPT.STATCD=TESTAT.STATCD
    AND TESTAT.STATNM='",cur_nome_stazione,"'
    AND UPPER(TEANAPT.ANA1)='",cur_analiz,"'"
    ,sep='')
    
    cat(paste('aggiorno con le coordinate dell analizzatore scelto sql=',sql_txt))
    
    #esegue query
    ds_analiz <- query_postgres(sql_txt)
    coord_analiz=paste(ds_analiz$netcd,"_",ds_analiz$statcd,"_",ds_analiz$anaptid,sep='')
    coord_analiz
    
  })
  #######################################################################################
  
  
  output$prov_coord_dvvv<- renderText({
    # aggiorno con le coordinate dei sensori VV-DV scelti
    
    req(input$prov_stazioni_dvvv_attivo)
    
    cur_staz_dvvv<- input$prov_stazioni_dvvv_attivo
    
    
    list_coord=unlist(strsplit(cur_staz_dvvv, split="_"))
    
    cur_netcd         =list_coord[1]
    cur_statcd        =list_coord[2]
    
    # determino coordinate degli analizzatori VV e DV presenti nella stazione cur_staz_dvvv
    
    sql_txt=paste("
                  SELECT   TEANAPT.ANA1, TEANAPT.NETCD, TEANAPT.STATCD, TEANAPT.ANAPTID, TEANAPT.PARAMCD
                  FROM TEANAPT,TESTAT
                  WHERE TEANAPT.NETCD=TESTAT.NETCD
                  AND TEANAPT.STATCD=TESTAT.STATCD
                  AND TESTAT.NETCD='",cur_netcd,"'
                  AND TESTAT.STATCD='",cur_statcd,"'
                  AND TESTAT.STATSTCD=1
                  AND ( TEANAPT.PARAMCD=12 OR TEANAPT.PARAMCD=13)
                  ORDER BY  TEANAPT.PARAMCD"
                  ,sep='')
    
    cat(paste('\n aggiorno con le coordinate dei sensori VV-DV scelti sql=',sql_txt))
    
    #esegue query, ottengo due record uno der VV e uno per DV
    ds_analiz <- query_postgres(sql_txt)
    
    lista_netcd =ds_analiz$netcd
    lista_statcd=ds_analiz$statcd
    lista_ptid  =ds_analiz$anaptid
    
    # formato coordinate: netcd_statcd_ptid-vv_ptid-dv
    coordinate_vvdv=paste(toString(lista_netcd[1]),"_",toString(lista_statcd[1]),"_",toString(lista_ptid[1]),"_",toString(lista_ptid[2]),sep='')
    
    rito=toString(coordinate_vvdv)
    
    rito="ciao"
    
    rito
    
  })
  #######################################################################################
  
  
  
  
  
  #######################################################################################
  output$prov_tabella_dati<-DT::renderDataTable({
    
    prov_dati_orari<-prov_dati_orari()
    prov_dati_orari
    
  })
  #######################################################################################
  
  
  
  
  
  #######################################################################################
  # click sul pulsante 'Estrazione Dati' nella pagina Provenienza
  
  prov_dati_orari<-eventReactive(input$prov_button_estrai_dati, {
    
    req(input$prov_stazioni_dvvv_attivo)
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Elaborazione in corso...", value = 0)
    
    data1=paste(input$date_oss[1])
    data2=paste(input$date_oss[2])
    
    
    # estraggo dati analizzatore scelto dalla select prov_select_analiz
    cat(paste("prov_select_analiz=",input$prov_stazioni_dvvv_attivo))
    
 
    
    str_ccord=input$prov_stazioni_dvvv_attivo
    
    list_coord=unlist(strsplit(str_ccord, split="_"))
    
    netcd=list_coord[1]
    statcd=list_coord[2]
    ptid=list_coord[3]
    
    tipo_dati='orari'
    ds_analizzatore=get_serie_dati_bypost(data1,data2,netcd,statcd,ptid,tipo_dati)
    
    ds_analizzatore
    
    
    
    #data_x<-get_serie_dati_bypost(data1,data2,netcd,statcd,ptid,'orari')
    
    
    # estraggo serie y....
    #list_coord=unlist(strsplit(input$cor_coord_analiz_2, split="_"))
    
    #netcd=list_coord[1]
    #statcd=list_coord[2]
    #ptid=list_coord[3]
    
    #data_y<-get_serie_dati_bypost(data1,data2,netcd,statcd,ptid,'orari')
    
    #unisco i dataset...
    #df_out <- merge(data_x, data_y, by='data_ora', all=TRUE)
    
    #progress$inc(1, detail = "Dataset pronto!")
    
    #df_out
    
  })#
  #######################################################################################
  
  
  
  
  
  
  #########################################################################################################
  # FINE Pagina PROVENIENZA
  #########################################################################################################
  
  
  
  
  
  
  
  
}#######################################################################################

