# FUNZIONI CHE EFFETTUATO QUERY SU POSTGRES

#########################################################################################
#########################################################################################


#####
#funzioni non reactive trasferite su funzioni_db.R


#funzioni non reactive trasferite su funzioni_db.R

get_serie_dati_bypost <- function(data1,data2,netcd,statcd,ptid,tipo_dati) {
  
  # corretto giorno 0-23 (16-10-2017)
  # accetta coordinate di una analizzatore, rende la serie di dati ordinata validi con 
  # un campo dato_ora e un campo <analizzatore_nomestazione>
  
  
  
  #preparo date per formato daydt
  
  daydt_start <-paste(substr(data1,0,4),substr(data1,6,7),substr(data1,9,10),"0100",sep="")
  daydt_stop <-paste(substr(data2,0,4),substr(data2,6,7),substr(data2,9,10),"2400",sep="")
  
  
  
  # nome stazione 
  sql_txt=paste("select statnm from testat where netcd=",netcd," and statcd=",statcd)
  obj_stazione <- query_postgres(sql_txt)
  str_stazione <- obj_stazione$statnm
  
  # nome_analizzatore
  sql_txt=paste("select ana1 from teanapt  where netcd=",netcd,"and statcd=",statcd,"and anaptid=",ptid)
  obj_analizzatore <- query_postgres(sql_txt)
  str_analizzatore <-obj_analizzatore$ana1
  
  str_campo_valori=paste(str_analizzatore,"_",str_stazione,sep="")
  
  k=get_k_conversione(netcd,statcd,ptid)
  
  
  
  if(tipo_dati=='orari'){
    sql_txt=paste(" select substr(daydt,0,9)  || ' ' || to_number(substr(daydt,9,2),'99')-1 as data_ora ,  
                  hourav * ",k," as \"",str_campo_valori,"\" from tdhour where netcd=",netcd, 
                  " AND statcd=",statcd,
                  " AND ptid=",ptid,
                  " AND istanzacd=1",
                  " AND daydt >='",daydt_start,"'",
                  " AND daydt <='",daydt_stop,"'",
                  " AND (vflagcd=9 or vflagcd=10 or vflagcd=11)",
                  " ORDER BY daydt", sep=""
    )  
  }else{
    sql_txt=paste(" select substr(daydt,0,9)  || ' ' || to_number(substr(daydt,9,2),'99')-1 as data_ora ,  
                  dayav * ",k," as \"",str_campo_valori,"\" from tdday where netcd=",netcd, 
                  " AND statcd=",statcd,
                  " AND ptid=",ptid,
                  " AND istanzacd=1",
                  " AND daydt >='",daydt_start,"'",
                  " AND daydt <='",daydt_stop,"'",
                  " AND (vflagcd=9 or vflagcd=10 or vflagcd=11)",
                  " ORDER BY daydt", sep=""
    )
    
  }
  
   cat(paste('Funzione get_serie_dati_bypost sql=', sql_txt))
  
  #esegue query
  ds <- query_postgres(sql_txt)
  
  
  if(nrow(ds)>0){
    
    
    if(tipo_dati=='orari'){
  
          #modifico il dato ora in fomrato dataora di R
          # formato da sql='28/11/2016 01:00'
          
          ds$data_ora <- as.POSIXct(ds$data_ora,format="%Y%m%d %H",  tz = 'UTC')
          
          str_data1<-paste(data1,' 00')
          str_data2<-paste(data2,' 23')
          
          # cat('\n str_data1=',str_data1, ' str_data2=',str_data2)
          
          # creo sequenza
          r_data1<-as.POSIXct(str_data1,format="%Y-%m-%d %H" , tz = 'UTC')
          r_data2<-as.POSIXct(str_data2,format="%Y-%m-%d %H" , tz = 'UTC')
          
          #creo sequenza
          sequenza <- seq(r_data1, r_data2, by="hour",  tz = 'UTC')
          
          #creo data frame con campo data_ora
          temp_data_frame <- data.frame(list(data_ora=sequenza))
          
          
          # dump data frame
          #cat("\n temp_data_frame ----------------------") 
          #cat(as.character(temp_data_frame$data_ora), "\n")
          #cat("\n----------------------") 
          
          
          # unisco i due df
          # ATTENZIONE! prima inserire il data set della sequenza
          df_out <- merge(temp_data_frame, ds, by='data_ora', all=TRUE)
    }else{
      
      # dati GIORNALIERI
      
      ds$data_ora <- as.POSIXct(ds$data_ora,format="%Y%m%d",  tz = 'UTC')
      
      str_data1<-paste(data1)
      str_data2<-paste(data2)
      
      # cat('\n str_data1=',str_data1, ' str_data2=',str_data2)
      
      # creo sequenza
      r_data1<-as.POSIXct(str_data1,format="%Y-%m-%d" , tz = 'UTC')
      r_data2<-as.POSIXct(str_data2,format="%Y-%m-%d" , tz = 'UTC')
      
      #creo sequenza
      sequenza <- seq(r_data1, r_data2, by="day",  tz = 'UTC')
      
      #creo data frame con campo data_ora
      temp_data_frame <- data.frame(list(data_ora=sequenza))
      
      
      # dump data frame
      #cat("\n temp_data_frame ----------------------") 
      #cat(as.character(temp_data_frame$data_ora), "\n")
      #cat("\n----------------------") 
      
      
      # unisco i due df
      # ATTENZIONE! prima inserire il data set della sequenza
      df_out <- merge(temp_data_frame, ds, by='data_ora', all=TRUE)
      
      
      
      
    }
      
      
      #cat("\n----------------------") 
      #cat(as.character(df_out), "\n")
      #cat("\n----------------------")
  }else{
    
    df_out<- data.frame()
  }    
  
  df_out
}


#########################################################################################
#########################################################################################

get_serie_dati_stazione <- function(data1,data2,netcd,statcd,tipo_dati) {
  # accetta coordinate di una analizzatore, rende la serie di dati ordinata validi con 
  # un campo dato_ora e un campo <analizzatore_nomestazione>
  
  
  # imposto intervallo date in standard R
  # dd/mm/yyyy
  
  
  #preparo date per formato daydt
  
  daydt_start <-paste(substr(data1,0,4),substr(data1,6,7),substr(data1,9,10),"0100",sep="")
  daydt_stop <-paste(substr(data2,0,4),substr(data2,6,7),substr(data2,9,10),"2400",sep="")
  
  
  
  if(tipo_dati=='orari'){
    sql_txt=paste(" select substr(daydt,0,9)  || ' ' || to_number(substr(daydt,9,2),'99')-1 as date ,  
     paramcd, hourav  from tdhour 
     where netcd=",netcd," AND statcd=",statcd, " AND istanzacd=1 
     AND daydt >='",daydt_start,"'              
     AND daydt <='",daydt_stop,"' 
     AND (vflagcd=9 or vflagcd=10 or vflagcd=11) 
     AND paramcd<63 and paramcd<>18
     ORDER BY daydt", sep="")  
  }else{
    
  }
  
  # cat("\n sql_txt:",sql_txt,"\n")
  
  #esegue query
  ds_orari_stazione <- query_postgres(sql_txt)
  
  if(nrow(ds_orari_stazione)>0){
      # pivot
      ds_orari_stazione <- cast(ds_orari_stazione, date ~ paramcd)
      #ds_orari_stazione$data_ora <- as.POSIXct(ds$data_ora,format="%Y%m%d%H")
      
    
      
      #cambio intestazione ai campi, al posto di ptid inserisco il nome
      lista_campi=names(ds_orari_stazione)
      numero_campi<-length(lista_campi)
      
      for (i in 2:numero_campi){
        cur_paramcd=lista_campi[i]
        
        #cat(paste("\n",cur_paramcd))
        
        cur_nome_analiz =get_nome_inquinante_by_paramcd(cur_paramcd)
        
        #cat(paste("\n",cur_nome_analiz))
        
        cur_ptid        =get_ptid_by_teanapt(netcd,statcd,cur_paramcd)
        
        # SE ptid=0 elimino la colonna dal data.frame
        if(cur_ptid==0){
            # elimono colonna con ptid=0..
            #ds_orari_stazione[i]<-NULL
            #cat(paste('\n Eliminata colonna ',i, ' cur_paramcd=',cur_paramcd, ' analizzatore=',cur_nome_analiz))
            #numero_campi=numero_campi-1
          
        }else{
          
          #cat(paste("\n",cur_ptid))
          
          cur_k           =get_k_conversione(netcd,statcd,cur_ptid)
          cur_decimali    =get_num_decimali_by_paramcd(cur_paramcd)
          
          #applico K
          ds_orari_stazione[,i]<-round(ds_orari_stazione[,i]*cur_k,digits= cur_decimali)
        }  
          
          #cat("\n cur_nome_analiz=",cur_nome_analiz, "cur_k=",cur_k, " \n")
          
          #cambio intestazione colonna
          colnames(ds_orari_stazione)[i]=cur_nome_analiz
        
        
      }  
      
      # conversione in formato date GMT
      ds_orari_stazione$date <- as.POSIXct(ds_orari_stazione$date,format="%Y%m%d %H")
      
      
      # creo sequenza
      r_data1<-as.POSIXct(paste(data1,' 00'),format="%Y-%m-%d %H" , tz = 'UTC')
      r_data2<-as.POSIXct(paste(data2,' 23'),format="%Y-%m-%d %H" , tz = 'UTC')
      
      
      #creo sequenza
      sequenza <- seq(r_data1, r_data2, by="hour")
      
      #creo data frame con campo data_ora
      temp_data_frame <- data.frame(list(date=sequenza))
      
      # unisco i due df
      # ATTENZIONE! prima inserire il data set della sequenza
      df_out <- merge(  temp_data_frame, ds_orari_stazione,  all=TRUE)
      
      
      #elimino dal data frame le colonne con ptid=0 DA FARE
      #df_out2 <- df_out[, !colnames(df_out) %in% dropList]
      
  }else{
    
    df_out<-data.frame()
  }  
  
  
  
  df_out
}


#########################################################################################
get_nome_inquinante <-function(netcd,statcd,ptid) {
# interroga tabella TEANAPT e rende il nome ANA1
  # mi ricavo il paramcd...
  sql=paste("select ana1 from teanapt where netcd=",netcd," and statcd=",statcd, " and anaptid=", ptid)
  
  #esegue query
  ds_ana1 <-  query_postgres(sql)
  
  ds_ana1$ana1
  
}  

#########################################################################################


#########################################################################################
get_nome_inquinante_by_paramcd <-function(paramcd) {
  # interroga tabella TEPARAM e rende il nome ANA1
  #SELECT paramdsb FROM TEPARAM where paramcd=2
  sql=paste("select paramdsb FROM TEPARAM where paramcd=",paramcd)
  
  #esegue query
  ds_ana1 <- query_postgres(sql)
  
  ds_ana1$paramdsb
  
}  

#########################################################################################





#########################################################################################
get_num_decimali_by_paramcd <-function(paramcd) {
  # in funzione del paramcd rende il numero di decimali
  if(paramcd==1) {
    #SO2
    decimali=1        
  } else if (paramcd==6){
    #CO  
    decimali=1  
  } else if  (paramcd==62) {
    #benzene  
    decimali=1  
  }  else if (paramcd==18) {
    #temp int  
    decimali=1  
  }  else if (paramcd==103){
    #volume  
    decimali=1  
  }   else if (paramcd==46) {
    #mercurio  
    decimali=1  
  }  else if (paramcd==10) {
    #h2s  
    decimali=1  
    
  } else   {
    decimali=0
  }  
  
  decimali
  
  
}  

#########################################################################################





#get_ptid_by_tdhour(con,netcd,statcd,cur_paramcd)
#########################################################################################
get_ptid_by_teanapt <-function(netcd,statcd,paramcd) {
  # interroga tabella teanapt e rende il ptid
  
  sql=paste("select anaptid from teanapt where netcd=",netcd, " and statcd=",statcd," and paramcd=",paramcd," and istanzacd=1")
  
  
  #esegue query
  ds <- query_postgres(sql)
 
  
  if(nrow(ds)==0){
    
    #msg=paste('ERRORE!!! funzione get_ptid_by_teanapt sql=',sql)
    #msgBox <- tkmessageBox(title = "ERRORE! ", message = msg, icon = "info", type = "ok")
   
    
    return=0
    
  }else{
    return=ds$anaptid  
  }
  
    return
  
}  

#########################################################################################


#########################################################################################
#########################################################################################
#######################################################################################
# rende la costate di conversione da ppb a microg/mc a 20 gradi interrogando la teunit
get_k_conversione <- function(netcd,statcd,ptid) {
  
  
  #trovo la costante di conversione per passare da ppb a microg/m3 , per default imposto a 0
  k=0
  
  # mi ricavo il paramcd...
  sql_paramcd=paste("select paramcd from teanapt where netcd=",netcd," and statcd=",statcd, " and anaptid=", ptid)
  
  
  #esegue query
  ds_paramcd <- query_postgres(sql_paramcd)
  
  if(nrow(ds_paramcd) >0){
    
    paramcd<-ds_paramcd$paramcd
    
    
    if(paramcd==25 | paramcd==39){
        k=1
        
    }else if(paramcd>=12 & paramcd<=24){    
      # meteo
      k=1
    
    }else{
        
        sql_k=paste("select unitfact from teunit where paramcd=", paramcd, "and unit1='ppb' and unit2='ug/m3 293K'")
        ds_k <-  query_postgres(sql_k)
        
        if(nrow(ds_k) >0){
          k<-ds_k$unitfact
        }
    }
    
    
  }
  
  
  k
  
}



#query Postgres
#########################################################################
query_postgres <- function (sql_txt){
  
  require("RPostgreSQL")
  drv <- dbDriver("PostgreSQL")
  
  all_cons <- dbListConnections( dbDriver("PostgreSQL") )
  for(con in all_cons) dbDisconnect(con)
  
  
  config_postgres<- read.csv('config_postgres.csv', header=T, sep=',',quote='')
  
  postG_dbname  =as.character(config_postgres$dbname)
  postG_host    =as.character(config_postgres$host)
  postG_port    =as.character(config_postgres$port)
  postG_user    =as.character(config_postgres$user)
  postG_password=as.character(config_postgres$password)
  
  
  # apro connessione
  #con <- dbConnect(drv, dbname = "ecomanager_db", host = "ecomanagerdb.arpat.toscana.it", port = 5432, user = "ecomanager", password = "ecomanager")
  con <- dbConnect(drv, dbname = postG_dbname, host = postG_host, port = postG_port, user = postG_user, password = postG_password)
  
  #esegue query
  result <- dbGetQuery(con,   sql_txt)
  
  # chiudo connessione
  dbDisconnect(con)
  
  return(result) 
  
}
#########################################################################




#########################################################################
#
list_select_stazione<- function (netcd, in_scansione){
  # accetta il codice stazione e un boolenano vero se si desidera solo le stazioni in scansione
  # torna una lista di stazioni
  
  if (in_scansione==TRUE)
    sql_txt=paste("select statcd,statnm from testat where netcd=",netcd," and statstcd=1 order by statcd")
  else
    sql_txt=paste("select statcd,statnm from testat where netcd=",netcd,"  order by statcd")
  
  #esegue query
  
  
  ds_elencostaz <- query_postgres(sql_txt)
  
  #trasformo in lista
  lista_staz <- split(ds_elencostaz$statcd,ds_elencostaz$statnm)
  lista_staz <- with(ds_elencostaz, split(statcd,statnm))
  
  lista_staz
  
}

#
#########################################################################




#########################################################################
#
list_select_analizzatore<- function (netcd, statcd){
  # accetta il codice rete e stazione rende la lista fegli analizzatori attivi
  
  
  #if (in_scansione==TRUE)
  #  sql_txt=paste("select statcd,statnm from testat where netcd=",netcd," and statstcd=1 order by statcd")
  #else
  #  sql_txt=paste("select statcd,statnm from testat where netcd=",netcd,"  order by statcd")
  
  sql_txt=paste("select anaptid,ana1 from teanapt where netcd=",netcd," and statcd=", statcd, "and ana999=1 order by anaptid")
  
  
  #esegue query
  ds_elencoanaliz <- query_postgres(sql_txt)
  
  
  #trasformo in lista
  lista_analiz <- split(ds_elencoanaliz$ana1,ds_elencoanaliz$anaptid)
  lista_analiz <- with(ds_elencoanaliz, split(anaptid,ana1))
  
  lista_analiz
  
}

#
#########################################################################


#######################################################################################
# genero stringa nome analizzatore correntemente sleezionato
ac_cur_nome_analizzatore <-  function (netcd, statcd, ptid){
  
  
  # nome analizzatore
  sql_txt=paste("select ana1,ana2 from teanapt where netcd=",netcd," and statcd=",statcd," and anaptid=",ptid)
  
  ds <- query_postgres(sql_txt)
  
  str_analizzatore=paste(ds$ana1)
  
  str_analizzatore
  
}  

#######################################################################################



#######################################################################################

reset_grafici_corr <-  function (){
  cat("reset_grafici_corr")
  
}  

#######################################################################################


