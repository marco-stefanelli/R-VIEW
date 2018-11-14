#library(markdown)


require("RPostgreSQL")
require("DT")
source("funzioni_db.R")




###################################################

# recupero elenco reti.
sql_txt="select netcd, netnm from netcd order by netnm"

#esegue query
ds_elencoreti <- query_postgres(sql_txt)

# trasformo in lista
lista_reti <- split(ds_elencoreti$netcd, ds_elencoreti$netnm)
lista_reti <- with(ds_elencoreti, split(netcd,netnm))



########################

# Lista_analizzatori IN scansione

sql_txt="
SELECT UPPER(TEANAPT.ANA1) AS ANA1, TESTAT.STATNM, TEANAPT.NETCD, TEANAPT.STATCD, TEANAPT.ANAPTID, TEANAPT.PARAMCD
FROM TEANAPT,TESTAT
WHERE TEANAPT.NETCD=TESTAT.NETCD
AND TEANAPT.STATCD=TESTAT.STATCD
AND TESTAT.STATSTCD=1
AND TEANAPT.ANA999=1
AND (TEANAPT.PARAMCD < 12 OR TEANAPT.PARAMCD > 29)
AND TEANAPT.PARAMCD<103
ORDER BY TESTAT.STATNM, TEANAPT.PARAMCD
"

cat(paste('Lista_analizzatori IN scansione sql=',sql_txt))

#esegue query
ds_elenco_analiz <- query_postgres(sql_txt)

# trasformo in lista

lista_analizzatori <- split(paste(ds_elenco_analiz$ana1,"_",ds_elenco_analiz$statnm,sep=''),ds_elenco_analiz$ana1)

#######################







navbarPage("R-View",
           
           
           tabPanel("Home",
                    
                    fluidPage( 
                     
                               
                      wellPanel(
                        img(src='logo_r-view.png', align = "left"),
                        h2(" Utility per analisi su dati di qualità dell'Aria"),
                        h3("Versione 1.91 - 9 Luglio 2018"),
                                                hr(),
                        p("Powered by Marco Stefanelli ARPAT - CRTQA"),
                        hr()
                  
                      )
                    
                 )  
                   
          ),
           
          
          
          
          tabPanel("Analisi Analizzatore",
                  
                    fluidPage(
                      #titlePanel("Dynamically generated user interface components"),
                      
                      ############### 1 riga ##############  
                      fluidRow(
                        
                        column(4, wellPanel(
                          selectInput("ui_select_rete", "Scelta rete", choices =lista_reti),
                          column(4, verbatimTextOutput("netcd"))
                          )
                        ),
                        
                        column(4, wellPanel(
                          uiOutput("ui_select_stazione"),
                          column(4, verbatimTextOutput("statcd"))
                        )),
                        
                        column(4, wellPanel(
                          # This outputs the dynamic UI component
                          uiOutput("ui_select_analizzatore"),
                          column(4, verbatimTextOutput("ptid")),
                          column(6, verbatimTextOutput("K_ppb_microg_20"))
                        ))
                        
                        
                      )
                      
                      ,wellPanel(
                        actionButton("button_dati_sing_an", "Estrazione Dati"),
                      
                        checkboxInput('filtra_maggiore_di', 'Filtra maggiori di', FALSE),
                        
                        numericInput("valore_maggiore_di", label = p("Valore"), value = 1000)	
                        
                      )  
                      
                       
                      
                      #end fluid row
                      ############### 1 riga ##############  
                      
                      
                    
                      ############### 2 riga ##############  
                      ,fluidRow(
                        
                        
                        
                        column(12,
                               
                           wellPanel(
                             h3('Analisi variazione nel tempo'),
                             plotOutput("plot_si_timevariation")
                           ),
                           
                          wellPanel(
                            h3('Analisi variazione nel tempo statistica'),
                            plotOutput("plot_si_timevariation_stat")
                          ),
                          
                          wellPanel(
                            tags$head(tags$script(src = "message-handler.js")),
                            
                            plotOutput("plot_istogramma"),
                            
                            plotOutput("plot_giorno_24h_tipo"),
                            
                            plotOutput("plot_giornotipo"),
                            
                            plotOutput("plot_medieorarie"),
                            
                            plotOutput("plot_mediegiornaliere"),
                           
                            plotOutput("plot_medie_mensili")
                            
                            
                            #verbatimTextOutput("titolo_grafico_parziale")
                          )  
                          
                        )# end column
                        
                      )#end fluid row  
                      ############### 2 riga ##############  
                      
                    )
                    
                      
           ),
           
           
           
           ################################## Analisi NOx ########################################
           tabPanel("Analisi NOx",
                    
                    fluidPage(
                      #titlePanel("Dynamically generated user interface components"),
                      
                      ############### 1 riga ##############  
                      fluidRow(
                        
                        column(6, wellPanel(
                          selectInput("nox_ui_select_rete", "Scelta rete", choices =lista_reti),
                          column(4, verbatimTextOutput("nox_stz_netcd"))
                        )
                        ),
                        
                        column(6, wellPanel(
                          uiOutput("nox_ui_select_stazione"),
                          column(4, verbatimTextOutput("nox_stz_statcd"))
                        ))
                        
                        
                      )#end fluid row
                      
                      ,wellPanel(
                        actionButton("button_dati_an_nox", "Estrazione Dati")
                      )  
                      
                      ############### 1 riga ##############  
                      
                      ############### 2 riga ##############  
                      ,fluidRow(
                        column(12,wellPanel(
                          h2('Grafici NO-NOx-NO2'),
                          plotOutput("plot_nox_no_scatter"),
                          plotOutput("plot_no_no2_scatter"),
                          plotOutput("plot_nox_no2_scatter"),
                          plotOutput("plot_nox_diff"),
                          
                          downloadButton('nox_download_Dataset.csv', 'Download'),
                          
                          h2('Dataset NO-NOx-NO2'),
                          DT::dataTableOutput("nox_tabella_dati"),
                          verbatimTextOutput("nox_info")  
                            
                          
                          
                          #verbatimTextOutput("titolo_grafico_parziale")
                          
                        )) #end main panel
                        
                      )#end fluid row  
                      ############### 2 riga ##############
                      
                      
                      ############### 3 riga ##############  
                      ,fluidRow(
                        column(12,wellPanel(
                          h2('Differenze validazione tripla NO-NOx-NO2'),
                          DT::dataTableOutput("nox_diff_tripla")
                          
                          
                        )
                        ) #end main panel
                        
                      )#end fluid row  
                      ############### 3 riga ##############  
                      
                      
                    )#end fluid page
           ), #end tab panel
          
           # END
           ################################## Analisi NOx ########################################
           
           
           
           tabPanel("Analisi Stazione",
                    
                    fluidPage(
                      
                      ############### 1 riga ##############  
                      fluidRow(
                        
                        column(6, wellPanel(
                          selectInput("staz_ui_select_rete", "Scelta rete", choices =lista_reti),
                          column(4, verbatimTextOutput("staz_stz_netcd"))
                        )
                        ),
                        
                        column(6, wellPanel(
                          uiOutput("staz_ui_select_stazione"),
                          column(4, verbatimTextOutput("staz_stz_statcd"))
                          ,checkboxInput("staz_in_scansione", label = "Solo stazioni IN scansione", value = TRUE)
                          
                        ))
                        
                        
                      )#end fluid row
                      ############### 1 riga ##############  
                      
                      
                      ,wellPanel(
                        actionButton("button_dati_staz", "Estrazione Dati")
                      )  
                      
                      
                      
                      ############### 2 riga ##############  
                      ,fluidRow(
                        column(12,wellPanel(
                          
                    
                          
                          wellPanel(
                            h3('Sommario statistico'),
                            verbatimTextOutput("dati_stazione_sommario_statistico")
                          ),
                          
                          wellPanel(
                            h3('Variazione nel tempo'),
                            plotOutput("dati_stazione_plot_timevariation"),
                            DT::dataTableOutput("dati_plot_timevariation")
                            
                          ),
                          
                          
                          wellPanel(
                            h3('Analisi bivariata'),
                            plotOutput("pairs_plot_staz")
                          ),
                          
                          
                          wellPanel(
                            h3('Grafico temporale'),
                            plotOutput("time_plot")
                          ),  
                          
                          downloadButton('download_staz_tabella_dati.csv', 'Download'),
                          DT::dataTableOutput("dati_stazione_staz_tabella_dati")
                          
                          
                        )) #end main panel
                        
                      )#end fluid row  
                      ############### 2 riga ##############
                      
                      
                      
                      ############### 3 riga ##############  
                      ,fluidRow(
                        column(12,wellPanel(
                          
                          
                          wellPanel(
                            h3('Rosa dei venti'),
                            plotOutput("ds_plot_rosadeiventi")
                          ),
                          
                          wellPanel(
                            h3('Rosa provenienza NOx - H2s')
                            ,plotOutput("ds_plot_direzionenox")
                          ),
                          
                          wellPanel(
                            h3('Rosa Percentile provenienza NOx - H2s')
                            ,plotOutput("ds_plot_percentile_direzionenox")
                          )
                          
                        )) #end main panel
                        
                      )#end fluid row  
                      ############### 2 riga ##############  
                      
                      
                    )#end fluid page
           ), #end tab panel
           
           # END
           ################################## Analisi inquinanti di una stazione ########################################
           
          
          tabPanel("Correlazioni",
                   
                   ############### 1 riga ##############  
                   fluidRow(
                     
                     column(4, wellPanel(
                       selectInput("cor_ui_select_rete", "Scelta rete", choices =lista_reti),
                       column(4, verbatimTextOutput("cor_netcd"))
                     )
                     ),
                     
                     column(4, wellPanel(
                       uiOutput("ui_cor_select_stazione"),
                       column(4, verbatimTextOutput("cor_statcd"))
                     )),
                     
                     column(4, wellPanel(
                       # This outputs the dynamic UI component
                       uiOutput("ui_cor_select_analizzatore"),
                       column(4, verbatimTextOutput("cor_ptid")),
                       column(6, verbatimTextOutput("cor_K_ppb_microg_20"))
                     ))
                     
                     
                   ),
                   
                   fluidRow(
                     column(6, wellPanel(
                                radioButtons("radio_corr_analizzatore", label = p(""),
                                choices = list("Analizzatore 1" = 1, "Analizzatore 2" = 2), 
                                selected = 1)
                       )
                     ), 
                     column(6, wellPanel(
                       textInput("cor_coord_analiz_1","",""),
                       textInput("cor_coord_analiz_2","","")
                      )
                    
                    )
                   )
                   
                   ,wellPanel(
                     actionButton("button_estrai_correlazione", "Estrazione Dati")
                   )  
                   #end fluid row
                   ############### 1 riga ##############  
                   
                   
                   
                   
                   
                   ############### START 2 riga ##############  
                   ,fluidRow(
                     
                     wellPanel(
                       plotOutput("ac_plot_medieorarie"),
                       
                       hr(),
                       
                       plotOutput("ac_plot_scatter_medieorarie"),
                       
                       verbatimTextOutput("coefficienti_dati_orari"),
                       
                       column(6, h4("Coeficiente di correlazione lineare di Pearson:"),  verbatimTextOutput("ac_pearson_orari" )),
                       column(6, h4("Coeficiente di correlazione di Kendall:"),  verbatimTextOutput("ac_kendall_orari" ))
                       
                       
                     )
                     
                   ) # END fluid row
                   
                   
                   
                   
                   
                   
          ),
          
          # END
          ################################## Analisi Correlazioni tra analizzatori ########################################
          
          
          
          
          
          
          
          tabPanel("Provenienza",
                   
                   fluidPage(
                     
                     ############### 1 riga ##############  
                     fluidRow(
                       
                       column(6, wellPanel(
                         selectInput("prov_select_analiz", "Scelta analizzatore", choices =lista_analizzatori)
                         ,column(5, verbatimTextOutput("prov_coord_analiz"))
                       )
                       ),
                       
                       column(6, wellPanel(
                         selectInput("prov_select_input_vvdv", "Input VV-DV",
                                     c("Seleziona un copia di sensori DV-VV della rete",
                                       "Carica dati esterni da file .CSV"
                                       )
                         )
                       ))
                       
                       
                     )#end fluid row
                     ############### 1 riga ##############  
                     
                     
                     ,wellPanel(
                       uiOutput("prov_ui_origine_dvvv")
                     )
                     
                     ,wellPanel(
                       actionButton("prov_button_estrai_dati", "Estrazione Dati")
                     )  
                     
                     
                     
                     ############### 2 riga ##############  
                     ,fluidRow(
                       column(12,wellPanel(
                         
                         h3("Tabella dati"),
                         DT::dataTableOutput("prov_tabella_dati")
                         
                       )) #end main panel
                       
                     )#end fluid row  
                     ############### 2 riga ##############
                     
                     
                     
                     
                     ############### 3 riga ##############  
                     ,fluidRow(
                       column(12,wellPanel(
                         
                         
                         wellPanel(
                           h3('Rosa dei venti'),
                           plotOutput("prov_rosadeiventi")
                         ),
                         
                         wellPanel(
                           h3('Rosa provenienza')
                           ,plotOutput("prov_plot_direzione")
                         ),
                         
                         wellPanel(
                           h3('Rosa Percentile provenienza NOx - H2s')
                           ,plotOutput("prov_plot_percentile_direzione")
                         )
                         
                       )) #end main panel
                       
                     )#end fluid row  
                     ############### 2 riga ##############  
                     
                     
                   )#end fluid page
          ), #end tab panel
          
          # END
          ################################## Analisi inquinanti di una stazione ########################################
          
          
          
          
          
         
           
           tabPanel("Dati",
                    
                    wellPanel(
                      h4("Sommario statistico"),
                      verbatimTextOutput("summary")  
                    ),
                    
                     wellPanel(
                        h4("Dataset"),
                        downloadButton('download_dati_orari.csv', 'Download'),
                        DT::dataTableOutput("tabella_dati")
                    ),
                    
                    wellPanel(
                      h4("Dati grafico giorno settimanale tipo"),
                      downloadButton('download_dati_g_sett_tipo.csv', 'Download'),
                      DT::dataTableOutput("tabella_giorno_settimanale_tipo")
                      
                    ),
                    
                    wellPanel(
                      h4("Dati grafico giorno 24h tipo"),
                      downloadButton('download_dati_g_24h_tipo.csv', 'Download'),
                      DT::dataTableOutput("tabella_giorno_tipo_24h")
                    ),
                    
                    wellPanel(
                      h4("Dati medie mensili"),
                      downloadButton('download_dati_mensili.csv', 'Download'),
                      DT::dataTableOutput("tabella_medie_mensili")
                    )
                    
                    
                    
           ),
           
           tabPanel(
             dateRangeInput("date_oss", "Intervallo di Osservazione:",
                            start = as.Date(paste("01/01/",toString(format(as.Date(Sys.Date(), format="%d/%m/%Y"),"%Y"))), format='%d/%m/%Y',  tz = 'UTC'),
                            end   = Sys.Date()-1,
                            
                            #start = as.Date("26/03/2017", format='%d/%m/%Y'),
                            #end  = as.Date("26/03/2017", format='%d/%m/%Y'),
                            
                            #start = as.POSIXct("26/03/2017", format='%d/%m/%Y'),
                            #end  = as.POSIXct("26/03/2017", format='%d/%m/%Y'),
                            
                            format = "dd/mm/yyyy"
             )
           ) 
             
             
             
           ,tabPanel("Cronologia versioni",   
                     wellPanel(
                      
                       h4("Cronologia versioni"),
                       p('1.91 - 09 Luglio 2018 - Analisi singolo analizzatore: inserito filtro per eliminare i dati sotto un certo valore, utile per analizzare i picchi'),
                       p('1.8 - 21 Marzo 2018 - Inserita pagina di analisi correlazione tra due analizzatori a scelta nella rete'),
                       p('1.7 - 06 Marzo 2018 - prima versione senza configurazioni specifiche, applicabule a tutti i sistemi Ecomanager basati su Postgres'),
                       p('1.6 - 02 Marzo 2018 - Estrazione dati solo con click su pulsnate, inserita configurazione DB Postgres su file csv'),
                       p('1.4 - 13 Febbraio 2018 - Analisi Inquinanti Stazione: Possibile scegliere anche stazioni fuori scansione, inserito grafico Rosa Percentile provenienza NOxX/H2s'),
                       
                       p('1.3 - 09 Gennaio 2018 - Corretto errore individuazione incoerenza validazione tripla quando presenti du invalidi '),
                       p('1.2 - 04 Gennaio 2018 - Analisi inquinanti stazione, inseriti grafici rosa dei venti e rosa inquinamento '),
                       p('1.1 - 09 Novembre 2017 - Analisi NOX, inserita tabella con validazione incoerente della tripla NO-NOX-NO2'),
                       p('1.0 - 20 Ottobre 2017 - Corretti bug su ora legale, bug su misure in parallelo, Bug con analizzatore senza valori (LU-FORNOLI), Aggiunte reti Autolaboratori e Geotermia in Analisi inquinanti Stazione.'),
                       p('Beta 0 - Settembte 2017')
                     )
           )         
           
)