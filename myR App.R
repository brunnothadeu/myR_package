myR.app <-
  function(){
    ui <- dashboardPage(
      dashboardHeader(title = "Interface MyR"),
      
      dashboardSidebar(
        sidebarMenu(
          
          menuItem("Intrucoes", tabName = "introTab"),
          
          menuItem("Leitura", tabName = "leituraTab", 
                   menuItem("Definicoes", tabName = "leituraTabConfig",
                            menuSubItem(icon = NULL, checkboxInput('header', 'Cabecalho', TRUE)),
                            menuSubItem(icon = NULL, radioButtons('sep', 'Separator', c('Comma (,)' = ',', 'Semicolon (;)' = ';', 'Tab ( )' = '\t'), ';')),
                            menuSubItem(icon = NULL, radioButtons('quote', 'Quote', c('None' = '', 'Double Quote' = '"', 'Single Quote' = "'"), '')),
                            menuSubItem(icon = NULL, numericInput("nhead", "Linhas exibidas:", 10)),
                            menuSubItem(icon = NULL, numericInput("maxupload", "Limite para upload (em MB):", 50)),
                            menuSubItem(icon = NULL, actionButton("maxuploadButtom", "Aumentar Limite"))
                   ),
                   menuItem("Respostas", tabName = "leituraTabRespostas"),
                   menuItem("Mapa", tabName = "leituraTabMapa"),
                   menuItem("BIB", tabName = "leituraTabBIB"),
                   menuItem("Parametros", tabName = "leituraTabParametros"),
                   menuItem("Desempenho para Posicionar", tabName = "leituraTabDesempPos"),
                   menuItem(".SCO", tabName = "leituraTabSCO"),
                   menuItem(".EXP", tabName = "leituraTabEXP"),
                   menuItem("INAmes - Full Plot", tabName = "leituraTabINAmesFP")
          ),
          
          menuItem("Abertura", tabName = "aberturaTab",
                   menuItem("Definicoes", tabName = "aberturaTabConfig",
                            menuSubItem(icon = NULL, radioButtons('tipoVE', 'Metodo', c('BIB' = 'bib', 'Codigo dos Itens' = 'codit'), 'BIB')),
                            menuSubItem(icon = NULL, numericInput("nheadVE", "Linhas exibidas:", 12))
                   ),
                   menuItem("Abrir banco", tabName = "aberturaTabAction")
          ),
          
          menuItem("Correcao", tabName = "correcaoTab",
                   menuItem("Definicoes", numericInput("Corrline", "Linha de exibicao:", 1)),
                   menuItem("Corrigir banco", tabName = "correcaoTabAction")
          ),
          
          menuItem("Escore", tabName = "escoreTab",
                   menuItem("Definicoes", radioButtons('tipoEsc', 'Metodo', c('Escore' = 'Escore', 'Normit' = 'Normit'), 'Escore')),
                   menuItem("Calcular Escore", tabName = "escoreTabAction")
          ),
          
          menuItem("AGI", tabName = "agiTab",
                   menuItem("Definicoes", tabName = "agiTabConfig",
                            menuSubItem(icon = NULL, textInput("agi.redGab", "Gabaritos", "NULL")),
                            menuSubItem(icon = NULL, textInput("agi.main", "Titulos", "Titulo IT1;Titulo IT2;Titulo IT3")),
                            menuSubItem(icon = NULL, textInput("agi.files", "Nomes dos arquivos PDF", "NULL")),                
                            menuSubItem(icon = NULL, textInput("agi.xlab", "Rotulo eixo X", "Escore")),    
                            menuSubItem(icon = NULL, textInput("agi.ylab", "Rotulo eixo Y", "Percentual de Respostas")),    
                            menuSubItem(icon = NULL, textInput("agi.Alts", "Marcacoes a serem consideradas", ".;*;A;B;C;D;E")),                  
                            menuSubItem(icon = NULL, textInput("agi.col", "Coloracao das curvas", "NULL")),                  
                            menuSubItem(icon = NULL, textInput("agi.cex", "Dimensao dos caracteres nas curvas", "3;1.2;0.7;0.7;0.7;0.7;0.7")), 
                            menuSubItem(icon = NULL, numericInput("agi.intervals", "breaks para desempenhos continuos", 50)),
                            menuSubItem(icon = NULL, textInput("agi.density", "Plotar densidade", "FALSE")), 
                            menuSubItem(icon = NULL, textInput("agi.smooth", "Indice de suavizacao das curvas", "NULL")), 
                            menuSubItem(icon = NULL, textInput("agi.dir.create", "Nome do diretorio para salvar os arquivos", "AGI")), 
                            menuSubItem(icon = NULL, textInput("agi.xlim", "Limites do eixo X", "NULL")), 
                            menuSubItem(icon = NULL, numericInput("agi.width", "Largura em polegadas", 9.6)), 
                            menuSubItem(icon = NULL, numericInput("agi.height", "Altura em polegadas", 6.8)),
                            menuSubItem(icon = NULL, textInput("agi.ext", "Extensao dos arquivos (PDF/PNG)", "PDF"))
                   ),
                   menuItem("Gerar AGI", tabName = "agiTabAction")
          ),
          
          menuItem("TCT", tabName = "tctTab",
                   menuItem("Definicoes", tabName = "tctTabConfig",
                            menuSubItem(icon = NULL, textInput("tct.Alts", "Marcacoes a serem consideradas", ".;*;A;B;C;D;E")), 
                            checkboxInput("tct.pbis", "Adotar Ponto-Bisserial?", FALSE),
                            menuSubItem(icon = NULL, textInput("tct.itemResto", "Utilizar o item-Resto? (FALSE/Escore/Normit)", "Escore")), 
                            menuSubItem(icon = NULL, numericInput("tct.arred", "Casas decimais a serem utilizadas:", 10)), 
                            checkboxInput("tct.sdpop", "Utilizar desvio populacional?", FALSE),
                            menuSubItem(icon = NULL, textInput("tct.crit", "Criterios para a analise da Bisserial (MinGab;MaxDist;Min[Gab-Dist])", "0.16;0.1;0")), 
                            checkboxInput("tct.summary", "Calcular summary dos dados?", FALSE),
                            menuSubItem(icon = NULL, numericInput("nheadTCT", "Linhas exibidas:", 12))
                   ),
                   menuItem("Gerar TCT", tabName = "tctTabAction")
          ),
          
          menuItem("Posicionamento", tabName = "posTab",
                   menuItem("Definicoes", tabName = "posTabConfig",
                            menuSubItem(icon = NULL, selectInput("pos.method", "Metodo:", list("Modelo" = "model", "Empirico" = "prop"))),
                            menuSubItem(icon = NULL, textInput("pos.breaks", "Limites e step dos pontos de posicionamento", "-5;5;0.1")),  
                            menuSubItem(icon = NULL, numericInput("pos.PROP", "Proporcao mimina no nivel", 0.65)),
                            menuSubItem(icon = NULL, numericInput("pos.NRESP", "Numero minimo de respostas no nivel", 50)),
                            menuSubItem(icon = NULL, numericInput("pos.PROP2", "Proporcao maxima no nivel anterior", 1)),
                            menuSubItem(icon = NULL, numericInput("pos.DIF", "Diferenca minima entre P(i) e P(i-1)", 0))
                   ),
                   menuItem("Posicionar os Itens", tabName = "posTabAction")
          ),
          
          menuItem("CCI", tabName = "cciTab",
                   menuItem("Definicoes", tabName = "cciTabConfig",
                            menuSubItem(icon = NULL, checkboxInput('cci.info', 'Plotar curva de informacao', TRUE)),
                            menuSubItem(icon = NULL, textInput("cci.main", "Titulos", "Titulo IT1;Titulo IT2;Titulo IT3")),
                            menuSubItem(icon = NULL, textInput("cci.SCOg", "Grupos para plot da densidade", "NULL")),
                            menuSubItem(icon = NULL, textInput("cci.breaks", "Intervalo de proficiencias para o plot", "-5;5;0.1")), 
                            menuSubItem(icon = NULL, textInput("cci.files", "Nomes dos arquivos PDF", "NULL")),                
                            menuSubItem(icon = NULL, textInput("cci.dir.create", "Nome do diretorio para salvar os arquivos", "CCI")),
                            menuSubItem(icon = NULL, numericInput("cci.width", "Largura em polegadas", 9.6)), 
                            menuSubItem(icon = NULL, numericInput("cci.height", "Altura em polegadas", 6.8))
                   ),
                   menuItem("Gerar CCI", tabName = "cciTabAction")
          ),
          
          menuItem("DIF", tabName = "difTab",
                   menuItem("Definicoes", tabName = "difTabConfig",
                            
                            menuSubItem(icon = NULL, textInput("dif.newGroup", "Grupos de interesse", '1;2')),
                            menuSubItem(icon = NULL, numericInput("dif.crit", "Criterio de DIF", 0.15)),
                            menuSubItem(icon = NULL, textInput("dif.probs", "Intervalo de analise", "0.05;0.95")),
                            tags$hr(),
                            menuSubItem(icon = NULL, textInput("dif.main", "Titulos", "NULL")),
                            menuSubItem(icon = NULL, textInput("dif.groups", "Nome dos grupos", "NULL")),
                            menuSubItem(icon = NULL, textInput("dif.col", "Coloracao das curvas", "NULL")),     
                            menuSubItem(icon = NULL, textInput("dif.density", "Plotar densidade", "area")), 
                            menuSubItem(icon = NULL, textInput("dif.dir.create", "Nome do diretorio para salvar os arquivos", "DIF")),                  
                            menuSubItem(icon = NULL, textInput("dif.xlim", "Limites do eixo X", "-4;4")), 
                            menuSubItem(icon = NULL, numericInput("dif.width", "Largura em polegadas", 9.6)), 
                            menuSubItem(icon = NULL, numericInput("dif.height", "Altura em polegadas", 6.8))
                   ),
                   menuItem("Gerar DIF", tabName = "difTabAction")
          ),
          
          menuItem("FULL PLOT", tabName = "allTab",
                   menuItem("Definicoes", tabName = "allTabConfig",
                            menuSubItem(icon = NULL, textInput("all.xlab", "Rotulos eixo X", "Escore;Proficiencia;Proficiencia")),
                            menuSubItem(icon = NULL, textInput("all.ylab", "Rotulos eixo Y", "Percentual de Respostas;Probabilidade;Percentual de Respostas")),
                            menuSubItem(icon = NULL, textInput("all.main", "Titulos", "NULL")),
                            menuSubItem(icon = NULL, textInput("all.files", "Nomes dos arquivos PDF", "NULL")),
                            menuSubItem(icon = NULL, textInput("all.redGab", "Gabaritos", "NULL")),
                            menuSubItem(icon = NULL, textInput("all.xlim.agi", "Limites do eixo X para a AGI", "NULL")),
                            menuSubItem(icon = NULL, textInput("all.smooth", "Indice de suavizacao das curvas da AGI", "NULL")),
                            menuSubItem(icon = NULL, textInput("all.Alts", "Marcacoes a serem consideradas", ".;*;A;B;C;D;E")),
                            menuSubItem(icon = NULL, textInput("all.col.agi", "Coloracao das curvas da AGI", "NULL")),
                            menuSubItem(icon = NULL, textInput("all.cex", "Dimensao dos caracteres nas curvas da AGI", "3;1.2;0.7;0.7;0.7;0.7;0.7")),
                            menuSubItem(icon = NULL, numericInput("all.intervals", "breaks para desempenhos continuos", 50)),
                            menuSubItem(icon = NULL, checkboxInput('all.info.cci', 'Plotar curva de informacao na CCI', TRUE)),
                            menuSubItem(icon = NULL, textInput("all.breaks.cci", "Intervalo de proficiencias para o plot da CCI", "-4;4;0.1")),
                            menuSubItem(icon = NULL, textInput("all.groups", "Nome dos grupos para o DIF", "NULL")),
                            menuSubItem(icon = NULL, textInput("all.probs", "Intervalo de analise do DIF", "0.05;0.95")),
                            menuSubItem(icon = NULL, textInput("all.col.dif", "Coloracao das curvas do DIF", "NULL")),
                            menuSubItem(icon = NULL, textInput("all.xlim.dif", "Limites do eixo X do DIF", "-4;4")),
                            menuSubItem(icon = NULL, textInput("all.dir.create", "Nome do diretorio para salvar os arquivos", "FULLPLOT")),
                            menuSubItem(icon = NULL, numericInput("all.width", "Largura em polegadas", 9.6)),
                            menuSubItem(icon = NULL, numericInput("all.height", "Altura em polegadas", 6.8)),
                            menuSubItem(icon = NULL, textInput("all.ext", "Extensao dos arquivos (PDF/PNG)", "PDF"))
                   ),
                   menuItem("Gerar Plots", tabName = "allTabAction")
          )
        )
      ),
      
      dashboardBody(
        tabItems(
          tabItem(tabName = "introTab",
                  verbatimTextOutput("Intro")
          ),        
          
          tabItem(tabName = "leituraTabRespostas",
                  fluidPage(
                    fluidRow(
                      fileInput('dados', 'Upload - Respostas',  accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv'), width = "95%")
                    ),
                    mainPanel(
                      DT::dataTableOutput("headD", width = "150%")
                    )
                  )
          ),
          
          tabItem(tabName = "leituraTabMapa",
                  fluidPage(
                    fluidRow(
                      fileInput('mapa', 'Upload - Mapa',  accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv'), width = "95%")
                    ),
                    mainPanel(
                      DT::dataTableOutput("headM", width = "150%")
                    )
                  )
          ),
          
          tabItem(tabName = "leituraTabBIB",
                  fluidPage(
                    fluidRow(
                      fileInput('bib', 'Upload - BIB',  accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv'), width = "95%")
                    ),
                    mainPanel(
                      DT::dataTableOutput("headB", width = "150%")
                    )
                  )
          ),  
          
          tabItem(tabName = "leituraTabParametros",
                  fluidPage(
                    fluidRow(
                      fileInput('pars', 'Upload - Parametros',  accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv'), width = "95%")
                    ),
                    mainPanel(
                      DT::dataTableOutput("headP", width = "150%")
                    )
                  )
          ),
          
          tabItem(tabName = "leituraTabDesempPos",
                  fluidPage(
                    fluidRow(
                      fileInput('desemppos', 'Upload - Desempenho para Posicionamento dos itens',  accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv'), width = "95%")
                    ),
                    mainPanel(
                      fluidRow(
                        column(2,
                               DT::dataTableOutput("headDP", width = "150%")
                        ),
                        column(7, offset = 3,
                               verbatimTextOutput("summaryDP")  
                        )
                      )
                    )
                  )
          ),
          
          tabItem(tabName = "leituraTabSCO",
                  fluidPage(
                    fluidRow(
                      fileInput('SCO', 'Upload - .SCO',  accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv'), width = "95%")
                    ),
                    mainPanel(
                      verbatimTextOutput("tableSCO"),
                      verbatimTextOutput("summarySCO")
                    )
                  )
          ),
          
          tabItem(tabName = "leituraTabEXP",
                  fluidPage(
                    fluidRow(
                      fileInput('EXP', 'Upload - .EXP',  accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv'), width = "95%")
                    ),
                    mainPanel(
                      DT::dataTableOutput("headEXP", width = "150%")
                    )
                  )
          ),
          
          tabItem(tabName = "leituraTabINAmesFP",
                  fluidPage(
                    fluidRow(
                      fileInput('APnames', 'Upload - INAmes para Full Plot',  accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv'), width = "95%")
                    ),
                    mainPanel(
                      DT::dataTableOutput("headAPn", width = "150%")
                    )
                  )
          ),
          
          tabItem(tabName = "aberturaTabAction",
                  fluidPage(
                    fluidRow(
                      actionButton("VEbuttom", "Abrir banco")
                    ),
                    mainPanel(
                      DT::dataTableOutput("headVE", width = "150%")
                    )
                  )
          ),
          
          tabItem(tabName = "correcaoTabAction",
                  fluidPage(
                    fluidRow(
                      actionButton("Corrbuttom", "Corrigir")
                    ),
                    mainPanel(
                      DT::dataTableOutput("headD2", width = "150%"),
                      DT::dataTableOutput("headall", width = "150%"),
                      plotOutput("plotProp")
                    )
                  )
          ),
          
          tabItem(tabName = "escoreTabAction",
                  fluidPage(
                    fluidRow(
                      actionButton("Escbuttom", "Calcular")
                    ),
                    mainPanel(
                      plotOutput("plothist"),
                      plotOutput("plotdens")
                    )
                  )
          ),
          
          tabItem(tabName = "agiTabAction",
                  fluidPage(
                    fluidRow(
                      actionButton("AGIbuttom", "Gerar PDF"),
                      menuSubItem(icon = NULL, numericInput("viewAGIit", "DEMO AGI:", 1)),
                      mainPanel(
                        plotOutput("viewAGI")
                      )
                    )
                  )
          ),
          
          tabItem(tabName = "tctTabAction",
                  fluidPage(
                    fluidRow(
                      actionButton("TCTbuttom", "Gerar TCT")
                    ),
                    mainPanel(
                      DT::dataTableOutput("headTCT", width = "150%")
                    )
                  )
          ),
          
          tabItem(tabName = "posTabAction",
                  fluidPage(
                    fluidRow(
                      actionButton("POSbuttom", "Posicionar Itens")
                    ),
                    mainPanel(
                      DT::dataTableOutput("headPOS", width = "150%")
                    )
                  )
          ),
          
          tabItem(tabName = "cciTabAction",
                  fluidPage(
                    fluidRow(
                      actionButton("CCIbuttom", "Gerar PDF"),
                      menuSubItem(icon = NULL, numericInput("viewCCIit", "DEMO CCI:", 1)),
                      mainPanel(
                        plotOutput("viewCCI")
                      )
                    )
                  )
          ),
          
          tabItem(tabName = "difTabAction",
                  fluidPage(
                    fluidRow(
                      actionButton("DIFbuttom", "Gerar PDF"),
                      menuSubItem(icon = NULL, numericInput("viewDIFit", "DEMO DIF:", 1)),
                      mainPanel(
                        plotOutput("viewDIF")
                      )
                    )
                  )
          ),
          
          tabItem(tabName = "allTabAction",
                  fluidPage(
                    fluidRow(
                      actionButton("ALLbuttom", "Gerar PDF"),
                      menuSubItem(icon = NULL, numericInput("viewALLit", "DEMO FULL PLOT:", 1)),
                      mainPanel(
                        plotOutput("viewALL")
                      )
                    )
                  )
          )
          
        )
      )
    )
    
    server <- function(input, output) {
      
      output$Intro <- renderText({ "Intro.." })
      output$res <- renderText({
        paste("You've selected:", input$tabs)
      })
      
      ### LEITURA
      observeEvent(input$maxuploadButtom, {
        options(shiny.maxRequestSize = input$maxupload * 1024^2) 
      })
      
      output$headD <- DT::renderDataTable({
        inFile <- input$dados
        if (is.null(inFile))
          return(NULL)
        dados <<- read.csv2(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote, stringsAsFactors = FALSE)
        head(dados, input$nhead)
      }, options = list(scrollX = T, bFilter = F, bSort = F, bInfo = F, bPaginate = F), rownames = F)
      
      output$headM <-DT::renderDataTable({
        inFile <- input$mapa
        if (is.null(inFile))
          return(NULL)     
        mapa <<- read.csv2(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote, stringsAsFactors = FALSE)
        head(mapa, input$nhead)
      }, options = list(scrollX = T, bFilter = F, bSort = F, bInfo = F, bPaginate = F), rownames = F)
      
      output$headB <- DT::renderDataTable({
        inFile <- input$bib
        if (is.null(inFile))
          return(NULL)     
        bib <<- as.list(read.csv2(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote, stringsAsFactors = FALSE))
        head(as.data.frame(bib), input$nhead)
      }, options = list(scrollX = T, bFilter = F, bSort = F, bInfo = F, bPaginate = F), rownames = F)
      
      output$headP <- DT::renderDataTable({
        inFile <- input$pars
        if (is.null(inFile))
          return(NULL)     
        pars <<- read.csv2(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote, stringsAsFactors = FALSE)
        names(pars) = c("Codigo", "a", "b", "c", "a01")[1:ncol(pars)]
        head(pars, input$nhead)
      }, options = list(scrollX = T, bFilter = F, bSort = F, bInfo = F, bPaginate = F), rownames = F)
      
      output$headDP <- DT::renderDataTable({
        inFile <- input$desemppos
        if (is.null(inFile))
          return(NULL)     
        desemppos <<- read.csv2(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote, stringsAsFactors = FALSE)
        head(desemppos, input$nhead)
      }, options = list(scrollX = T, bFilter = F, bSort = F, bInfo = F, bPaginate = F), rownames = F) 
      
      output$summaryDP <- renderPrint({
        inFile <- input$desemppos
        if (is.null(inFile))
          return(invisible(NULL))     
        desemppos <<- read.csv2(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote, stringsAsFactors = FALSE)[[1]]
        cat("\n Summary Desempenho: \n")
        summary(desemppos)
      }) 
      
      output$tableSCO <- renderPrint({
        inFile <- input$SCO
        if (is.null(inFile))
          return(invisible(NULL))     
        SCO <<- read.SCO(readLines(inFile$datapath), summary = FALSE)
        cat("Frequencia Grupos: \n")
        table(SCO[[1]])
      })   
      
      output$summarySCO <- renderPrint({
        inFile <- input$SCO
        if (is.null(inFile))
          return(invisible(NULL))
        SCO <<- read.SCO(readLines(inFile$datapath), summary = FALSE)
        cat("\n Summary SCO: \n")
        summary(SCO[[2]])
      })    
      
      output$headEXP <- DT::renderDataTable({
        inFile <- input$EXP
        if (is.null(inFile))
          return(NULL)     
        EXP <<- remakeEXP(readLines(inFile$datapath))
        head(EXP[[1]], input$nhead)
      }, options = list(scrollX = T, bFilter = F, bSort = F, bInfo = F, bPaginate = F), rownames = F) 
      
      output$headAPn <- DT::renderDataTable({
        inFile <- input$APnames
        if (is.null(inFile))
          return(NULL)     
        APnames <<- read.csv2(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote, stringsAsFactors = FALSE)
        names(APnames) = "INAmes"
        head(APnames, input$nhead)
      }, options = list(scrollX = T, bFilter = F, bSort = F, bInfo = F, bPaginate = F))
      
      ### ABERTURA
      VE <- eventReactive(input$VEbuttom, {
        if(is.null(input$tipoVE) | input$VEbuttom == 0)
          return(NULL)
        if(input$tipoVE == "bib"){
          dados <<- vetor.estendido(dados, bib)
        }else if(input$tipoVE == "codit"){
          dados <<- vetor.estendido2(dados, bib)
        }
      })
      
      output$headVE <- DT::renderDataTable({
        VE()
        head(dados, input$nheadVE)
      }, options = list(scrollX = T, bFilter = F, bSort = F, bInfo = F, bPaginate = F, lengthChange = T, pageLength = 50))
      
      ### CORRECAO
      output$headD2 <- DT::renderDataTable({
        Corr()
        head(dadosCorr[-c(1:3)], 6)
      }, options = list(scrollX = T, bFilter = F, bSort = F, bInfo = F, bPaginate = F), rownames = F) 
      
      Corr <- eventReactive(input$Corrbuttom, {
        if(input$Corrbuttom == 0)
          return(NULL)
        dadosCorr <<- corrigir(dados, mapa$Gabarito)
        return(make.headall(dadosCorr))
      })
      
      make.headall <- function(dadosCorr){
        if(input$Corrbuttom == 0)
          return(NULL)
        df <- data.frame(matrix(NA, nrow = 3, ncol = nrow(mapa)))
        df[1, ] <- dados[input$Corrline, -c(1:3)]
        df[2, ] <- mapa$Gabarito    
        df[3, ] <- dadosCorr[input$Corrline, -c(1:3)]      
        aux <- data.frame(matrix(unlist(lapply(strsplit(as.vector(summary(dadosCorr[-c(1:3)])), ":"), FUN = function(x) as.numeric(x[[2]]))), nrow = 7))
        names(aux) <- names(df)
        df <- rbind(df, aux)
        names(df) <- names(dadosCorr[-c(1:3)])
        aux <- data.frame(Info = c("Resposta_Linha", "Gabarito_Item", "Correcao_Linha", "Minimo_Item", "1st Quart._Item", "Mediana_Item", "Media_Item", "3rd Quart._Item", "Maximo_Item", "Missings_Item"))
        df <- cbind(aux, df)
        return(df)
      }
      
      output$headall <- DT::renderDataTable({
        input$Corrline
        make.headall(dadosCorr)
      }, options = list(scrollX = T, bFilter = F, bSort = F, bInfo = F, bPaginate = F), rownames = F)   
      
      output$plotProp <- renderPlot({
        if(input$Corrbuttom == 0)
          return(invisible(NULL))
        plot(colMeans(dadosCorr[-c(1:3)], na.rm = T), xlab = "Item", ylab = "Proporcao de Acertos", cex = .8, lwd = 4, ylim = c(0, 1))
      })   
      
      ### ESCORE
      Esc <- eventReactive(input$Escbuttom, {
        if(input$Escbuttom == 0)
          return(NULL)
        dados <<- dados[1:(nrow(mapa) + 3)]; dadosCorr <<- dadosCorr[1:(nrow(mapa) + 3)]
        dadosCorr <<- escore(dadosCorr, input$tipoEsc)
        dados[names(dadosCorr)[ncol(dadosCorr)]] <<- dadosCorr[[names(dadosCorr)[ncol(dadosCorr)]]]
      })
      
      output$plothist <- renderPlot({
        Esc()
        hist(dadosCorr[[ncol(dadosCorr)]], xlab = names(dadosCorr)[ncol(dadosCorr)], main = "")
      })
      
      output$plotdens <- renderPlot({
        Esc()
        plot(density(dadosCorr[[ncol(dadosCorr)]]), xlab = names(dadosCorr)[ncol(dadosCorr)], main = "")
      })  
      
      ### AGI
      readplotAGI <- function(){
        if(input$agi.xlim == "NULL"){
          xlim <- NULL
        }else{
          xlim <- as.numeric(unlist(strsplit(input$agi.xlim, ";")))
        }
        if(input$agi.smooth == "NULL"){
          smooth <- NULL
        }else{
          smooth <- as.numeric(input$agi.smooth)
        }
        if(input$agi.files == "NULL"){
          files <- NULL
        }else{
          files <- unlist(strsplit(input$agi.files, ";"))
        }
        if(input$agi.col == "NULL"){
          col.agi <- NULL
        }else{
          col.agi <- unlist(strsplit(input$agi.col, ";"))
        }
        if(input$agi.redGab == "NULL"){
          redGab <- mapa$Gabarito
        }else{
          redGab <- unlist(strsplit(input$agi.redGab, ";"))
        }
        paramsAGI <<- list(
          redGab = redGab,
          main = unlist(strsplit(input$agi.main, ";")),
          files = files,
          xlab = input$agi.xlab,
          ylab = input$agi.ylab,
          Alts = unlist(strsplit(input$agi.Alts, ";")),
          col.agi = col.agi,
          cex = as.numeric(unlist(strsplit(input$agi.cex, ";"))),
          intervals = input$agi.intervals,
          density = input$agi.density %in% c("TRUE", "T", "t", "true", "True"),
          smooth = smooth,
          dir.create = input$agi.dir.create,
          xlim = xlim,
          width = input$agi.width,
          height = input$agi.height,
          ext = input$agi.ext
        )
        if(length(paramsAGI$main) < nrow(mapa))
          paramsAGI$main <<- c(paramsAGI$main, paste0("IT Cod: ", mapa$Codigo[(length(paramsAGI$main) + 1):nrow(mapa)]))
      }
      
      output$viewAGI <- renderPlot({
        readplotAGI()
        agi.plot(dados[-c(1:3, ncol(dados))], dados[[ncol(dados)]], redGab = paramsAGI$redGab, main = paramsAGI$main, files = paramsAGI$files, xlab = paramsAGI$xlab, ylab = paramsAGI$ylab, Alts = paramsAGI$Alts, col.agi = paramsAGI$col.agi, cex = paramsAGI$cex, intervals = paramsAGI$intervals, density = paramsAGI$density, smooth = paramsAGI$smooth, dir.create = paramsAGI$dir.create, xlim = paramsAGI$xlim, width = paramsAGI$width, height = paramsAGI$height, ext = paramsAGI$ext, shinyDemo = as.numeric(input$viewAGIit))
      })
      
      observeEvent(input$AGIbuttom, {
        readplotAGI()
        agi.plot(dados[-c(1:3, ncol(dados))], dados[[ncol(dados)]], redGab = paramsAGI$redGab, main = paramsAGI$main, files = paramsAGI$files, xlab = paramsAGI$xlab, ylab = paramsAGI$ylab, Alts = paramsAGI$Alts, col.agi = paramsAGI$col.agi, cex = paramsAGI$cex, intervals = paramsAGI$intervals, density = paramsAGI$density, smooth = paramsAGI$smooth, dir.create = paramsAGI$dir.create, xlim = paramsAGI$xlim, width = paramsAGI$width, height = paramsAGI$height, ext = paramsAGI$ext)
      })
      
      ### TCT
      TCT <- eventReactive(input$TCTbuttom, {
        if(input$TCTbuttom == 0)
          return(NULL)
        tct.result <<- tct(dados, dadosCorr, mapa, Alts = unlist(strsplit(input$tct.Alts, ";")), pbis = input$tct.pbis, itemResto = input$tct.itemResto, arred = input$tct.arred, sdpop = input$tct.sdpop, crit = as.numeric(unlist(strsplit(input$tct.crit, ";"))), summary = input$tct.summary)
        if(input$tct.summary){
          write.csv2(tct.result[[2]], "Summary.csv", row.names = F)
          tct.result <<- tct.result[[1]]
        }
        write.csv2(tct.result, "TCT.csv", row.names = F)
      })
      
      output$headTCT <- DT::renderDataTable({
        TCT()
        head(tct.result, input$nheadTCT)
      }, options = list(scrollX = T, bFilter = F, bSort = F, bInfo = F, bPaginate = F), rownames = F) 
      
      ### Posicionamento
      POS <- eventReactive(input$POSbuttom, {
        if(input$POSbuttom == 0)
          return(NULL)
        if(input$pos.method == "model"){
          if(!exists("pars"))
            stop("Dataset pars nao encontrado.")
          names(pars) <- c("Codigo", "a", "b", "c", "a01")
          posicionamento <<- data.frame(Codigo = pars$Codigo, Nivel = pos.model(pars, as.numeric(unlist(strsplit(input$pos.breaks, ";"))), list(PROP = input$pos.PROP, PROP2 = input$pos.PROP2, DIF = input$pos.DIF)), stringsAsFactors = F)
        }else{
          posicionamento <<- data.frame(Codigo = names(dadosCorr[-c(1:3, ncol(dados))]), Nivel = pos.prop(dadosCorr[-c(1:3, ncol(dados))], desemppos, dadosCorr[[3]], as.numeric(unlist(strsplit(input$pos.breaks, ";"))), list(PROP = input$pos.PROP, NRESP = input$pos.NRESP, PROP2 = input$pos.PROP2, DIF = input$pos.DIF)), stringsAsFactors = F)
        }
        posicionamento$Nivel <- as.numeric(posicionamento$Nivel)
        write.csv2(posicionamento, paste0("Posicionamento_", input$pos.method, ".csv"), row.names = F)
      })
      
      output$headPOS <- DT::renderDataTable({
        POS()
        posicionamento
      }, options = list(scrollX = T, bFilter = F, bSort = F, bInfo = F, bPaginate = F), rownames = F)
      
      ### CCI
      readplotCCI <- function(){
        if(input$cci.files == "NULL"){
          files <- NULL
        }else{
          files <- unlist(strsplit(input$cci.files, ";"))
        }
        if(input$cci.SCOg == "NULL"){
          plotSCO <- NULL
        }else{
          plotSCO <- SCO[SCO[[1]] %in% unlist(strsplit(input$cci.SCOg, ";")), 2]
        }
        if(input$cci.breaks == "NULL"){
          breaks <- NULL
        }else{
          breaks <- as.numeric(unlist(strsplit(input$cci.breaks, ";")))
        }
        paramsCCI <<- list(
          main = unlist(strsplit(input$cci.main, ";")),
          SCO = plotSCO,
          breaks = breaks,
          files = files,
          dir.create = input$cci.dir.create,
          width = input$cci.width,
          height = input$cci.height
        )
        if(length(paramsCCI$main) < nrow(mapa))
          paramsCCI$main <<- c(paramsCCI$main, paste0("IT Cod: ", mapa$Codigo[(length(paramsCCI$main) + 1):nrow(mapa)]))
        names(pars) <<- c("Codigo", "a", "b", "c", ifelse(input$cci.info, "a01", "NoInfo"))[1:ncol(pars)]
      }
      
      output$viewCCI <- renderPlot({
        readplotCCI()
        cci.plot(pars, main = paramsCCI$main, SCO = paramsCCI$SCO, breaks = paramsCCI$breaks, files = paramsCCI$files, dir.create = paramsCCI$dir.create, width = paramsCCI$width, height = paramsCCI$height, shinyDemo = as.numeric(input$viewCCIit) )
      })
      
      observeEvent(input$CCIbuttom, {
        readplotCCI()
        cci.plot(pars, main = paramsCCI$main, SCO = paramsCCI$SCO, breaks = paramsCCI$breaks, files = paramsCCI$files, dir.create = paramsCCI$dir.create, width = paramsCCI$width, height = paramsCCI$height)
      })
      
      ### DIF
      readplotDIF <- function(){
        if(input$dif.groups == "NULL"){
          groups <- NULL
        }else{
          groups <- unlist(strsplit(input$dif.groups, ";"))
        }
        if(input$dif.col == "NULL"){
          col <- NULL
        }else{
          col <- unlist(strsplit(input$dif.col, ";"))
        }
        if(input$dif.density == "NULL"){
          density <- NULL
        }else{
          density <- input$dif.density
        }
        if(input$dif.main == "NULL"){
          main <- NULL
        }else{
          main <- input$dif.main
        }
        paramsDIF <<- list(
          newGroup = unlist(strsplit(input$dif.newGroup, ";")),
          crit = input$dif.crit,
          probs = as.numeric(unlist(strsplit(input$dif.probs, ";"))),
          main = main,
          groups = groups,
          col = col,
          density = density,
          dir.create = input$dif.dir.create,
          xlim = as.numeric(unlist(strsplit(input$dif.xlim, ";"))),
          width = input$dif.width,
          height = input$dif.height
        )
        EXPtemp <<- calcDIF(EXP, paramsDIF$newGroup)
        DIFits <<- checkDIF(EXPtemp, SCO, newGroup = paramsDIF$newGroup, crit = paramsDIF$crit, probs = paramsDIF$probs)
        if(length(paramsDIF$main) < length(EXPtemp))
          paramsDIF$main <<- c(paramsDIF$main, paste0("IT: ", names(EXPtemp)[(length(paramsDIF$main) + 1):length(EXPtemp)]))
      }
      
      output$viewDIF <- renderPlot({
        readplotDIF()
        AUX <<- EXPtemp[names(EXPtemp) %in% c(as.character(DIFits[[1]][, 1]), as.character(DIFits[[2]][, 1]))]
        if(as.numeric(input$viewDIFit) <= length(AUX))
          dif.plot(AUX, SCO, main = names(AUX), groups = paramsDIF$groups, probs = paramsDIF$probs, col.dif = paramsDIF$col, density = paramsDIF$density, dir.create = paramsDIF$dir.create, xlim = paramsDIF$xlim, width = paramsDIF$width, height = paramsDIF$height, shinyDemo = as.numeric(input$viewDIFit))
      })
      
      output$ITsDIF <- renderPrint({
        readplotDIF()
        DIFits
      })  
      
      observeEvent(input$DIFbuttom, {
        readplotDIF()
        dif.plot(EXPtemp, SCO, main = paramsDIF$main, groups = paramsDIF$groups, probs = paramsDIF$probs, col.dif = paramsDIF$col, density = paramsDIF$density, dir.create = paramsDIF$dir.create, xlim = paramsDIF$xlim, width = paramsDIF$width, height = paramsDIF$height)
        write.csv2(DIFits$DIF, "DIF.csv", row.names = F)
        write.csv2(DIFits$Ajuste, "Ajuste.csv", row.names = F)
      })
      
      ### Full Plot
      readplotALL <- function(){
        if(input$all.main == "NULL"){
          main <- APnames[[1]]
        }else{
          main <- unlist(substr(input$all.main, ";"))
        }
        if(input$all.xlim.agi == "NULL"){
          xlim.agi <- NULL
        }else{
          xlim.agi <- as.numeric(unlist(strsplit(input$all.xlim.agi, ";")))
        }
        if(input$all.smooth == "NULL"){
          smooth <- NULL
        }else{
          smooth <- as.numeric(input$all.smooth)
        }
        if(input$all.files == "NULL"){
          files <- APnames[[1]]
        }else{
          files <- unlist(strsplit(input$all.files, ";"))
        }
        if(input$all.col.agi == "NULL"){
          col.agi <- NULL
        }else{
          col.agi <- unlist(strsplit(input$all.col.agi, ";"))
        }
        if(input$all.redGab == "NULL"){
          redGab <- mapa$Gabarito
        }else{
          redGab <- unlist(strsplit(input$all.redGab, ";"))
        }    
        if(input$all.breaks.cci == "NULL"){
          breaks.cci <- NULL
        }else{
          breaks.cci <- as.numeric(unlist(strsplit(input$all.breaks.cci, ";")))
        }    
        if(input$all.groups == "NULL"){
          groups <- NULL
        }else{
          groups <- unlist(strsplit(input$all.groups, ";"))
        }
        if(input$all.col.dif == "NULL"){
          col.dif <- NULL
        }else{
          col.dif <- unlist(strsplit(input$all.col.dif, ";"))
        }
        
        paramsALL <<- list(
          xlab = unlist(strsplit(input$all.xlab, ";")),
          ylab = unlist(strsplit(input$all.ylab, ";")),
          main = main,
          files = files,
          redGab = redGab,
          xlim.agi = xlim.agi,
          smooth = smooth,
          Alts = unlist(strsplit(input$all.Alts, ";")),
          col.agi = col.agi,
          cex = as.numeric(unlist(strsplit(input$all.cex, ";"))),
          intervals = input$all.intervals,
          breaks.cci = breaks.cci,
          groups = groups,
          probs = as.numeric(unlist(strsplit(input$all.probs, ";"))),
          col.dif = col.dif,
          xlim.dif = as.numeric(unlist(strsplit(input$all.xlim.dif, ";"))),
          dir.create = input$all.dir.create,
          width = input$all.width,
          height = input$all.height,
          ext = input$all.ext
        )
        if(length(paramsALL$main) < length(APnames[[1]]))
          paramsALL$main <<- c(paramsALL$main, paste0("IT: ", APnames[[1]][(length(paramsALL$main) + 1):length(APnames[[1]])]))
        names(pars) <<- c("Codigo", "a", "b", "c", ifelse(input$all.info.cci, "a01", "NoInfo"))[1:ncol(pars)]
      }
      
      output$viewALL <- renderPlot({
        readplotALL()
        full.plot(dados[-c(1:3, ncol(dados))], desemp = dadosCorr[[ncol(dadosCorr)]], pars = pars, EXP = EXP[APnames[[1]]], SCO = SCO, xlab = paramsALL$xlab, ylab = paramsALL$ylab,
                 main = paramsALL$main, files = paramsALL$files, redGab = paramsALL$redGab, xlim.agi = paramsALL$xlim.agi, smooth = paramsALL$smooth,
                 Alts = paramsALL$Alts, col.agi = paramsALL$col.agi, cex = paramsALL$cex, intervals = paramsALL$intervals, breaks = paramsALL$breaks.cci, groups = paramsALL$groups,
                 probs = paramsALL$probs, col.dif = paramsALL$col.dif, xlim.dif = paramsALL$xlim.dif, dir.create = paramsALL$dir.create, width = paramsALL$width, height = paramsALL$height, ext = paramsALL$ext, shinyDemo = as.numeric(input$viewALLit))
      })
      
      observeEvent(input$ALLbuttom, {
        readplotALL()
        full.plot(dados[-c(1:3, ncol(dados))], desemp = dadosCorr[[ncol(dadosCorr)]], pars = pars, EXP = EXP[APnames[[1]]], SCO = SCO, xlab = paramsALL$xlab, ylab = paramsALL$ylab,
                 main = paramsALL$main, files = paramsALL$files, redGab = paramsALL$redGab, xlim.agi = paramsALL$xlim.agi, smooth = paramsALL$smooth,
                 Alts = paramsALL$Alts, col.agi = paramsALL$col.agi, cex = paramsALL$cex, intervals = paramsALL$intervals, breaks = paramsALL$breaks.cci, groups = paramsALL$groups,
                 probs = paramsALL$probs, col.dif = paramsALL$col.dif, xlim.dif = paramsALL$xlim.dif, dir.create = paramsALL$dir.create, width = paramsALL$width, height = paramsALL$height, ext = paramsALL$ext)
      })
    }
    shinyApp(ui, server)
  }

