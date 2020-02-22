

# Main NavBar ----
#options(spinner.size=1, spinner.type=5)

ui <-  navbarPage("Benvenuto",
                  #theme=shinythemes::shinytheme("flatly"),
                  
                  ### WELCOME PAGE ----
                  tabPanel("read.me", icon = icon("book-open"),   
                                     column(3,
                                             br(),  
                                             tags$b("Please wait/refresh for the app to be downloaded from the survey.", style="color:red"),
                                             

                                             h3("Data"),
                                             h4("Load your data and start convention. After the convention filter the data and see all the relevant informations base on 
                                                the filtered data during the app runs."),
                                             
                                             h3("Authors"),
                                            h4("Information about the Authors."),
                                             h3("Citations"),
                                            h4("Local and global citated documents."),
                                            h3("Tree"),
                                            h4("Create a thematic map based on co-word network analysis and clustering and plot a bibliographic network."),
                                             h3("Map"),
                                            h4("Country scientidic production and collaboration."),
                                             h3("Words"),
                                            h4("Most used words."),
                                            h3("Cluster"),
                                            h3("Cluster analysis."),
                                             h3("Network analysis."),

                           #https://rstudio.com/about/logos/
                          # tags$a("Contact with the App Builder", href=http://"www00.unibg.it/struttura/strutturasmst.asp?id_notizia=81930"),
                          tags$a(href="https://github.com/massimoaria/bibliometrix", "Get the Code!"), br(),
                          tags$a(href="https://cran.r-project.org/web/packages/bibliometrix/bibliometrix.pdf", "Get the manual!"),
                          
                           h6("Built with ",
                              img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                              "by",
                              img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
                              ".") #,
                           
                          # h6("For more read:Aria, M., & Cuccurullo, C. (2017).",  strong(" bibliometrix: An R-tool for comprehensive science mapping analysis."),
                          #   em(" Journal of Informetrics"),", 11(4), 959-975.")   #, align="center")
                          
                           #fluidRow(
                          # )closes fluidRow
                           
                  )  #closes clolumn
                  ), # closes tabPanel
                  
                  ### Loading page ----
                  
                  tabPanel( "Data", icon = icon("database"),
                    sidebarLayout(
                      sidebarPanel(width=2,
                                   h4(em(strong("Importing the datset"))),
                                   br(),
                                   selectInput("load", 
                                               label = "Please, choose  a .txt file",
                                               choices = c(#" "="null",
                                                           "Import raw file"="import" ), #,
                                                           #"Load bibliometrix file(s)"="load"),
                                             #  selected = "null"),
                                   selected =   "Import raw file"),
                                   
                                            
                                   #br(),
                                   conditionalPanel(condition = "input.load == 'import'",
                                                    selectInput("dbsource", 
                                                                label = "Database",
                                                                choices = c("Web of Science (WoS/WoK)"="isi", 
                                                                            "Scopus"="scopus",
                                                                            "Dimensions"="dimensions"),
                                                                selected = "isi")),
                                   conditionalPanel(condition = "input.dbsource == 'isi' & input.load == 'import'",
                                                    selectInput("format", 
                                                                label = "File format",
                                                                choices = c("Plain Text"="plaintext", 
                                                                            "BibTeX"="bibtex"),
                                                                selected = "plaintext")),
                                   
                                   h6(paste("Note: For WoS select plain text file.",
                                            "And for Scorpus select BibTex.")),
                                   
                                   #conditionalPanel(condition = "input.dbsource == 'dimensions' & input.load == 'import'",
                                  #                  selectInput("format", 
                                  #                              label = "File format",
                                  #                              choices = c("Excel (Topic Analysis)"="excel",
                                  #                                          "CSV (bibliometric mapping)"="csv"),
                                  #                              selected = "excel")),
                                   
                                   
                                   conditionalPanel(condition = "input.load != 'null'",
                                                    fileInput("file1", "Choose a file",
                                                              multiple = FALSE,
                                                              accept = c(
                                                                ".csv", 
                                                                ".txt", 
                                                                ".bib", 
                                                                ".xlsx", 
                                                                ".zip",
                                                                ".xls", 
                                                                #".rdata", 
                                                                ".rda", 
                                                                ".rds") 
                                                              )),
                                                                
                                                   
                                   
                                   #h6("Here accept single .txt/.bib/.csv/.xslx/.RData files, or multiple .txt/.bib/.csv files compressed in a single .zip archive."),
                                   conditionalPanel(condition = "input.load != 'null'",
                                                    actionButton("applyLoad", "Start Convension")),
                                   tags$hr(),
                                   
                                   uiOutput("textLog"),
                                   #shinycssloaders::withSpinner(verbatimTextOutput("log")),
                                   
                                   tags$hr(),
                                   
                                   h4(em(strong("Export the .bib file "))),
                                   br(),
                                   
                                   ### download xlsx
                                   #  selectInput('save_file', 'Save as:', choices = c(' ' ='null',
                                   #                                                   'Excel/R format' = 'xlsx'),
                                   #              selected = 'null'),
                                   # ### prova
                                   # conditionalPanel(condition = "input.save_file != 'null'",
                                   # shinySaveButton("save", "Save file", "Save file as ...", filetype=list(xlsx="xlsx", RData="RData")))#,
                                   # 
                                   # ###FINE PROVA
                                   ### download xlsx
                                   selectInput('save_file', 'Save as:', choices = c(' ' ='null',
                                                                                    'Excel' = 'xlsx',
                                                                                    'R Data Format' = 'RData'),
                                               selected = 'null'),
                                   conditionalPanel(condition = "input.save_file != 'null'",
                                                    downloadButton("collection.save", "Export"))
                      ),
                     

                      
                     
                     mainPanel(   width = 10,
                                   tabsetPanel(type = "tabs",
                                               # Tab 1: Plot
                                               tabPanel( title = "Dataset", icon = icon("database"),
                                                         tags$head(tags$style(HTML("table.dataTable.hover tbody tr:hover, table.dataTable.display tbody tr:hover {
                                                         background-color: #9c4242 !important;}"))),

                                                         tags$style(HTML(".dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing,.dataTables_wrapper .dataTables_paginate .paginate_button, .dataTables_wrapper .dataTables_paginate .paginate_button.disabled {
                                                         color: #000000 !important; }")),
                       
                                                         shinycssloaders::withSpinner(DT::DTOutput("contents"))),
                                               
                                               ### Filters page ----
                                               tabPanel("Filter", icon = icon("table"),
                                                        sidebarLayout(
                                                          sidebarPanel(width=2,
                                                                       h3(em(strong("Filter "))),
                                                                       br(),
                                                                       uiOutput("textDim"),
                                                                       uiOutput("selectType"),
                                                                       uiOutput("selectSO"),
                                                                       uiOutput("sliderPY"),
                                                                       uiOutput("sliderTC") ,
                                                                       #uiOutput("selectDE"),#AURTHOR'S KEYWORDS
                                                                       #uiOutput("selectSource"),
                                                                       selectInput("bradfordSources", 
                                                                                   label = "Source by Bradford Law Zones",
                                                                                   choices = c(#"Core Sources"="core", 
                                                                                     #                         "Core + Zone 2 Sources"="zone2",
                                                                                     "All Sources"="all"),
                                                                                   selected = "all")
                                                          ),
                                                          
                                                          mainPanel(DT::DTOutput("dataFiltered"))
                                                          
                                                        ) ) ,
                                               
                                               navbarMenu( "Summary", icon = icon("list-alt"),
                                                           tabPanel("Main Information",          
                                                           shinycssloaders::withSpinner(DT::DTOutput(outputId = "MainInfo"))
                                                           ),
                                                           tabPanel("Most Productive Authors",          
                                                                    shinycssloaders::withSpinner(DT::DTOutput(outputId = "Mos.Prod.Authors"))
                                                            ),
                                                           tabPanel("Most Cited Papers",          
                                                                    shinycssloaders::withSpinner(DT::DTOutput(outputId = "Most.Cited.Papers"))
                                                           ),
                                                           tabPanel("Most Productive Countries",          
                                                                    shinycssloaders::withSpinner(DT::DTOutput(outputId = "Most.Prod.Countries"))
                                                           ),
                                                           tabPanel("Total Citation Per Country",          
                                                                    shinycssloaders::withSpinner(DT::DTOutput(outputId = "TC.Per.Countries"))
                                                           ),
                                                           
                                                           tabPanel("Most Relevant Sources",   
                                                                    
                                                                   # sidebarLayout(
                                                                  #    sidebarPanel(width=2,
                                                                  #                 h4(em(strong("Most Relevant Sources "))),
                                                                  #                 numericInput("MostRelSourcesK", 
                                                                  #                              label=("Number of Sources"), 
                                                                  #                              value = 20)),
                                                                  #  mainPanel(
                                                                    shinycssloaders::withSpinner(DT::DTOutput(outputId = "Most.Rel.Sources"))
                                                                  # shinycssloaders::withSpinner(DT::DTOutput("MostRelSourcesTable"))
                                                                   # ))
                                              
                                                                    
                                                           ),
                                                           tabPanel("Most Relevant Keywords",          
                                                                    shinycssloaders::withSpinner(DT::DTOutput(outputId ="Most.Rel.Keywords"))
                                                           ),
                                                           tabPanel("Annual Scientific Production",
                                                                    verbatimTextOutput("CAGR"),
                                                                    shinycssloaders::withSpinner(plotlyOutput(outputId = "AnnualProdPlot", height = 400)),
                                                                    shinycssloaders::withSpinner(DT::DTOutput("AnnualProdTable")),
                                                                    
                                                                    h4("Average Citation Per Year"),
                                                                    shinycssloaders::withSpinner(plotlyOutput(outputId = "AnnualTotCitperYearPlot", height = 400)),
                                                                    shinycssloaders::withSpinner(DT::DTOutput("AnnualTotCitperYearTable"))
                                                           ))
                                                           
                                                           
                                                       
                                                                                   

                                                         
                                               
                                              )))), #CLOSE THE TABPANEL DATA
                  
                  
                  
                  
                  
                              #tabPanel("Citations", icon = icon("user-friends"),
                                       
                              #         navbarMenu( "Statistics", icon = icon("list-alt"),
                              #                     tabPanel("Local Cited Sources",          
                              #                              shinycssloaders::withSpinner(DT::DTOutput(outputId = "MainInfo"))
                              #                     )
                              #         )
                               # ),  #CLOSE THE TABPNEL CITATIONS
                  
                  tabPanel("Authors", icon = icon("user"),
                           sidebarLayout(
                             sidebarPanel(width=2,
                           
                           h4(em(strong("Most Relevant Authors"))),
                           
                           selectInput("AuFreqMeasure", 
                                       label = "Frequency measure",
                                       choices = c("N. of Documents "="t", 
                                                   "Percentage"="p",
                                                   "Fractionalized Frequency"="f"),
                                       selected = "t"),
                           
                           numericInput("MostRelAuthorsK", 
                                        label=("Number of Authors"), 
                                        value = 50),
                           tags$hr(),
                           h4(em(strong("Top Authors' Production"))),
                          
                           numericInput("TopAuthorsProdK", 
                                        label=("Number of Authors"), 
                                        value = 50),
                           tags$hr(),
                           h4(em(strong("Authors' Impact"))),
                           actionButton("applyHauthor", "Apply!"),
                           selectInput("HmeasureAuthors", 
                                       label = "Impact measure",
                                       choices = c("H-Index"="h", 
                                                   "G-Index"="g",
                                                   "M-Index"="m",
                                                   "Total Citation"="tc"),
                                       selected = "h"),
                           numericInput("Hkauthor", 
                                        label=("Number of authors"), 
                                        value = 50),
                           tags$hr(),
                           
                           h4(em(strong("Most Relevant Affiliations"))),
                           
                           selectInput("disAff", 
                                       label = "Affiliation Name",
                                       choices = c("Yes"="Y", 
                                                   "No"="N"),
                                       selected = "Y"),
                           
                           numericInput("MostRelAffiliationsK", 
                                        label=("Number of Affiliations"), 
                                        value = 50)
                           
                           ),
                  
                  mainPanel(
                    tabsetPanel(type = "tabs",
                                
                                
                                tabPanel("Relevant Authors",
                                         shinycssloaders::withSpinner(plotlyOutput(outputId = "MostRelAuthorsPlot", height = 700)),
                                         shinycssloaders::withSpinner(DT::DTOutput("MostRelAuthorsTable"))),
                                
                                tabPanel("Top Authors' Production",
                                         shinycssloaders::withSpinner(plotlyOutput(outputId = "TopAuthorsProdPlot", height=700)),
                                         shinycssloaders::withSpinner(DT::DTOutput("TopAuthorsProdTable"))),
                                
                                tabPanel("Top Authors' Documents",
                                         shinycssloaders::withSpinner(DT::DTOutput("TopAuthorsProdTablePapers"))),
                               
                                 tabPanel("Author's Impact",
                                         shinycssloaders::withSpinner(plotlyOutput(outputId = "AuthorHindexPlot", height = 700)),
                                         shinycssloaders::withSpinner(DT::DTOutput(outputId = "AuthorHindexTable"))) ,
                                
                                tabPanel("Most Relevant Affiliations",
                                         shinycssloaders::withSpinner(plotlyOutput(outputId = "MostRelAffiliationsPlot", height = 700))  ,
                                         shinycssloaders::withSpinner(DT::DTOutput("MostRelAffiliationsTable")))
                     
                                
                    )))), #CLOSE TAB PANEL AUTHORS
                  
                  
                  
                
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  tabPanel("Citations", icon = icon("user-friends"),
                           sidebarLayout(
                             sidebarPanel(width=2,
                                          h4(em(strong("Most Local Cited Documents"))),
                                          #br(),
                                          #h4(em(strong("Graphical Parameters: "))),
                                         
                                          numericInput("MostLocCitDocsK", 
                                                       label=("Number of Documents"), 
                                                       value = 50),
                                         
                                          selectInput(inputId = "LocCitSep", 
                                                      label = "Field separator character", 
                                                      choices = c(";" = ";", 
                                                                  ".  " = ".  ",
                                                                  "," = ","),
                                                      selected = ";"),
                                          
                                          tags$hr(),
                                          h4(em(strong("Most Global Cited Documents"))),
                                          numericInput("MostCitDocsK", 
                                                       label=("Number of Documents"), 
                                                       value = 50),
                                          
                                          selectInput("CitDocsMeasure", 
                                                      label = "Measure",
                                                      choices = c("Total Citations"="TC", 
                                                                  "Total Citations per Year"="TCY"),
                                                      selected = "TC"),
                                          
                                          
                                        

                                          tags$hr(),
                                          h4(em(strong("Most Local Cited Sources (from Reference Lists)"))),
                                          #br(),
                                          #h4(em(strong("Graphical Parameters: "))),
                                          #"  ",
                                          numericInput("MostRelCitSourcesK", 
                                                       label=("Number of Sources"), 
                                                       value = 50)#,
                                         
                                          #h3(em(strong("MY CITITAION"))),
                                          #br(),
                                          #numericInput("MostRelCitSourcesK", 
                                          #             label=("Number of Sources"), 
                                          #              value = 20)
                                         
                                          # tags$hr(),
                                          
                                         # h4(em(strong("Most Relevant Authors"))),
                                         # br(),
                                          
                                          #selectInput("AuFreqMeasure", 
                                          #            label = "Frequency measure",
                                          #            choices = c("N. of Documents "="t", 
                                          #                        "Percentage"="p",
                                          #                        "Fractionalized Frequency"="f"),
                                          #            selected = "t"),
                                          
                                          #numericInput("MostRelAuthorsK", 
                                          #             label=("Number of Authors"), 
                                          #             value = 50),
                                          #tags$hr(),
                                          #h4(em(strong("Authors' Production over Time"))),
                                          #br(),
                                          #numericInput("TopAuthorsProdK", 
                                          #             label=("Number of Authors"), 
                                          #             value = 50),
                                          #tags$hr(),
                                          #h4(em(strong("Authors' Impact"))),
                                          #selectInput("HmeasureAuthors", 
                                          #            label = "Impact measure",
                                          #            choices = c("H-Index"="h", 
                                          #                        "G-Index"="g",
                                          #                        "M-Index"="m",
                                          #                        "Total Citation"="tc"),
                                          #            selected = "h")
                                          
                                         
                             ),
                             mainPanel(
                               tabsetPanel(type = "tabs",
                                           
                                           tabPanel("Most Local Citated Documents",
                                                    shinycssloaders::withSpinner(plotlyOutput(outputId = "MostLocCitDocsPlot",height = 700)),
                                                    shinycssloaders::withSpinner(DT::DTOutput("MostLocCitDocsTable"))),
                                          
                                           tabPanel("Most Global Citated Documents",
                                                    shinycssloaders::withSpinner(plotlyOutput(outputId = "MostCitDocsPlot",height = 700)),
                                                    shinycssloaders::withSpinner(DT::DTOutput("MostCitDocsTable" ))),
                                           
                                           
                                           
                                           
                                           tabPanel("Most Citated Sources",
                                                    shinycssloaders::withSpinner(plotlyOutput(outputId = "MostRelCitSourcesPlot",height = 700)),
                                                    shinycssloaders::withSpinner(DT::DTOutput("MostRelCitSourcesTable")))#,
                                          
                                          # tabPanel("Relevant Authors",
                                          #          shinycssloaders::withSpinner(plotlyOutput(outputId = "MostRelAuthorsPlot", height = 700)),
                                          #          shinycssloaders::withSpinner(DT::DTOutput("MostRelAuthorsTable"))),
                                           
                                        #   tabPanel("Top Authors' Production",
                                        #            shinycssloaders::withSpinner(plotlyOutput(outputId = "TopAuthorsProdPlot", height=700)),
                                        #            shinycssloaders::withSpinner(DT::DTOutput("TopAuthorsProdTable"))),
                                        #            
                                        #  tabPanel("Top Authors' Documents",
                                        #           shinycssloaders::withSpinner(DT::DTOutput("TopAuthorsProdTablePapers"))),
                                        #  tabPanel("Author's Impact",
                                        #           shinycssloaders::withSpinner(plotlyOutput(outputId = "AuthorHindexPlot", height = 700)),
                                        #           shinycssloaders::withSpinner(DT::DTOutput(outputId = "AuthorHindexTable")))
                                          
                               )))),
                             
                             
                  
                  tabPanel("Tree", icon = icon("tree"),
                           sidebarLayout(
                             sidebarPanel(width=2,   
                                          h4(em(strong("Three Plot "))),
                                         
                                          actionButton("apply3F", "Apply!"),
                                        
                                          selectInput("CentralField",
                                                      label = "Middle Based",
                                                      choices = c("Authors" = "AU",
                                                                  "Affiliations" = "AU_UN",
                                                                  "Countries"="AU_CO",
                                                                  "Keywords" = "DE",
                                                                  "Keywords Plus" = "ID",
                                                                  "Titles" = "TI_TM",
                                                                  "Abstract" = "AB_TM",
                                                                  "Sources" = "SO",
                                                                  "References" = "CR",
                                                                  "Cited Sources" = "CR_SO"),
                                                      selected = "AU"),
                                          sliderInput("CentralFieldn", 
                                                      label=("Number of Middle Based"), 
                                                      min = 1, max = 100, step = 1, value = 50),
                                          selectInput("LeftField",
                                                      label = "Left Based",
                                                      choices = c("Authors" = "AU",
                                                                  "Affiliations" = "AU_UN",
                                                                  "Countries"="AU_CO",
                                                                  "Keywords" = "DE",
                                                                  "Keywords Plus" = "ID",
                                                                  "Titles" = "TI_TM",
                                                                  "Abstract" = "AB_TM",
                                                                  "Sources" = "SO",
                                                                  "References" = "CR",
                                                                  "Cited Sources" = "CR_SO"),
                                                      selected = "CR"),
                                          sliderInput("LeftFieldn", 
                                                      label=("Number of Left Based"), 
                                                      min = 1, max = 100, step = 1, value = 50),
                                          selectInput("RightField",
                                                      label = "Right Based",
                                                      choices = c("Authors" = "AU",
                                                                  "Affiliations" = "AU_UN",
                                                                  "Countries"="AU_CO",
                                                                  "Keywords" = "DE",
                                                                  "Keywords Plus" = "ID",
                                                                  "Titles" = "TI_TM",
                                                                  "Abstract" = "AB_TM",
                                                                  "Sources" = "SO",
                                                                  "References" = "CR",
                                                                  "Cited Sources" = "CR_SO"),
                                                      selected = "DE"),
                                          sliderInput("RightFieldn", 
                                                      label=("Number of Right Based"), 
                                                      min = 1, max = 100, step = 1, value = 50)
                             ),
                  
                             mainPanel(
                               shinycssloaders::withSpinner(networkD3::sankeyNetworkOutput(outputId = "ThreeFielsPlot",height = "700px"))
                             )
                           )),
                  
                  
                  
                                                   
                              tabPanel("Map", icon = icon("map"),                   
                                                   
                           mainPanel(
                             tabsetPanel( type ="tabs",   
                                           
                                          tabPanel("Map1",
                                                   shinycssloaders::withSpinner(plotlyOutput(outputId = "countryProdPlot", height = 700)),
                                                   shinycssloaders::withSpinner(DT::DTOutput("countryProdTable"))),
                                                   
                                        
                                          tabPanel("Map2",
                                                   sidebarLayout(
                                                     
                                                     sidebarPanel(width=2,
                                                                  h4(em(strong("Collaboration WorldMap"))),
                                                                  br(),
                                                                  actionButton("applyWM", "Apply!"),

                                                                  numericInput("WMedges.min", 
                                                                               label=("Min edges"),
                                                                               value = 2,
                                                                               step = 1),
                                                                
                                                                  h4(em(strong("Graphical Parameters: "))),
                                                               
                                                                  sliderInput(inputId = "WMedgesize",
                                                                              label = "Edge size",
                                                                              min = 0.1,
                                                                              max = 20,
                                                                              value = 5)
                                                     ),
                                                     
                                                       mainPanel(
                                                       tabsetPanel(type = "tabs",
                                                                   tabPanel("Graph", 
                                                                            shinycssloaders::withSpinner(plotOutput(outputId = "WMPlot",height = 700))),
                                                                   tabPanel("Table", 
                                                                            shinycssloaders::withSpinner(DT::DTOutput(
                                                                              outputId = "WMTable")))
                                                       )
                                                       
                                                       #shinycssloaders::withSpinner(plotOutput(outputId = "colPlot"))
                                                     )
                                                     
                                                   )
                                                   
                                                   ) #CLOSE THE MAP2 TABPANEL
                             )) 
                           
                              ),  #CLOSE THE MAP TABPANEL                        
                             
                  
                  
                  tabPanel("Words", icon = icon("wordpress"),
                           
                           sidebarLayout(
                             # Sidebar with a slider and selection inputs
                             sidebarPanel(width=2,
                                          h4(em(strong("Most Frequent Words"))),
                                          br(),
                                          selectInput("MostRelWords", "Field",
                                                      choices = c("Keywords Plus" = "ID",
                                                                  "Author's keywords" = "DE",
                                                                  "Titles" = "TI",
                                                                  "Abstracts" = "AB"),
                                                      selected = "ID"),
                                          hr(),
                                          sliderInput("MostRelWordsN", label = "Number of words", min = 2, max = 50, step = 1, value = 20),
                             tags$hr(),    
                             
                             h4(em(strong("WordCloud"))),
                             br(),
                             selectInput("summaryTerms", "Field",
                                         choices = c("Keywords Plus" = "ID",
                                                     "Author's keywords" = "DE",
                                                     "Titles" = "TI",
                                                     "Abstracts" = "AB"),
                                         selected = "ID"),
                             hr(),
                             sliderInput("n_words", label = "Number of words", min = 50, max = 300, step = 5, value = 100),
                             selectInput("measure", "Word occurrence measure",
                                         choices = c("Frequency" = "freq",
                                                     "Square root" = "sqrt",
                                                     "Log" = "log",
                                                     "Log10" = "log10"),
                                         selected = "freq"),
                             selectInput("wcShape", "Shape",
                                         choices = c("Circle" = "circle",
                                                     "Cardiod" = "cardioid",
                                                     "Diamond" = "diamond",
                                                     "Pentagon" = "pentagon",
                                                     "Star" = "star",
                                                     "Triangle-forward" = "triangle-forward"
                                                     ,"Triangle" = "triangle"),
                                         selected = "circle"),
                             selectInput("font", label = "Font type",
                                         choices = c("Impact", "Comic Sans MS (No plz!)" = "Comic Sans MS",
                                                     "Arial", "Arial Black", "Tahoma", "Verdana", "Courier New",
                                                     "Georgia", "Times New Roman", "Andale Mono")),
                             selectInput("wcCol", "Text colors",
                                         choices = c("Random Dark" = "random-dark",
                                                     "Random Light" = "random-light"),
                                         selected = "random-dark"),
                             colourpicker::colourInput("wcBGCol", label= "Backgroud color",value="white", showColour = "background", returnName=TRUE),
                             sliderInput("scale", label = "Font size", min=0.2,max=5,step=0.1,value=1),
                             sliderInput("ellipticity", label = "Ellipticity", min=0,max=1,step=0.05,value=0.65),
                             sliderInput("padding", label = "Padding", min = 0, max = 5, value = 1, step = 1),
                             sliderInput("rotate", label = "Rotate", min = 0, max = 20, value = 0, step = 1),
                                          
                           
                             
                              h4(em(strong("Word Dynamics"))),
                         
                            
                              selectInput("growthTerms", "Field",
                                          choices = c("Keywords Plus" = "ID",
                                                      "Author's keywords" = "DE",
                                                      "Titles" = "TI",
                                                      "Abstracts" = "AB"),
                                          selected = "ID"),
                             
                              selectInput("cumTerms", "Occurrences",
                                          choices = c("Cumulate" = "Cum",
                                                      "Per year" = "noCum"),
                                          selected = "noCum"),
                             
                              selectInput("se", "Confidence Interval",
                                          choices = c("Yes" = "Yes",
                                                      "No" = "No"),
                                          selected = "No"),
                              hr(),
                              sliderInput("topkw", label = "Number of words", min = 1, max = 100, step = 1, value = c(1,10))
                             
                             
                                         
                             ),
                             
                             # Show Word Cloud
                             mainPanel(
                               tabsetPanel(type = "tabs",
                                           
                                           tabPanel("Words Dynamic",
                                                    shinycssloaders::withSpinner(plotOutput(outputId = "kwGrowthPlot")),
                                                    shinycssloaders::withSpinner(DT::DTOutput(outputId = "kwGrowthtable"))
                                           ),
                                           
                                           tabPanel("Frequent Words",
                                                    withSpinner(plotlyOutput(outputId = "MostRelWordsPlot", height = 700)),
                                                    shinycssloaders::withSpinner(DT::DTOutput("MostRelWordsTable"))
                                           ),
                                           tabPanel("World Cloud",
                                                    wordcloud2::wordcloud2Output("wordcloud", height = "500px"),
                                                    shinycssloaders::withSpinner(DT::DTOutput("wordTable"))
                                                    
                                           ))
                               
                             )
                           )),  #CLOSE THE TABPANEL WORD                 
                                                  
                           
                  
                  tabPanel("Cluster", icon = icon("layer-group"),
                           
                           sidebarLayout(
                             sidebarPanel(width=3,
                                          h4(em(strong("Cluster"))),
                                          br(),
                                          
                                          actionButton("applyTM", "Apply!"),
                                         
                                          selectInput("TMfield", 
                                                      label = "Field",
                                                      choices = c("Keywords Plus" = "ID", 
                                                                  "Author's Keywords" = "DE",
                                                                  "Titles" = "TI",
                                                                  "Abstracts" = "AB"),
                                                      selected = "ID"),
                                          conditionalPanel(
                                            condition = "input.TMfield == 'TI' | input.TMfield == 'AB'",
                                            selectInput("TMstemming", label="Word Stemming",
                                                        choices = c("Yes" = TRUE,
                                                                    "No" = FALSE),
                                                        selected = FALSE)),
                                          sliderInput("TMn", label="Number of Words",value=250,min=50,max=500,step=10),
                                          sliderInput("TMfreq", label="Min Cluster Frequency",value=5,min=1,max=100,step=1),
                                          sliderInput("TMn.labels", label="Number of Labels (for each cluster)",value=1,min=1,max=5,step=1),
                                          sliderInput("sizeTM", label="Label size",value=0.3,min=0.0,max=1,step=0.05)
                             ),
                             mainPanel(
                                       tabsetPanel(type = "tabs",
                                                   tabPanel("Map",
                                                            shinycssloaders::withSpinner(plotlyOutput(outputId = "TMPlot", height = 700))
                                                   ),
                                                   tabPanel("Network",
                                                            shinycssloaders::withSpinner(visNetworkOutput("NetPlot", height = "750px",width = "1100px"))),
                                                   tabPanel("Table",
                                                            shinycssloaders::withSpinner(DT::DTOutput(outputId = "TMTable"))
                                                   )
                                       )))
     
                  ), ## End of tabPanel ("Thematic Map")
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  navbarMenu("Network", icon = icon("project-diagram"),
                  tabPanel(title="Collobration Network", 
                           sidebarLayout(
                             sidebarPanel(width=2,
                                          h4(em(strong("Collaboration Network"))),
                                          br(),
                                          actionButton("applyCol", "Apply!"),
                                          downloadButton("network.col", "Save Pajek"),
                                          downloadButton("networkCol.fig", "Save Fig"),

                                          h4(em(strong("Network Parameters: "))),

                                          selectInput("colField", 
                                                      label = "Field",
                                                      choices = c("Authors" = "COL_AU", 
                                                                  "Institutions" = "COL_UN",
                                                                  "Countries" = "COL_CO"),
                                                      selected = "COL_AU"),
                                          selectInput("colnormalize", 
                                                      label = "Normalization",
                                                      choices = c("none", 
                                                                  "association",
                                                                  "jaccard", 
                                                                  "salton",
                                                                  "inclusion",
                                                                  "equivalence"),
                                                      selected = "none"),
                                          
                                          selectInput("collayout", 
                                                      label = "Network Layout",
                                                      choices = c("Automatic layout"="auto", 
                                                                  "Circle"="circle",
                                                                  "Fruchterman & Reingold"="fruchterman",
                                                                  "Kamada & Kawai"="kamada",
                                                                  "MultiDimensional Scaling"="mds",
                                                                  "Sphere"="sphere",
                                                                  "Star"="star"),
                                                      selected = "auto"),
                                          
                                          selectInput("colCluster", 
                                                      label = "Clustering Algorithm",
                                                      choices = c("None" = "none", 
                                                                  "Edge Betweenness" = "edge_betweenness",
                                                                  "InfoMap" = "infomap",
                                                                  "Leading Eigenvalues" = "leading_eigen",
                                                                  "Louvain" = "louvain",
                                                                  "Spinglass" = "spinglass",
                                                                  "Walktrap" = "walktrap"),
                                                      selected = "louvain"),
                                          
                                          sliderInput(inputId = "colNodes",
                                                      label = "Number of Nodes",
                                                      min = 5,
                                                      max = 1000,
                                                      value = 50),
                                          
                                          selectInput(inputId ="col.isolates",
                                                      label = "Remove Isolated Nodes",
                                                      choices = c("Yes" = "yes",
                                                                  "No" = "no"),
                                                      selected = "no"),
                                          
                                          numericInput("coledges.min", 
                                                       label=("Min edges"),
                                                       value = 2,
                                                       step = 1),
                                          
                                          h4(em(strong("Graphical Parameters: "))),
                                          
                                          sliderInput(inputId = "colAlpha",
                                                      label = "Opacity",
                                                      min = 0,
                                                      max = 1,
                                                      value = 0.7,
                                                      step=0.05),
                                          
                                          sliderInput(inputId = "min.citation",
                                                      label = "Min Citation",
                                                      min = 0,
                                                      max = 300,
                                                      value = 5, 
                                                      step = 1),
                                          
                                          sliderInput(inputId = "colLabels",
                                                      label = "Number of labels",
                                                      min = 5,
                                                      max = 250,
                                                      value = 50),
                                          
                                          selectInput(inputId ="collabel.cex",
                                                      label = "Label cex",
                                                      choices = c("Yes", 
                                                                  "No"),
                                                      selected = "Yes"),
                                          
                                          sliderInput(inputId = "collabelsize",
                                                      label = "Label size",
                                                      min = 0.0,
                                                      max = 20,
                                                      value = 6,
                                                      step = 0.10),
                                          
                                          selectInput(inputId ="col.shape",
                                                      label = "Node Shape",
                                                      choices = c("Box"="box",
                                                                  "Circle"="circle",
                                                                  "Database"="database",
                                                                  "Ellipse"="ellipse",
                                                                  "Text"="text"),
                                                      selected = "box"),
                                          
                                          sliderInput(inputId = "coledgesize",
                                                      label = "Edge size",
                                                      min = 0.1,
                                                      max = 20,
                                                      value = 5), 
                                          
                                          selectInput(inputId ="soc.curved",
                                                      label = "Curved edges",
                                                      choices = c("Yes",
                                                                  "No"),
                                                      selected = "No")
                             ),
                             mainPanel(
                               tabsetPanel(type = "tabs",
                                           tabPanel("Graph" ,
                                                    shinycssloaders::withSpinner(visNetworkOutput("colPlot", height = "750px",width = "1100px"))  ), 
                                                  # shinycssloaders::withSpinner(plotOutput(outputId = "colPlot"))), #put one of them
                                           tabPanel("Table", 
                                                    shinycssloaders::withSpinner(DT::DTOutput(
                                                      outputId = "colTable")))
                               )
                               )
                             )
                               
                  ), ## Collobration-Network
                  
                  tabPanel(title="Co-Citation Network",
                           sidebarLayout(
                             
                             sidebarPanel(width=2,
                                          h4(em(strong("Co-Citation Network"))),
                                          br(),
                                          actionButton("applyCocit", "Apply!"),
                                          downloadButton("network.cocit", "Save Pajek"),
                                          downloadButton("networkCocit.fig", "Save Fig"),
                                          "  ",
                                          "  ",
                                          h4(em(strong("Network Parameters: "))),
                                          "  ",
                                          
                                          selectInput("citField", 
                                                      label = "Field",
                                                      choices = c("Papers" = "CR", 
                                                                  "Authors" = "CR_AU",
                                                                  "Sources" = "CR_SO"),
                                                      selected = "CR"),
                                          
                                          selectInput(inputId = "citSep", 
                                                      label = "Field separator character", 
                                                      choices = c('";" (Semicolon)' = ";", 
                                                                  '"." (Dot and 3 or more spaces)' = ".   ",
                                                                  '"," (Comma)' = ","),
                                                      selected = "';'"),
                                          
                                          selectInput("citlayout", 
                                                      label = "Network Layout",
                                                      choices = c("Automatic layout"="auto", 
                                                                  "Circle"="circle",
                                                                  "Fruchterman & Reingold"="fruchterman",
                                                                  "Kamada & Kawai"="kamada",
                                                                  "MultiDimensional Scaling"="mds",
                                                                  "Sphere"="sphere",
                                                                  "Star"="star"),
                                                      selected = "auto"),
                                          
                                          selectInput("cocitCluster", 
                                                      label = "Clustering Algorithm",
                                                      choices = c("None" = "none", 
                                                                  "Edge Betweenness" = "edge_betweenness",
                                                                  "InfoMap" = "infomap",
                                                                  "Leading Eigenvalues" = "leading_eigen",
                                                                  "Louvain" = "louvain",
                                                                  "Spinglass" = "spinglass",
                                                                  "Walktrap" = "walktrap"),
                                                      selected = "louvain"),
                                          
                                          sliderInput(inputId = "citNodes",
                                                      label = "Number of Nodes",
                                                      min = 5,
                                                      max = 1000,
                                                      value = 50),
                                          
                                          selectInput(inputId ="cit.isolates",
                                                      label = "Remove Isolated Nodes",
                                                      choices = c("Yes" = "yes",
                                                                  "No" = "no"),
                                                      selected = "no"),
                                          
                                          # selectInput(inputId ="my.min.citation",
                                          #             label = "Min Citation",
                                          #            min = 0,
                                          #             max = 500,
                                          #             value = 5),
                                          
                                          numericInput("citedges.min", 
                                                       label=("Min edges"),
                                                       value = 2,
                                                       step = 1),
                                          "  ",
                                          "  ",
                                          h4(em(strong("Graphical Parameters: "))),
                                          "  ",
                                          sliderInput(inputId = "cocitAlpha",
                                                      label = "Opacity",
                                                      min = 0,
                                                      max = 1,
                                                      value = 0.7,
                                                      step=0.05),
                                          selectInput(inputId ="citShortlabel",
                                                      label = "Short Label",
                                                      choices = c("Yes", 
                                                                  "No"),
                                                      selected = "Yes"),
                                          
                                          sliderInput(inputId = "citLabels",
                                                      label = "Number of labels",
                                                      min = 5,
                                                      max = 250,
                                                      value = 50),
                                          
                                          selectInput(inputId ="citlabel.cex",
                                                      label = "Label cex",
                                                      choices = c("Yes", 
                                                                  "No"),
                                                      selected = "Yes"),
                                          
                                          sliderInput(inputId = "citlabelsize",
                                                      label = "Label size",
                                                      min = 0.0,
                                                      max = 20,
                                                      value = 6,
                                                      step = 0.10),
                                          
                                          selectInput(inputId ="cocit.shape",
                                                      label = "Node Shape",
                                                      choices = c("Box"="box",
                                                                  # "Icon"="icon",
                                                                  "Circle"="circle",
                                                                  "Database"="database",
                                                                  "Ellipse"="ellipse",
                                                                  "Text"="text"),
                                                      selected = "icon"),
                                          sliderInput(inputId = "citedgesize",
                                                      label = "Edge size",
                                                      min = 0.1,
                                                      max = 20,
                                                      value = 5), 
                                          
                                          selectInput(inputId ="cocit.curved",
                                                      label = "Curved edges",
                                                      choices = c("Yes",
                                                                  "No"),
                                                      selected = "No")
                             ),
                             mainPanel(
                               tabsetPanel(type = "tabs",
                                           
                                           tabPanel("Graph", 
                                                    shinycssloaders::withSpinner(visNetworkOutput("cocitPlot", height = "750px",width = "1100px"))),         
                                           tabPanel("Table", 
                                                    shinycssloaders::withSpinner(DT::DTOutput(
                                                      outputId = "cocitTable")))
                                           
                               )
                               #shinycssloaders::withSpinner(plotOutput(outputId = "cocitPlot"))
                             )
                             
                           )
                           
                  ), ## End of tabPanel "Co-citations"
                  
                  
                  
                  
                 tabPanel("Co-Occurrence Network",
                          
                          sidebarLayout(
                            
                            sidebarPanel(width=2,
                                         h4(em(strong("Co-Occurrence Network"))),
                                         br(),
                                         
                                         actionButton("applyCoc", "Apply!"),
                                         downloadButton("network.coc", "Save Pajek"),
                                         downloadButton("networkCoc.fig", "Save Fig"),
                                         "  ",
                                         "  ",
                                         h4(em(strong("Network Parameters: "))),
                                         "  ",
                                         selectInput("field", 
                                                     label = "Field",
                                                     choices = c("Keywords Plus" = "ID", 
                                                                 "Author's Keywords" = "DE",
                                                                 "Titles" = "TI",
                                                                 "Abstracts" = "AB"),
                                                     selected = "ID"),
                                         
                                         selectInput("layout", 
                                                     label = "Network Layout",
                                                     choices = c("Automatic layout"="auto", 
                                                                 "Circle"="circle",
                                                                 "Fruchterman & Reingold"="fruchterman",
                                                                 "Kamada & Kawai"="kamada",
                                                                 "MultiDimensional Scaling"="mds",
                                                                 "Sphere"="sphere",
                                                                 "Star"="star"),
                                                     selected = "auto"),
                                         
                                         selectInput("normalize", 
                                                     label = "Normalization",
                                                     choices = c("none", 
                                                                 "association",
                                                                 "jaccard", 
                                                                 "salton",
                                                                 "inclusion",
                                                                 "equivalence"),
                                                     selected = "association"),
                                         selectInput("cocyears",
                                                     label = "Node Color by Year",
                                                     choices = c("No" = "No",
                                                                 "Yes"= "Yes"),
                                                     selected = "No"),
                                         selectInput("cocCluster", 
                                                     label = "Clustering Algorithm",
                                                     choices = c("None" = "none",
                                                                 "Edge Betweenness" = "edge_betweenness",
                                                                 "InfoMap" = "infomap",
                                                                 "Leading Eigenvalues" = "leading_eigen",
                                                                 "Louvain" = "louvain",
                                                                 "Spinglass" = "spinglass",
                                                                 "Walktrap" = "walktrap"),
                                                     selected = "louvain"),
                                         
                                         sliderInput(inputId = "Nodes",
                                                     label = "Number of Nodes",
                                                     min = 5,
                                                     max = 1000,
                                                     value = 50),
                                         
                                         selectInput(inputId ="coc.isolates",
                                                     label = "Remove Isolated Nodes",
                                                     choices = c("Yes" = "yes",
                                                                 "No" = "no"),
                                                     selected = "no"),
                                         
                                         numericInput("edges.min", 
                                                      label=("Min edges"),
                                                      value = 2,
                                                      step = 1,
                                                      min = 0),
                                         #uiOutput("Focus"),
                                         "  ",
                                         h4(em(strong("Graphical Parameters: "))),
                                         "  ",
                                         sliderInput(inputId = "cocAlpha",
                                                     label = "Opacity",
                                                     min = 0,
                                                     max = 1,
                                                     value = 0.7,
                                                     step=0.05),
                                         sliderInput(inputId = "Labels",
                                                     label = "Number of labels",
                                                     min = 0,
                                                     max = 1000,
                                                     value = 50),
                                         selectInput(inputId ="label.cex",
                                                     label = "Label cex",
                                                     choices = c("Yes", 
                                                                 "No"),
                                                     selected = "Yes"),
                                         
                                         sliderInput(inputId = "labelsize",
                                                     label = "Label size",
                                                     min = 0.0,
                                                     max = 20,
                                                     value = 6,
                                                     step = 0.10),
                                         
                                         selectInput(inputId ="coc.shape",
                                                     label = "Node Shape",
                                                     choices = c("Box"="box",
                                                                 "Circle"="circle",
                                                                 "Database"="database",
                                                                 "Ellipse"="ellipse",
                                                                 "Text"="text"),
                                                     selected = "box"),
                                         sliderInput(
                                           inputId = "edgesize",
                                           label = "Edge size",
                                           min = 0.1,
                                           max = 20,
                                           value = 5), 
                                         
                                         selectInput(inputId ="coc.curved",
                                                     label = "Curved edges",
                                                     choices = c("Yes",
                                                                 "No"),
                                                     selected = "No")
                                         
                                         
                            ),
                            
                            mainPanel(
                              tabsetPanel(type = "tabs",
                                          tabPanel("Map", 
                                                   shinycssloaders::withSpinner(visNetworkOutput("cocPlot", height = "750px",width = "1100px"))),
                                          tabPanel("Table", 
                                                   shinycssloaders::withSpinner(DT::DTOutput(
                                                     outputId = "cocTable")))
                              )
                              
                            )
                          )
                 ) ## End of tabPanel ("CS network")
                 
                  ),#CLOSE NETWORK NAVBAR 
                  
                  
                                               
                  
                      
                  navbarMenu("Quit",
                             tabPanel(actionLink("stop_radiant", "Stop", icon = icon("power-off"), 
                                                 onclick = "setTimeout(function(){window.close();}, 100); ")
                             ))
                             
                  
                               
                    
                      
                      
                        ## color of datatable
                       #tags$head(tags$style(HTML("table.dataTable.hover tbody tr:hover, table.dataTable.display tbody tr:hover {
                      #             background-color: #9c4242 !important;
                      #             }
                      #             "))),
                      #   tags$style(HTML(".dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing,.dataTables_wrapper .dataTables_paginate .paginate_button, .dataTables_wrapper .dataTables_paginate .paginate_button.disabled {
                      # color: #000000 !important;
                      # }")),
                        #shinycssloaders::withSpinner(tableOutput("contents"))
                       
                        #shinycssloaders::withSpinner(DT::DTOutput("contents"))
                      )
                   


                  
                