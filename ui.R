# Define UI for application that draws a histogram
ui <- navbarPage("MD-FIS",
                 
                 # Application title
                 # titlePanel("Upload dataset"),
                 
                 tabPanel("Prepare",
                     verticalLayout(
                       
                       h2("Upload Dataset"),
                       
                       fileInput("dataset", "Choose CSV File (Title required)",
                                 multiple = FALSE,
                                 accept = c("text/csv",
                                            "text/comma-separated-values,text/plain",
                                            ".csv")),
                       
                       actionButton("preview", "Preview"),
                       
                       tags$hr(),
             
                       h2("Dataset Shape: "),
                       textOutput("shape"),
                       tags$hr(),
                       
                       h2("Dataset Preview:"),
                       
                       tableOutput("contents")
                       
                     )
                 ),
                 
                 tabPanel("Normalization",
                          # Sidebar with a slider input for number of bins 
                          verticalLayout(
                            
                            h1("Normalization Parameters"),
                            
                            numericInput("labelfrom", 
                                         h3("Label from"), 
                                         value = 1),
                            
                            numericInput("labelto", 
                                         h3("Label To"), 
                                         value = 30),
                            
                            checkboxInput("labely", label = "Normalize Labels", value = FALSE),
                            
                            actionButton("normalization", "Normalize"),
                            
                            tags$hr(),
                            
                            h1("Normalizated Preview:"),
                            tableOutput("normalizatedDataset"),
                            
                            textOutput("nstatus")
                            
                          )
                 ),
                 tabPanel("Splitting",
                          
                          verticalLayout(
                            
                            h1("Filter Parameters"),
                            
                            numericInput("groupfrom", 
                                         h3("Group from"), 
                                         value = 1),
                            
                            numericInput("groupto", 
                                         h3("Group To (8, 9, 10 or 11)"), 
                                         value = 1),
                                                        
                            
                            sliderInput("filtersize", h3("Filter size:"),
                                        min = 0, max = 10, value = 3),
                            
                            sliderInput("boundary", h3("Abnormal Data:"),
                                        min = 0, max = 100, value = c(20, 80)),
                            
                            checkboxInput("smallgroupfilter", label = "Small Group Filter", value = TRUE),
                            checkboxInput("abnormaly", label = "Label Abnormal Filter", value = FALSE),
                            
                            
                            actionButton("filter", "Filter Dataset"),
                            
                            textOutput("filterstatus"),
                     
                            tags$hr(),
                            
                            h1("Split Parameters:"),
                            
                            numericInput("repeatInitSet", 
                                         h3("Initset Repeat Times"), 
                                         value = 1000),
                            
                            numericInput("weight", 
                                         h3("Group Weigth"), 
                                         value = 0.5),
                            
                            sliderInput("devsetsize", h3("Devset Size (%):"),
                                        min = 0, max = 100, value = 20),
                            
                            textOutput("devsetsizelabel"),
                          
                            
                            sliderInput("testsetsize", h3("TestSet Size (%):"),
                                        min = 0, max = 100, value = 20),
                            
                            textOutput("testsetsizelabel"),
                            
                            actionButton("splitdev", "Split Devset"),
                            
                            actionButton("splittest", "Split Testset"),
                            
                            textOutput("splitstatus"),
                            
                            tags$hr(),
                            
                            h1("Datasets preview and download:"),
                            
                            # Input: Selector for choosing dataset ----
                            selectInput(inputId = "chosendataset",
                                        label = "Choose a dataset:",
                                        choices = c("trainSet", "devSet", "testSet")),
                            
                            actionButton("finalpreview", "Preview Finalset"),
                            
                            # Button
                            downloadButton("downloadData", "Download"),
                            
                            # Preview
                            tableOutput("spliteddata")
                            
                          )            
                          
                 )
)