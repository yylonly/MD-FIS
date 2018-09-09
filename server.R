source("md.R")

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #conformscaledata <- NULL

  
  output$contents <- renderTable({
    
    previewUpload()
    
  })
    
    
  previewUpload <-  eventReactive(input$preview, {
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$dataset)
    
    print("preview start")
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      
      {
        
        df <- read.csv(input$dataset$datapath)
        d <- dim(df)
        datasetr <<- d[1]
        datasetc <- d[2]
        
        output$shape <- renderText({ 
          req(input$dataset) 
          paste("row:", datasetr, "column:", datasetc)})
     
      },
      
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
      
    )
    
    print("preview finish")
    return(df)
    
  })
  
  
  ## Selete Dataset
  datasetSeleted <- reactive({
    switch(input$chosendataset,
           "trainSet" = FinalTrainingSet,
           "devSet" = FinalDevSet,
           "testSet" = FinTestingSet)
    
  })
  

  ## final preview
  observeEvent(input$finalpreview, {
    
    # Table of selected dataset ----
    output$spliteddata <- renderTable({
      
      datasetSeleted()
      
    })
    
  })
  
  
  
  
  output$devsetsizelabel <- renderText({dsize <<- round(input$devsetsize*0.01*datasetr)})
  
  output$testsetsizelabel <- renderText({tsize <<- round(input$testsetsize*0.01*datasetr)})
  


  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(

    
    filename = function() {
        paste(input$chosendataset, ".csv", sep = "")
    },
    
    content = function(file) {
      write.csv(datasetSeleted(), file, row.names = FALSE)
    }
    
  )
  
  
  
  ##scale range from 0 to 1
  observeEvent(input$normalization, {

    print("normalization start")
    
    allX <- read.csv(input$dataset$datapath)
    
    alldata <<- data.matrix(allX)
    
    if (input$labely == TRUE) 
        y <- alldata[, input$labelfrom:input$labelto]/100
    else
        y <- alldata[, input$labelfrom:input$labelto]
    
    X <- alldata[, 1:input$labelfrom-1]
    
    maxs <- apply(X, 2, max)
    mins <- apply(X, 2, min)
    ranges <- maxs - mins
    # if all value is 0, range will be 0
    ranges[ranges == 0] = 1
    means <- apply(X, 2, mean)
    
    scaledallx <<- scale(X, center = mins, scale = ranges)
    # access from outside ----
    scaleddata <<- cbind(scaledallx, y)
    
    output$normalizatedDataset <- renderTable(scaleddata)
    
    print("normalization finish")
    
  })
  
  
  ## filter action
  observeEvent(input$filter, {
    
    print("filter start")
    output$filterstatus <-  renderText("filter start")
    
    conformscaledata <<- scaleddata
    
    ## 1. get rid of groups without [20, 80] 
    if (input$abnormaly == TRUE) {
      
      print("boundary:")
      print(input$boundary)
  
      index80 <- which(rowSums(scaleddata[, input$labelfrom:input$labelto] >= (input$boundary[2]/100)) == 2)
      index20 <- which(rowSums(scaleddata[, input$labelfrom:input$labelto] <= (input$boundary[1]/100)) == 2)
      index <- c(index20, index80)
      filledscaleddata <<- scaleddata[index, ]
      scaleddata <<- scaleddata[-index, ]
      
      scaledallx <- scaledallx[-index, ]
      
      big80alldata <- alldata[index80, ]
      alldata <- alldata[-index80, ]
    
    }
    
    
    ## 2. get rid of groups less 3 
    if (input$smallgroupfilter == TRUE) {
      
      print("small group size:")
      print(input$filtersize)
      
      onlygroupdata <- data.frame(scaledallx[, input$groupfrom:input$groupto])
      onlygroupdatastatic <- aggregate(list(numdup=rep(1, nrow(onlygroupdata))), onlygroupdata, length)
      
      order_onlygroupdatastatic <- onlygroupdatastatic[order(onlygroupdatastatic$numdup, decreasing = FALSE), ]
      #filtered data
      filtered_group <- filter(order_onlygroupdatastatic, numdup <= input$filtersize)
        
      #get filtered data index  
      conformIndex <- which(is.na(row.match(data.frame(scaleddata[, input$groupfrom:input$groupto]), filtered_group[, input$groupfrom:input$groupto])))
        
      conformscaledata <- scaleddata[conformIndex, ]
      less3scaleddata <<- scaleddata[-conformIndex, ]
        
      conformalldata <<- alldata[conformIndex, ]
      less3conformalldata <- alldata[-conformIndex, ]
    }
    
    #$spliteddata <- renderTable(conformscaledata)
  
    print("filter finish")
    output$filterstatus <-  renderText("filter finish")
    
  })
  
  
  ## splitdev action
  observeEvent(input$splitdev, {
    
    print("split start")
    output$splitstatus <-  renderText("splitdev start")
    
    
    ## Get best inital dataset
    numbers <- dim(conformscaledata)[1];
    
    allIndexes <- NULL
    allsumdiss <- NULL
    
    times <- choose(numbers, 5)
    
    ## Generate 10000 intial data set and get best one
    for (i in 1:input$repeatInitSet) {
      
      ## A random sample of 5 data points
      set.seed(i)
      initalIndexes <- sample(numbers, 5)
      
      TrainningSet <- conformscaledata[-initalIndexes, ]
      initalTestSet <- conformscaledata[initalIndexes, ]
      
      allIndexes <- rbind(allIndexes, initalIndexes)
      
      diss <- proxy::dist(initalTestSet, TrainningSet)
      sumdiss <- sum(diss)
      allsumdiss <- c(allsumdiss, sumdiss)
      
    }
    
    bestInitalIndex <- allIndexes[which.min(allsumdiss), ]
    bestDistance <- min(allsumdiss)
    
    #Begin compute remaining testset
    RemainingSet <- conformscaledata[-bestInitalIndex, ]
    initalSet <- conformscaledata[bestInitalIndex, ]
    
    #split dataset
    SelectedIndex <- maxDissim(initalSet, RemainingSet, n = (dsize-5), obj = minDiss, alpha = input$weight, groupsize = input$groupto)
    SelectedSet <- RemainingSet[SelectedIndex, ]
    
    #training set and selected set
    FinTestingSet <<- rbind(initalSet, SelectedSet)
    
    if (input$abnormaly == TRUE & input$smallgroupfilter == TRUE) {
        FinTrainingSet <<- rbind(RemainingSet[-SelectedIndex, ], less3scaleddata, filledscaleddata)
    } else if (input$smallgroupfilter == TRUE) {
        FinTrainingSet <<- rbind(RemainingSet[-SelectedIndex, ], less3scaleddata)
    } else {
        FinTrainingSet <<- rbind(RemainingSet[-SelectedIndex, ])
    }
    
    print("split finish")
    output$splitstatus <-  renderText(paste("splitdev finish"))
  })
  
  
  ## splittest action
  observeEvent(input$splittest, {
    
    print("splittest start")
    output$splitstatus <-  renderText("splittest start")
    
    
    ## Get best inital dataset
    numbers = dim(FinTrainingSet)[1];
    
    allIndexes <- NULL
    allsumdiss <- NULL
    
    times <- choose(numbers, 5)
    
    ## Generate 10000 intial data set and get best one
    for (i in 1:input$repeatInitSet) {
      
      ## A random sample of 5 data points
      set.seed(i)
      initalIndexes <- sample(numbers, 5)
      
      TrainningSet <- FinTrainingSet[-initalIndexes, ]
      initalTestSet <- FinTrainingSet[initalIndexes, ]
      
      allIndexes <- rbind(allIndexes, initalIndexes)
      
      diss <- proxy::dist(initalTestSet, TrainningSet)
      sumdiss <- sum(diss)
      allsumdiss <- c(allsumdiss, sumdiss)
      
    }
    
    bestInitalIndex <- allIndexes[which.min(allsumdiss), ]
    bestDistance <- min(allsumdiss)
    
    #Begin compute remaining testset
    RemainingSet <- FinTrainingSet[-bestInitalIndex, ]
    initalSet <- FinTrainingSet[bestInitalIndex, ]
    
    #split dataset
    SelectedIndex <- maxDissim(initalSet, RemainingSet, n = (tsize-5), obj = minDiss, alpha = input$weight, groupsize = input$groupto)
    SelectedSet <- RemainingSet[SelectedIndex, ]
    
    #training set and selected set
    FinalDevSet <<- rbind(initalSet, SelectedSet)
    
    if (input$abnormaly == TRUE & input$smallgroupfilter == TRUE) {
      FinalTrainingSet <<- rbind(RemainingSet[-SelectedIndex, ], less3scaleddata, filledscaleddata)
    } else if (input$smallgroupfilter == TRUE) {
      FinalTrainingSet <<- rbind(RemainingSet[-SelectedIndex, ], less3scaleddata)
    } else {
      FinalTrainingSet <<- rbind(RemainingSet[-SelectedIndex, ])
    }
    
    print("splittest finish")
    output$splitstatus <-  renderText("splittest finish")
  })
  
}