#R
# This is the server logic for a Shiny web application using recmap
#
# https://CRAN.R-project.org/package=recmap

# use R version >= 4.3 - grDevices::hcl; grDevices::hcl.colors; grDevices::hcl.pals;
# library(colorspace)

stopifnot(require(GA),
          require(maps),
          require(recmap))


if (Sys.info()['machine'] == "arm64" && Sys.info()['sysname'] == "Darwin"){
  stopifnot(isFALSE(GA::gaControl()$useRcpp))
}

# ----- get U.S. county minimal bounding boxes  ------
.get_county_mbb <-
  function(state = 'colorado',
           scaleX = 0.5,
           scaleY = 0.5) {

    if (require(noncensus))
        data(counties)
    else
        stop("no package 'noncensus'.")

    # sanity check
    if (!state %in%  row.names(state.x77)) {
      warning("not a valid U.S. state")
      return(NULL)
    }
    MBB <- lapply(map('county', state, plot = FALSE)$names,
                  function(x) {
                    r <- map('county', x, plot = FALSE)
                    dx <- scaleX * (r$range[2] - r$range[1])
                    dy <- scaleY * (r$range[4] - r$range[3])
                    x <- r$range[1] + dx
                    y <- r$range[3] + dy
                    data.frame(
                      polyname = r$name,
                      x = x,
                      y = y,
                      dx = dx,
                      dy = dy
                    )
                  })
    MBB <- do.call('rbind', MBB)
    MBB <- merge(MBB, county.fips, by = 'polyname')
    MBB$fips <- as.integer(MBB$fips)

    P <- data.frame(
      fips = paste(counties$state_fips,
                   counties$county_fips, sep = ''),
      z = counties$population,
      name = counties$county_name
    )
    P$fips <- as.integer(levels(P$fips))[P$fips]

    M <- merge(MBB, P, by = 'fips')
    attr(M, 'Map.name') <- paste("U.S.", state)
    attr(M, 'Map.stat') <- 'population'
    class(M) <- c('recmap', 'data.frame')
    M
  }

# ----- shiny server -------
shinyServer(function(input, output, session) {
  output$plot_hoverinfo <- renderText({

    if (TRUE){
      res <- Cartogram()$Cartogram
      x <- input$plot_hover$x
      y <- input$plot_hover$y

      query <-
        ((res$x - res$dx) < x) &
        (x < (res$x + res$dx)) &
        ((res$y - res$dy) < y) & (y < (res$y + res$dy))
      rv <- "no object identified."
      if (sum(query) == 1) {
        rv <- paste(res$name[query])
      }

      message(paste("region name:", rv))
    }
  })

  #---- define colormaps ----
  # some taken from the vignette of https://CRAN.R-project.org/package=colorspace
  colormap <- reactive({
    
    # heat_hcl <- heat_hcl(12, c = c(80, 30), l = c(30, 90), power =  c(1/5, 2))
    
    heat_hcl <- c('#8E063B', '#A63945', '#BC584D', '#CF7355', '#DF8B5B',
                  '#EAA162', '#F2B468', '#F6C56F', '#F6D277', '#F2DD80',
                  '#EBE48B', '#E2E6BD')
    
    
    # sequential_hcl(12, c = 0, power = 2.2)
    # sequential_hcl(12, power = 2.2)
    # sequential_hcl(12, c = 0, power = 2.2)
    
    list(
        colorspace_heat_hcl = heat_hcl,
         colorspace_rev_heat_hcl = rev(heat_hcl),
         colorspace_Set2 = hcl.colors(20, "Set 2"),
         colorspace_Set3 = hcl.colors(20, "Set 3"),
         colorspace_rev_Set3 = rev(hcl.colors(20, "Set 3")),
         heat.colors = heat.colors(12),
        rainbow = rainbow(12),
# use DBVIS color maps;
# KEIM, Daniel, 1995. Visual support for query specification and data mining [Dissertation].
# München: Universität. Aachen : Shaker. ISBN 3-8265-0594-8goes back to 1999
         DanKeim = rev(c('#C6CF32', '#88E53B', '#50E258', '#29C67D', '#19999E',
                         '#2064AF', '#3835AB', '#561493', '#6E086D', '#790D43',
                         '#741F1E', '#5F3307')
         ),
         DanKeim_HSV = rev(c('#BECC3D', '#70C337', '#31B93C', '#2BB077',
                             '#269FA7', '#21589E', '#221C94', '#56188B',
                             '#82147F', '#791043', '#6F0E0D', '#66380A')))
  })

  output$colormap <- renderUI({
    selectInput('colormapname', 'colormap name', names(colormap()))
  })

  output$colormapPlot <- renderPlot({
    par(mar = c(0,0,0,0));
    pal(unlist(colormap()[input$colormapname]))
  })

  #---- define output UI  ----
  output$methodRadio <- renderUI({
    inputData <- c("checkerboard" = "checkerboard",
                   "US state" = "USstate")
    
    if (require('noncensus')){
      inputData <- c("checkerboard" = "checkerboard",
                     "US county" = "UScounty",
                     "US state" = "USstate")
    }
   
    
	  radioButtons("datatype", "Type of data:",
	               inputData)
  })
  
  output$II <- renderUI({
    if (input$datatype == 'checkerboard'){
      numericInput("checkerboardSize", "checkerboardSize", 4, min = 2,
                   max = 16, step = 1)
    }else if (input$datatype == 'USstate'){
      list(
        selectInput('area', 'area', colnames(state.x77)),
        selectInput('color', 'color', colnames(state.x77)),
        htmlOutput("colormap")
        )
    }else if (input$datatype == 'UScounty'){
      list(
        selectInput('state', 'U.S. state', row.names(state.x77), "Louisiana"),
        helpText('overlap to compose pseudo dual:'),
        sliderInput("scaleX", "scaleX", 0, 1, 0.6),
        sliderInput("scaleY", "scaleY", 0, 1, 0.63))
    }})

  #---- define input map ----
  Map <-  reactive({
    progress <- shiny::Progress$new(session = session)
    progress$set(message = "get input map")
    on.exit(progress$close())

    res <- NULL

    if (input$datatype == "UScounty" && length(input$state) == 1){
      res <-
        .get_county_mbb(
          state = input$state,
          scaleX = input$scaleX,
          scaleY = input$scaleY
        )
      res$name <- gsub(" ", "\n", res$name)
      res}else if (input$datatype == "checkerboard" && length(input$checkerboardSize) == 1){
        res <- checkerboard(input$checkerboardSize)
      }else if (input$datatype == "USstate"){
        usa <- data.frame(x = state.center$x,
                          y = state.center$y,
                          # make the rectangles overlapping by correcting lines of longitude distance
                          dx = sqrt(state.area) / 2 / (0.8 * 60 * cos(state.center$y*pi/180)),
                          dy = sqrt(state.area) / 2 / (0.8 * 60),
                          z = sqrt(state.area),
                          name = state.name)

        usa$z <- state.x77[, input$area]

        res <- usa[!usa$name %in% c("Hawaii", "Alaska"), ]
        attr(res, 'Map.name') <- "U.S."
        attr(res, 'Map.stat') <- input$area

        class(res) <- c('recmap', class(res))
      }
    res
  })

  #---- define fitness function ----
  wfitness <- function(idxOrder, Map,  ...) {
    Cartogram <- recmap(Map[idxOrder,])

    if (sum(Cartogram$topology.error == 100) > 0) {
      return(0)
    }

    1 / (c(input$objective_weight,
        1 - input$objective_weight) %*% c(S['topology error',] / nrow(Cartogram)^2,
                                          S['relative position error',]))
  }

  #---- compute Cartogram ----
  Cartogram <- reactive({
    progress <- shiny::Progress$new(session = session, min = 0, max = 1)
    progress$set(message = "recmapGA init")
    on.exit(progress$close())

    options(warn = -1)

    M <- Map()
    if (is.null(M)){return(NULL)}
    time.elapsed <- rep(proc.time()[3], input$GAmaxiter)

    res <- recmapGA(
      M,
      maxiter = input$GAmaxiter,
      popSize = input$GApopulation * nrow(M),
      pmutation = input$GApmutation,
      run = input$GArun,
      seed = input$seed,
      parallel = input$parallel,
      monitor = function(object, digits = getOption("digits"), ...)
      {
        fitness <- na.exclude(object@fitness)
        sumryStat <- c(mean(fitness), max(fitness))

        time.elapsed[object@iter] <- ( proc.time()[3] - time.elapsed[object@iter] ) / object@iter
        progress$set(
          message = "GA",
          detail = paste(
            "iteration",
            object@iter, "/", input$GAmaxiter,
            "fittest = ",
            round(sumryStat[2], 5),
            "\n elapsed time / generation", round(time.elapsed[object@iter], 2), "in secs"
          ),
          value = object@iter / input$GAmaxiter
        )
      }
    )
    res
  })

  #----plot input map ----
  output$mapPlot <- renderPlot({
    message("Plotting map ...")
    M <- Map()
    if (is.null(M)){return()}

    op <- par(mfrow = c(1, 1), mar = c(0, 0, 0, 0))

    recmap::plot.recmap(M, col.text = 'darkred')

    x <- input$plot_hover$x
    y <- input$plot_hover$y
    if(TRUE){
      C <- Cartogram()
      if(is.null(C)) {return()}
      res <- C$Cartogram

      query <-
        ((res$x - res$dx) < x) &
        (x < (res$x + res$dx)) &
        ((res$y - res$dy) < y) & (y < (res$y + res$dy))
      rv <- "no object identified."
      if (sum(query) == 1) {
        rv <- paste(res$name[query])
        idx <- which(M$name == rv)
        rect(M$x[idx] - M$dx[idx],
             M$y[idx] - M$dy[idx] ,
             M$x[idx] + M$dx[idx],
             M$y[idx] + M$dy[idx],
             col = rgb(0.8, 0.1, 0.1, 0.3))
      }}
  })

  #----plot output cartogram ----
  output$cartogramPlot <- renderPlot({
    res <- Cartogram()
    if (is.null(res)){return(NULL)}
    op <- par(mfrow = c(1, 1), mar = c(0, 0, 0, 0))

    if(input$datatype == "USstate"){
      cm <- unlist(colormap()[input$colormapname])

      S <- state.x77[which(rownames(state.x77) %in% res$Cartogram$name), input$color]
      S <- S[res$GA@solution[1, ]]
      S <- round((length(cm) - 1) * (S - min(S)) / (max(S)  - min(S))) + 1
      plot(res, col=cm[S], col.text = 'black')
      legend("topleft", c(paste("Area ~", input$area),
                          paste("Color ~", input$color)),
             box.col = 'white')

    }else{
      plot(res$Cartogram, col.text = 'darkred')}

  })

  output$gaPlot <- renderPlot({
      plot(Cartogram()$GA)
  })

  output$gaSolution <- DT::renderDataTable({
    t(Cartogram()$GA@solution)
  })

  output$summary <- DT::renderDataTable({
    res <- Cartogram()
    t(res$Summary)
  })

  # ------- pdf -------
  output$foo = downloadHandler(
    filename = paste("recmap.pdf", sep = ''),
    content = function(file) {
      pdf(file, 12, 12)
      res <- Cartogram()
      plot.recmap(
        res$Cartogram,
        col.text = 'darkred',
        sub = paste(
          'U.S. state',
          input$state,
          '- rectangular cartogram generated by using https://CRAN.R-project.org/package=recmap version',
          packageVersion('recmap')
        )
      )
      dev.off()
      #plotInput()
      #dev.copy2pdf(file = file, width=12, height=8, out.type="pdf")
    }
  )

  #---- sessionInfo ----
  output$sessionInfo <- renderPrint({
    capture.output(sessionInfo())
  })
})
