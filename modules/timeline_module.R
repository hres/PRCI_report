#module 1 projected publication

timelineUI<-function(id){
    ns<-NS(id)
    
    
    fluidRow(
        column(width = 12, offset = 0.5, 
               tags$b("Projected Publication Dates", style = 'font-size:30px;'),
               br(),
               plotlyOutput(ns("timeplot"),height = "450px") %>% withSpinner())
    )
    
}


timeline_server<-function(input,output,session,pipeline) {
    
    output$timeplot <- renderPlotly({
        
        min_date <- min(pipeline()$Date, na.rm = TRUE)
        max_date <- max(pipeline()$Date, na.rm = TRUE)
        dates <- seq(min_date, max_date, "months")
        
        positions <- c(0.2, -0.2, 0.4, -0.4, 0.6, -0.6, 0.8, -0.8, 1, -1)
        directions <- c(1, -1)
        
        line_pos <- data.frame(
            "date" = sort(unique(pipeline()$Date), na.last = T),
            "position" = rep(positions,
                             length.out = length(unique(pipeline()$Date))),
            "direction" = rep(directions,
                              length.out = length(unique(pipeline()$Date)))
        )
        
        pipeline <- left_join(pipeline(), line_pos,
                              by = c('Date' = 'date'))
        text_offset <- 0.25
        
        pipeline$month_count <- ave(
            pipeline$Date == pipeline$Date, pipeline$Date,
            FUN = cumsum)
        pipeline$text_position <-
            (pipeline$month_count * text_offset * pipeline$direction) +
            pipeline$position
        
        annotations <- list()
        annotation <- list()
        for (i in 1:nrow(pipeline)) {
            r <- runif(1, -250, -50)
            ay <- ifelse(runif(1, 0, 1) < 0.5, abs(r), r)
            annotations[[i]] <- list(x = pipeline$Date[i],
                                     y = 0, ax = 0, ay = pipeline$text_position[i] * 120,
                                     text = pipeline$Products[i], arrowhead = 6)
        }
        
        plot_ly(height = 400) %>%
            add_trace(uid = "d6088d", mode = "none", name = "Col2",
                      type = "scatter", x = dates, y = rep(0, length(dates)),
                      hoverinfo = "none", showlegend = FALSE) %>%
            layout(xaxis = list(range = c(min_date - months(1), min_date + months(6))),
                   yaxis = list(type = "linear", range = c(-10, 10),
                                showgrid = FALSE, showline = FALSE, tickmode = "auto",
                                zeroline = TRUE, autorange = FALSE, showticklabels = FALSE),
                   autosize = TRUE, annotations = annotations)
        
    })
    
}