function(input, output) {
  
   #select report data based on drug or device (pipeline category not available yet)
    
    pipeline_selected<-reactive({
      
     
      
    if(input$tab=='drug'){
        df<-pipeline%>%filter(`Product category`=='Drug')
      }else{
       df<-pipeline%>%filter(`Product category`=='Device')
      }
      
      return(df)
    })
    
    
    report_selected<-reactive({
      
      if(input$tab=='drug'){

        df<-report%>%filter(`Product type`=='Drug')
      
      }else{
        
        df<-report%>%filter(`Product type`=='Device')
      }
     
      
      return(df)
    })
  
    
  
    callModule(timeline_server,'drug_timeline',reactive(pipeline_selected()))
  
    callModule(published_server,'drug_published',reactive(report_selected()))
    
    # output$published_table <- renderDataTable(stages_late,
    #     options = list(dom = "t"), rownames = FALSE,
    #         colnames = c("Stage", "Packages Historically Late"))
    
    
    callModule(inprogress_server,'drug_inprogress',reactive(report_selected()))
    
   
    callModule(timeline_server,'device_timeline',reactive(pipeline_selected()))
    
    callModule(published_server,'device_published',reactive(report_selected()))
    
    callModule(inprogress_server,'device_inprogress',reactive(report_selected()))
    
    output$download <- downloadHandler (
      filename = "PRCI_report.pdf",
      content = function(file) {
        src <- normalizePath("report.Rmd")
        on.exit(setwd(tempdir()))
        file.copy(src, "report.Rmd", overwrite = TRUE)
        out <- render("report.Rmd", pdf_document())
        file.rename(out, file)
      }
    )
    
    # output$timeplot_old <- renderPlotly({
    #     
    #     df <- pipeline %>%
    #         filter(Publication <= Sys.Date() + 90)
    # 
    #     #positions <- c(0.2, -0.2, 0.4, -0.4, 0.5, -0.5, 0.9, -0.9, 1.25, -1.25)
    #     positions <- c(0.1, -0.1, 0.2, -0.2, 0.3, -0.3, 0.4, -0.4, 0.5, -0.5, 
    #         0.6, -0.6, 0.7, -0.7, 0.8, -0.8, 0.9, -0.9, 1, -1)
    #     directions <- c(1, -1)
    # 
    #     line_pos <- data.frame(
    #         "date" = sort(unique(df$Publication), na.last = T),
    #         "position" = rep(positions,
    #             length.out = length(unique(df$Publication))),
    #         "direction" = rep(directions,
    #             length.out = length(unique(df$Publication)))
    #     )
    # 
    #     df <- left_join(df, line_pos, by = c('Publication' = 'date'))
    #     text_offset <- 0.1
    # 
    #     df$month_count <- ave(df$Publication == df$Publication, df$Publication,
    #         FUN = cumsum)
    #     df$text_position <- (df$month_count * text_offset * df$direction) +
    #         df$position
    # 
    #     month_buffer <- 2
    # 
    #     month_date_range <- seq(min(df$Publication, na.rm = T) - months(month_buffer),
    #         max(df$Publication, na.rm = T) + months(month_buffer), by = 'month')
    #     month_df <- data.frame(month_date_range)
    #     month_df$month_format <- paste0(year(month_df$month_date_range), ' ', 
    #         quarters(month_df$month_date_range))
    #     month_df$month_format <- ifelse(
    #         month_df$month_format == lag(month_df$month_format, default = ''),
    #         '', month_df$month_format)
    # 
    #     timeline_plot <- ggplot(df, aes(x = Publication, y = 0,
    #             label = Product), color = "blue") +
    #         theme_classic() +
    #         geom_hline(yintercept = 0, color = "black", size = 0.3) +
    #         # Plot vertical segment lines for milestones
    #         geom_segment(data = df[df$month_count == 1,], aes(y = position,
    #             yend = 0, xend = Publication), color = 'black', size = 0.1) +
    #         geom_point(aes(y = 0), size = 3) +  # Plot scatter points at zero and date
    #         geom_text(data = month_df, aes(x = month_date_range, y = -0.1,
    #             label = month_format), vjust = 0.5, color = 'black', size = 3) +
    #         geom_text(aes(y = text_position, label = Product), size = 3)+
    #         theme(
    #             axis.line.y = element_blank(),
    #             axis.text.y = element_blank(),
    #             axis.title.x = element_blank(),
    #             axis.title.y = element_blank(),
    #             axis.ticks.y = element_blank(),
    #             axis.text.x = element_blank(),
    #             axis.ticks.x = element_blank(),
    #             axis.line.x = element_blank(),
    #             legend.position = "bottom"
    #         )
    #     
    #     timeline_plotly <- ggplotly(timeline_plot, dynamicTicks = TRUE,
    #         height = 600)
    #   
    #     timeline_plotly
    #       
    # })
    
  
}