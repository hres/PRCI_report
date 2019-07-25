function(input, output) {
    
    output$published <- renderValueBox({
        n <- report %>%
            filter(state == "Published") %>%
            nrow()
        valueBox(n, "Total Number of Packages Published",
            icon=icon('check-circle'), "light-blue")
    })
    
    output$published_proactive <- renderValueBox({
        n <- report %>%
            filter(state == "Published", proactive == 1) %>%
            nrow()
        valueBox(n, "Proactive", NULL, "light-blue")
    })
    
    output$published_request <- renderValueBox({
        n <- report %>%
            filter(state == "Published", proactive == 0) %>%
            nrow()
        valueBox(n, "On Request", NULL, "light-blue")
    })
    
    output$published_late <- renderValueBox({
        n <- report %>%
            filter(state == "Published", late == 1) %>%
            nrow()
        valueBox(n, "Number of Packages Published Late",
            icon=icon('exclamation-circle'), "light-blue")
    })
    
    output$published_late_proactive <- renderValueBox({
        n <- report %>%
            filter(state == "Published", late == 1, proactive == 1) %>%
            nrow()
        valueBox(n, "Proactive", NULL, "light-blue")
    })
    
    output$published_late_request <- renderValueBox({
        n <- report %>%
            filter(state == "Published", late == 1, proactive == 0) %>%
            nrow()
        valueBox(n, "On Request", NULL, "light-blue")
    })
    
    output$published_table <- renderDataTable(stages_late,
        options = list(dom = "t"), rownames = FALSE,
            colnames = c("Stage", "Packages Late"))
    
    output$in_progress <- renderValueBox({
        n <- report %>%
            filter(state != "Published") %>%
            nrow()
        valueBox(n, "Total Number of Packages In Progress", 
            icon=icon('check-circle'), "light-blue")
    })
    
    output$in_progress_proactive <- renderValueBox({
        n <- report %>%
            filter(state != "Published", proactive == 1) %>%
            nrow()
        valueBox(n, "Proactive", NULL, "light-blue")
    })
    
    output$in_progress_request <- renderValueBox({
        n <- report %>%
            filter(state != "Published", proactive == 0) %>%
            nrow()
        valueBox(n, "On Request", NULL, "light-blue")
    })
    
    output$in_progress_late <- renderValueBox({
        n <- report %>%
            filter(state != "Published", late == 1) %>%
            nrow()
        valueBox(n, "Number of Packages In Progress, Late",
            icon=icon('exclamation-circle'), "light-blue")
    })
    
    output$in_progress_late_proactive <- renderValueBox({
        n <- report %>%
            filter(state != "Published", late == 1, proactive == 1) %>%
            nrow()
        valueBox(n, "Proactive", NULL, "light-blue")
    })
    
    output$in_progress_late_request <- renderValueBox({
        n <- report %>%
            filter(state != "Published", late == 1, proactive == 0) %>%
            nrow()
        valueBox(n, "On Request", NULL, "light-blue")
    })
    
    output$in_progress_plot <- renderPlot({
        report %>%
            filter(state != "Published") %>%
            ggplot(aes(x = state, fill = late)) +
                geom_bar(width = 0.5) +
                scale_fill_manual(name = "Status",
                    labels = c("On Time", "Late"),
                    values = c("FALSE" = "olivedrab3", "TRUE" = "red3")) +
                geom_text(stat = "count", aes(label = ..count..),
                    position = position_stack(vjust = 0.5), size = 8) +
                labs(title = "In Progress Performance", x = "State",
                    y = "Number of Packages") +
                theme(plot.title = element_text(hjust = 0.5, size = 20),
                    axis.title = element_text(size = 15),
                    axis.text.x = element_text(size = 12),
                    legend.title = element_text(size = 15),
                    legend.text = element_text(size = 12),
                    plot.background = element_rect(fill = "white"),
                    panel.background = element_rect(fill = "white",
                        colour = "grey", size = 0.5))
    })
    
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
    
    output$timeplot <- renderPlot({
        
        df <- pipeline
  
        #positions <- c(0.2, -0.2, 0.4, -0.4, 0.5, -0.5, 0.9, -0.9, 1.25, -1.25)
        positions <- c(0.1, -0.1, 0.2, -0.2, 0.3, -0.3, 0.4, -0.4, 0.5, -0.5, 
            0.6, -0.6, 0.7, -0.7, 0.8, -0.8, 0.9, -0.9, 1, -1)
        directions <- c(1, -1)
  
        line_pos <- data.frame(
            "date" = sort(unique(df$Publication), na.last = T),
            "position" = rep(positions,
                length.out = length(unique(df$Publication))),
            "direction" = rep(directions,
                length.out = length(unique(df$Publication)))
        )
  
        df <- left_join(df, line_pos, by = c('Publication' = 'date'))
        text_offset <- 0.1
  
        df$month_count <- ave(df$Publication == df$Publication, df$Publication,
            FUN = cumsum)
        df$text_position <- (df$month_count * text_offset * df$direction) +
            df$position
  
        month_buffer <- 2
  
        month_date_range <- seq(min(df$Publication, na.rm = T) - months(month_buffer),
            max(df$Publication, na.rm = T) + months(month_buffer), by = 'month')
        month_df <- data.frame(month_date_range)
        month_df$month_format <- paste0(year(month_df$month_date_range), ' ', 
            quarters(month_df$month_date_range))
        month_df$month_format <- ifelse(
            month_df$month_format == lag(month_df$month_format, default = ''),
            '', month_df$month_format)
  
        timeline_plot <- ggplot(df, aes(x = Publication, y = 0,
                label = Product), color = "blue") +
            theme_classic() +
            geom_hline(yintercept = 0, color = "black", size = 0.3) +
            # Plot vertical segment lines for milestones
            geom_segment(data = df[df$month_count == 1,], aes(y = position,
                yend = 0, xend = Publication), color = 'black', size = 0.2) +
            geom_point(aes(y = 0), size = 3) +  # Plot scatter points at zero and date
            geom_text(data = month_df, aes(x = month_date_range, y = -0.1,
                label = month_format), vjust = 0.5, color = 'black', size = 3) +
            geom_text(aes(y = text_position, label = Product), size = 5)+
            theme(
                axis.line.y = element_blank(),
                axis.text.y = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.line.x = element_blank(),
                legend.position = "bottom"
            )
            timeline_plot
    })
    
}