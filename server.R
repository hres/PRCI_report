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
    
}