#module 2 published package

publishedUI<-function(id){
    ns<-NS(id)
    
    
    # numbers of packages published
    fluidRow(
        column(width=12,
        tags$b("Published Packages", style = 'font-size:30px;'),
        br(),
        valueBoxOutput(ns("published"), 6),
        valueBoxOutput(ns("published_late"), 6),
        valueBoxOutput(ns("published_proactive"), 3),
        valueBoxOutput(ns("published_request"), 3),
        valueBoxOutput(ns("published_late_proactive"), 3),
        valueBoxOutput(ns("published_late_request"), 3)
        )
    )
}


published_server<-function(input,output,session,report){
    
    output$published <- renderValueBox({
        n <- report() %>%
            filter(state == "Published") %>%
            nrow()
        valueBox(n, "Total Number of Packages Published",
                 icon=icon('check-circle'), "light-blue")
    })
    
    output$published_proactive <- renderValueBox({
        n <- report() %>%
            filter(state == "Published", proactive == 1) %>%
            nrow()
        valueBox(n, "Proactive", NULL, "light-blue")
    })
    
    output$published_request <- renderValueBox({
        n <- report() %>%
            filter(state == "Published", proactive == 0) %>%
            nrow()
        valueBox(n, "On Request", NULL, "light-blue")
    })
    
    output$published_late <- renderValueBox({
        n <- report() %>%
            filter(state == "Published", late == 1) %>%
            nrow()
        valueBox(n, "Number of Packages Published Late",
                 icon=icon('exclamation-circle'), "light-blue")
    })
    
    output$published_late_proactive <- renderValueBox({
        n <- report() %>%
            filter(state == "Published", late == 1, proactive == 1) %>%
            nrow()
        valueBox(n, "Proactive", NULL, "light-blue")
    })
    
    output$published_late_request <- renderValueBox({
        n <- report() %>%
            filter(state == "Published", late == 1, proactive == 0) %>%
            nrow()
        valueBox(n, "On Request", NULL, "light-blue")
    })
}



