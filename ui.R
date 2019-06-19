dashboardPage (
    
    # display page title
    dashboardHeader(title = paste0("Public Release of Clinical Information  ",
        Sys.Date() - 14, " to ", Sys.Date()), titleWidth = 700),
    
    dashboardSidebar(disable = TRUE),

    dashboardBody(
        
        fluidRow(
            column(width = 12, offset = 0.5, tags$b("Published Packages",style='font-size:30px;'))
        ),
        
        br(),
        
        # numbers of packages published
        fluidRow(
            valueBoxOutput("published", 6),
            valueBoxOutput("published_late", 6)
        ),
        fluidRow(
            valueBoxOutput("published_proactive", 3),
            valueBoxOutput("published_request", 3),
            valueBoxOutput("published_late_proactive", 3),
            valueBoxOutput("published_late_request", 3)
        ),
        
        # plot of packages published
        fluidRow(
            column(width = 6, offset = 3, dataTableOutput("published_table"))
        ),
        
        fluidRow(
            column(width = 12, offset = 0.5, tags$b("In Progress Packages",style='font-size:30px;'))
        ),
        
        br(),
        
        # numbers of packages in progress
        fluidRow(
            valueBoxOutput("in_progress", 6),
            valueBoxOutput("in_progress_late", 6)
        ),
        fluidRow(
            valueBoxOutput("in_progress_proactive", 3),
            valueBoxOutput("in_progress_request", 3),
            valueBoxOutput("in_progress_late_proactive", 3),
            valueBoxOutput("in_progress_late_request", 3)
        ),
        
        # plot of packages in progress
        fluidRow(
            column(width = 12, offset = 0.5, plotOutput("in_progress_plot"))
        ),
        
        br(),
        
        # input text for print out
        fluidRow(
            column(width = 12, offset = 0.5,
                textInput("narrative", "Narrative", width = "100%")
            )
        ),
        
        # display download button
        fluidRow(
            column(width = 12, align = "center",
                downloadButton("download", "Download PDF Report")
            )
        )
        
    )
    
)