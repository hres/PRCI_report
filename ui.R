dashboardPage (
    
    # display page title
    dashboardHeader(title = paste0("Public Release of Clinical Information  ",
        Sys.Date() - 14, " to ", Sys.Date()), titleWidth = 700),
    
    dashboardSidebar(
        width=150,
        sidebarMenu(id='tab',
            menuItem('Drug',tabName='drug',icon=icon('pills')),
            menuItem('Medical Device',tabName='device',icon=icon('syringe'))
        )
    ),

    dashboardBody(
        tabItems(
        
        tabItem('drug',
                
        withSpinner(timelineUI('drug_timeline')),
        publishedUI('drug_published'),
        
        # plot of packages published
        # fluidRow(
        #     column(width = 6, offset = 3, dataTableOutput("published_table"))
        # ),
        
        inprogressUI('drug_inprogress')
        ),
        
        tabItem('device',
                
                withSpinner(timelineUI('device_timeline')),
                publishedUI('device_published'),
                
                inprogressUI('device_inprogress')
                )
        )
        
    )
    
)