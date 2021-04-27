suppressMessages({
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(dashboardthemes)
library(shinycssloaders)
library(dplyr)
library(scales)
})

timeoutSeconds<-180

load("mapdata.RData")

inactivity<-sprintf("function idleTimer(){
                    var t = setTimeout(logout, %s);
                    window.onmousemove = resetTimer; //catches mouse movements
                    window.onmousedown = resetTimer; //catches mouse movements
                    window.onclick = resetTimer; //catches mouse clicks
                    window.onscroll = resetTimer; //catches scrolling
                    window.onkeypress = resetTimer; //catches keyboard actions
                    
                    function logout(){
                    Shiny.setInputValue('timeOut', '%ss')
                    }
                    
                    function resetTimer(){
                    clearTimeout(t);
                    t = setTimeout(logout, %s); //time is in milliseconds (1000 is 1 second) )
                    }
}
                    idleTimer();",timeoutSeconds*1000, timeoutSeconds, timeoutSeconds*1000)









dashboardPage(
    dashboardHeader(title="Essex County Council Map Maker",
                    titleWidth="500px",
                    dropdownMenu(type = "message",
                                 messageItem(
                                     from = "Contact",
                                     message = "Louis.YongYangYen@essex.gov.uk"
                                 ),headerText = "Any Issues or feedback")),
    dashboardSidebar(disable = T,minified = F),
    dashboardBody(
      useShinyjs(),
      fluidRow(
        shinyDashboardThemes("blue_gradient"),
        tags$script(inactivity),
        column(width = 3,
        box(width = 12,
            collapsible = T,
            title = "Data",
fileInput("upload",
          "Upload csv file",
          multiple = F,
          accept = ".csv"),
textAreaInput("textdata","Manual Data",height = "200px",value = def),
actionButton("reset","Reset")

        ),
box(width = 12,
    collapsible = T,
    title="Districts",
    selectInput(inputId = "Districts",
                choices = Essex,
                label = "Choose Districts",
                multiple = T,
                selectize = T,
                selected = Essex)
),
h3("Palette Examples"),
img(src="Colours2.png",align="right",width="90%")
),
column(width = 2,
        box(width = 12,
            collapsible = T,
            title="Design",
            selectizeInput("Palette",
                           choices=palettelist,
                           label="Choose Palette"),

            
            
            selectInput(inputId = "Scale",
                        choices = c("Linear",
                                    "Quantile","None"),
                        selected = "Linear",
                        label = "Choose Scale"),
            textInput("title","Enter Title Here",value = "Example map"),
            textInput("Legend_Title","Enter Legend Title Here",value="IAMLEGEND")
        ),        
       
       box(width = 12,
                      collapsible = T,
                      title = "Geography",
                      selectInput(inputId = "Geo",
                                  choices = c("Districts",
                                              "Middle layer super output areas (MSOA)",
                                              "Lower layer super output area (LSOA)"),
                                  label = "Choose Map Type"
                      ),

           
                      downloadButton("downloadtemplate","Download Template",style="white-space: normal;
                        text-align:center;
                        height:50px;
                        width:150px;
                        font-size: 14px;")
                      
        ),
       box(title="Inset Map",
           width = 12,
           collapsible = T,
           collapsed = T,
checkboxInput("insetmode","As an Inset map?"),
"Note: Zoom in on map before ticking box, to reset the map untick the checkbox."
           )
       
),
        

        column(width = 7,
        column(width=12,
        downloadButton('downloadPlot',HTML('Download Map')),
        plotOutput("plot",width = "100%",
                   height = "600px",
                   brush = brushOpts(
                     id = "plot1_brush",
                     resetOnNew = T
                   ),
                   dblclick = "plot1_dblclick")%>%withSpinner(type=5),
        verbatimTextOutput("hover_info"),
        br()),
        column(width = 12,
        h2("Guidance"),
        h4("1. Select your chosen map type and download the template"),
        h4("2. In the template add your value data as required, the following headers are needed: Code, District and Value"),
        h4("3. Upload the csv file, the geography should change to the correct map type"),
        h4("4. You can manually update the data that is viewed in the 'Manual Data' text box you can remove areas as well as edit the data values"),
        h4("5. If you are not using all the districts you can remove them from the 'choose districts' list"),
        h4("6. Edit design as required, you can now zoom in by highlighting a box on the map and double clicking (you can then reset by double clicking again)."),
        h4("7. Click download plot to save the file as a png file")
)
)

        )
)
)
