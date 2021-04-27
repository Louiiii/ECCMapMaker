
suppressMessages({
library(shiny)
library(dplyr)
library(rgdal)
library(htmlwidgets)
library(ggplot2)
library(tidyr)
library(sf)
library(data.table)
library(readr)
library(RColorBrewer)
library(shinyjs)
library(shinyWidgets)
library(cowplot)
library(ggpubr)


})
load("mapdata.RData")

shinyServer(function(input, output,session) {


  ranges <- reactiveValues(x = NULL, y = NULL)


  datasetInput<-reactive({
    suppressWarnings({
    switch(input$Geo,
           "Districts"=LAD,
           "Middle layer super output areas (MSOA)"=MSOA,
           "Lower layer super output area (LSOA)"=LSOA)
    })
  })  
  

  output$downloadtemplate<-downloadHandler(
    filename=function(){
      paste(input$Geo,".csv",sep="")
    },
    content=function(file){
      write.csv(datasetInput(),file,row.names = F)
    }
  )  
  
  
  data<-reactive({
    
    if(is.null(input$upload)){
      if(input$Geo=="Districts"){
LAD%>%filter(District%in%input$Districts)
        
      }else if (input$Geo=="Middle layer super output areas (MSOA)"){
MSOA%>%filter(District%in%input$Districts)
      }else if (input$Geo=="Lower layer super output area (LSOA)"){
LSOA%>%filter(District%in%input$Districts)
      }
    }else{
fread(input$upload$datapath,header = T,blank.lines.skip = T)
    }
    
  })
  
  suppressWarnings({
suppressMessages({  
  
 map<-reactive({
   

   if(input$Geo=="Districts"){
     right_join(LADMap,fread(input$textdata,header = T,blank.lines.skip = T)%>%filter(District%in%input$Districts)%>%drop_na(),by="Code")
   }else if (input$Geo=="Middle layer super output areas (MSOA)"){
     right_join(MSOAMap,fread(input$textdata,header = T,blank.lines.skip = T)%>%filter(District%in%input$Districts)%>%drop_na(),by="Code")
   }else if (input$Geo=="Lower layer super output area (LSOA)"){
     right_join(LSOAMap,fread(input$textdata,header = T,blank.lines.skip = T)%>%filter(District%in%input$Districts)%>%drop_na(),by="Code")
   }
 }) 
  
})
  
  })

  
  
  map2<-reactive({
    a<-map()
    st_crs(a) <- 4326
    return(a)
  })
  
  

  
  
  
  
  
  
  
  breaks<-reactive({
if(input$Scale=="Linear"){
  return(range(map2()$Value))
}else if (input$Scale=="Quantile"){
  return(unname(quantile(map2()$Value)))
}else{
  return(NULL)
}
  })
  
  
  
  pal<-reactive({
    if(input$Scale=="Linear"){
      return(brewer.pal(n=brewer.pal.info[input$Palette,]$maxcolors,name = input$Palette))
    }else if (input$Scale=="Quantile"){
      return(brewer.pal(n=4,name=input$Palette))
    }
  })
  

  observe({
    suppressWarnings({
    
    if(!is.null(input$upload)){
    df<-fread(input$upload$datapath,header = T,blank.lines.skip = T)
    if(all(df$Code%in%LSOAMap$Code)){
      updateSelectInput(session,"Geo",selected = "Lower layer super output area (LSOA)",choices = c("Districts",
                                                                                                    "Middle layer super output areas (MSOA)",
                                                                                                    "Lower layer super output area (LSOA)"))
    }else if (all(df$Code%in%MSOAMap$Code)){
      updateSelectInput(session,"Geo",selected = "Middle layer super output areas (MSOA)",choices = c("Districts",
                                                                                                      "Middle layer super output areas (MSOA)",
                                                                                                      "Lower layer super output area (LSOA)"))
      
    }else if (all(df$Code%in%LADMap$Code)){
      updateSelectInput(session,"Geo",selected = "Districts",choices = c("Districts",
                                                                         "Middle layer super output areas (MSOA)",
                                                                         "Lower layer super output area (LSOA)"))
     
    }
    }
    })
    }
               )
 

  observe({
    updateTextAreaInput(session,"textdata",label = "Manual Data",value = format_csv(data()))
  })
  
  
  
  
  
  
  
output$table<-renderText({format_csv(data())})

   
dataplot<-reactive({
  
  if(input$insetmode==T){

    
    xlim<-isolate(ranges$x)
    ylim<-isolate(ranges$y)
map_a<-ggplot()+
      geom_sf(data=allessex,fill="white")+
  annotate("rect",xmin=xlim[1],xmax=xlim[2],ymin = ylim[1],ymax = ylim[2],alpha=0.2,fill="red")+
      theme_void()+
  theme(panel.background = element_rect(fill = "lightblue"))

map_b<-ggplot()+
  geom_sf(data=map2(),aes(fill=Value))+
  scale_fill_distiller(
    name=input$Legend_Title,
    breaks=breaks(),
    direction = 1,
    palette=input$Palette)+
  theme_void()+
  ggtitle(input$title)+
  theme(
    plot.title = element_text(hjust=0.5,size=20)
  )+
  coord_sf(xlim = xlim, ylim = ylim, expand = T)

combined<-ggdraw()+
  draw_plot(map_b,width = 0.8)+
draw_plot(map_a,x=0.60,y=0.05,width = 0.3,height=0.3)
  
  
combined
   
  }else{
  
  suppressWarnings({
  ggplot(data=map2())+
    geom_sf(aes(fill=Value))+
    scale_fill_distiller(
      name=input$Legend_Title,
      breaks=breaks(),
      direction = 1,
      palette=input$Palette)+
    theme_void()+
    ggtitle(input$title)+
    theme(
      plot.title = element_text(hjust=0.5,size=20)
    )+
      coord_sf(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
    
    
    
  })
    
  }
})
  
  
  output$plot<-renderPlot({
    suppressWarnings({
    dataplot()    })
  })
  
  output$downloadPlot <- downloadHandler(
    filename = "Map.png",
    content = function(file){
      ggsave(file,plot=dataplot())
    }
  )


  observeEvent(input$reset, {
    reset("upload")
    reset("textdata")
    reset("Geo")
    reset("Palette")
    reset("Scale")
    reset("title")
    reset("Legend_Title")
    reset("Districts")
    
  })

  observeEvent(input$Geo,{
    suppressWarnings({
    if(is.null(input$upload)){
      if(input$Geo=="Districts"){
        updateTextAreaInput(session,"textdata","Manual Data",value = format_csv(LAD))
      }else if (input$Geo=="Middle layer super output areas (MSOA)"){
        updateTextAreaInput(session,"textdata","Manual Data",value = format_csv(MSOA))
      }else if(input$Geo=="Lower layer super output area (LSOA)"){
        updateTextAreaInput(session,"textdata","Manual Data",value = format_csv(LSOA))
        
      }
  
    }
    })  
    

  })
  
  
  observeEvent(input$timeOut,{
    print(paste0("Session (", session$token, ") timed out at: ", Sys.time()))
    showModal(modalDialog(
      title= "Timeout",
      paste("Session timeout due to", input$Timeout, "inactivity -",Sys.time()),
      footer=NULL
    ))
    session$close()
  })
  
  
  
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  
observeEvent(input$insetmode,{
  if (input$insetmode==F){
    ranges$x <- NULL
    ranges$y <- NULL
  }
})
  
  
})
