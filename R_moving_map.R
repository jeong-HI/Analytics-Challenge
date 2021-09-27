library(shiny)
library(quantmod)
library(tidyverse)
library(rgdal)


setwd("C:/Users/jeong/Documents/shiny_j")
all<- read.csv("all3.csv")
all<- all%>%unite(YM,Year,Month,sep="-")
all<- all%>%unite(yyyymmdd,YM,Day,sep="-")
all<- select(all, -(X.1:X.3))
all<- select(all,-(X))
all$Date <- strptime(as.character(all$yyyymmdd),format="%Y-%m-%d")
all$Date <- as.POSIXct(all$Date)
all2<-all%>%separate(법정동코드,c('val','nul'),sep=3) 
all2<- all2%>%unite(EMD_CD,시군구코드,val,sep="")
sml_all2 <- all2%>%select(EMD_CD,건물명,using,세대수.세대.,Date )

map <- readOGR("TL_SCCO_EMD.shp")
df_map= fortify(map)
df_map_info =  map@data
df_map_info[, "id"] = (1:nrow(df_map_info)) - 1
df_map_info[, "SIDO"] = as.numeric(substr(df_map_info$EMD_CD, start = 1, stop = 2))
df_map_info_saejong = df_map_info[df_map_info$SIDO == 36, ]
df_map_saejong = df_map[df_map$id %in% df_map_info_saejong$id, ]
plus_cd<- df_map_info_saejong%>%select(EMD_CD,id)
plus_cd$id <- as.character(plus_cd$id)

ui <- fluidPage(
  
  titlePanel("House Supply"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      br(),
      br(),
      br(),
      
      selectInput("var",label="select a house:", 
                  choices= c('apt','vil','house','all'),
                  selected= "apt", multiple = F),
      
      br(),
      br(),
      
      sliderInput("binsize",
                  "Number of bins:",
                  min = 1,
                  max = 100,
                  value =50),
      br(),
      br(),
      
      dateRangeInput("dates",
                     "Data range",
                     start = "1901-03-09", 
                     end= "2020-11-30"),
      br(),
      br(),
      br(),
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      plotOutput("distPlot1"),
      plotOutput("distPlot2")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot <- renderPlot({
    if(input$var=='apt'){
      sml<- all[all$Date >= input$dates[1] & all$Date < input$dates[2],]
      pt<- sml%>%filter(using=='공동주택')%>%ggplot(aes(Date))+geom_histogram(bins= input$binsize)
    }else
      if(input$var=='vil'){
        sml<- all[all$Date >= input$dates[1] & all$Date < input$dates[2],]
        pt<- sml%>%filter(using=='빌라')%>%ggplot(aes(Date))+geom_histogram(bins= input$binsize)
      }else
        if(input$var=='house'){
          sml<- all[all$Date >= input$dates[1] & all$Date < input$dates[2],]
          pt<- sml%>%filter(using=='단독주택')%>%ggplot(aes(Date))+geom_histogram(bins= input$binsize)
        }else
          if(input$var=='all'){
            sml<- all[all$Date >= input$dates[1] & all$Date < input$dates[2],]
            pt<- sml%>%ggplot(aes(Date))+geom_histogram(bins= input$binsize) 
          }
    pt
  })
  
  
  output$distPlot1 <- renderPlot({
    
    if(input$var=='apt'){
      sml<- all[all$Date >= input$dates[1] & all$Date < input$dates[2],]
      ex<- sml%>%separate(yyyymmdd,c('Year','MonDay'), sep=4)
      ex2<-ex%>%filter(using=='공동주택')%>%group_by(Year)%>%summarise(hssum=sum(세대수.세대.))
      pt1<-ex2%>%ggplot(aes(Year,hssum,group=1))+geom_line()+theme(axis.text.x=element_text(angle=45, hjust=1))
      
    }else
      if(input$var=='vil'){
        sml<- all[all$Date >= input$dates[1] & all$Date < input$dates[2],]
        ex<- sml%>%separate(yyyymmdd,c('Year','MonDay'), sep=4)
        ex2<-ex%>%filter(using=='빌라')%>%group_by(Year)%>%summarise(hssum=sum(세대수.세대.))
        pt1<-ex2%>%ggplot(aes(Year,hssum,group=1))+geom_line()+theme(axis.text.x=element_text(angle=45, hjust=1))
      }else
        if(input$var=='house'){
          sml<- all[all$Date >= input$dates[1] & all$Date < input$dates[2],]
          ex<- sml%>%separate(yyyymmdd,c('Year','MonDay'), sep=4)
          ex2<-ex%>%filter(using=='단독주택')%>%group_by(Year)%>%summarise(hssum=sum(세대수.세대.))
          pt1<-ex2%>%ggplot(aes(Year,hssum,group=1))+geom_line()+theme(axis.text.x=element_text(angle=45, hjust=1))
        }else
          if(input$var=='all'){
            sml<- all[all$Date >= input$dates[1] & all$Date < input$dates[2],]
            ex<- all%>%separate(yyyymmdd,c('Year','MonDay'), sep=4)
            ex2<-ex%>%group_by(Year)%>%summarise(hssum=sum(세대수.세대.))
            pt1<-ex2%>%ggplot(aes(Year,hssum,group=1))+geom_line()+theme(axis.text.x=element_text(angle=45, hjust=1))
            
          }
    pt1
    
  })
  
  
  output$distPlot2 <- renderPlot({
    if(input$var=='apt'){
      sml2_1<- sml_all2[sml_all2$Date >= input$dates[1] & sml_all2$Date < input$dates[2],]
      sml2<-sml2_1%>%filter(using=='공동주택')
      kk<-sml2%>%group_by(EMD_CD)%>%summarise(numofhouse=sum(세대수.세대.))
      df_map_saejong_join<- plus_cd%>%left_join(kk, by="EMD_CD")
      df_map_saejong_join2<- df_map_saejong%>%left_join(df_map_saejong_join, by="id")
      pt2<- ggplot(data = df_map_saejong_join2, aes(x = long, y = lat, group = group,fill=numofhouse)) + geom_polygon() + geom_polygon(color='grey',size=0.5)+labs(fill="세대수") + scale_fill_gradient(low='darkgrey',high = "yellow")
      
    }else
      if(input$var=='vil'){
        sml2_1<- sml_all2[sml_all2$Date >= input$dates[1] & sml_all2$Date < input$dates[2],]
        sml2<-sml2_1%>%filter(using=='빌라')
        kk<-sml2%>%group_by(EMD_CD)%>%summarise(numofhouse=sum(세대수.세대.))
        df_map_saejong_join<- plus_cd%>%left_join(kk, by="EMD_CD")
        df_map_saejong_join2<- df_map_saejong%>%left_join(df_map_saejong_join, by="id")
        pt2<-ggplot(data = df_map_saejong_join2, aes(x = long, y = lat, group = group,fill=numofhouse)) + geom_polygon() + geom_polygon(color='drey',size=0.5)+labs(fill="세대수") + scale_fill_gradient(low='darkgrey',high = "yellow")
      }else
        if(input$var=='house'){
          sml2_1<- sml_all2[sml_all2$Date >= input$dates[1] & sml_all2$Date < input$dates[2],]
          sml2<-sml2_1%>%filter(using=='단독주택')
          kk<-sml2%>%group_by(EMD_CD)%>%summarise(numofhouse=sum(세대수.세대.))
          df_map_saejong_join<- plus_cd%>%left_join(kk, by="EMD_CD")
          df_map_saejong_join2<- df_map_saejong%>%left_join(df_map_saejong_join, by="id")
          pt2<- ggplot(data = df_map_saejong_join2, aes(x = long, y = lat, group = group,fill=numofhouse)) + geom_polygon() + geom_polygon(color='grey',size=0.5)+labs(fill="세대수") + scale_fill_gradient(low='darkgrey',high = "yellow")
        }else
          if(input$var=='all'){
            sml2<- sml_all2[sml_all2$Date >= input$dates[1] & sml_all2$Date < input$dates[2],]
            kk<-sml2%>%group_by(EMD_CD)%>%summarise(numofhouse=sum(세대수.세대.))
            df_map_saejong_join<- plus_cd%>%left_join(kk, by="EMD_CD")
            df_map_saejong_join2<- df_map_saejong%>%left_join(df_map_saejong_join, by="id")
            pt2<-ggplot(data = df_map_saejong_join2, aes(x = long, y = lat, group = group,fill=numofhouse)) + geom_polygon() + geom_polygon(color='grey',size=0.5)+labs(fill="세대수") + scale_fill_gradient(low='darkgrey',high = "yellow")
          }
    pt2
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
