library(tidyverse)
library(shiny)
library(rgdal)

setwd("C:/Users/jeong/Documents/shiny_j")
incom<- read_csv("income.csv")
income <- as.data.frame(incom)
income_sep<-income%>%separate(년월,c('year','mon'),sep=4) 
income<- income_sep%>%unite(YM,year,mon,sep="-")
pop<- income%>%group_by(YM)%>%summarise(incomepop=sum(전입자수))

map <- readOGR("TL_SCCO_EMD.shp")
df_map= fortify(map)
df_map_info =  map@data
df_map_info[, "id"] = (1:nrow(df_map_info)) - 1
df_map_info[, "SIDO"] = as.numeric(substr(df_map_info$EMD_CD, start = 1, stop = 2))
df_map_info_saejong = df_map_info[df_map_info$SIDO == 36, ]

numofpp<-income%>%group_by(EMD_KOR_NM)%>%summarise(n=sum(전입자수))
numofpp2<-numofpp%>%left_join(df_map_info_saejong,by="EMD_KOR_NM")
map_pp = df_map[df_map$id %in% numofpp2$id, ]
st_id<- numofpp2%>%select(EMD_CD,id,n)
st_id$id <- as.character(st_id$id)
numofpp3<-map_pp%>%left_join(st_id,by="id")

out<- read_csv("out.csv")
mvout <- as.data.frame(out)
out_sep<-mvout%>%separate(년월,c('year','mon'),sep=4) 
mv_out<- out_sep%>%unite(YM,year,mon,sep="-")
pop2<- mv_out%>%group_by(YM)%>%summarise(outpop=sum(전출자수))

numofopp<-mvout%>%group_by(EMD_KOR_NM)%>%summarise(n=sum(전출자수))
numofopp2<-numofopp%>%left_join(df_map_info_saejong,by="EMD_KOR_NM")
map_pp2 = df_map[df_map$id %in% numofopp2$id, ]
st_id2<- numofopp2%>%select(EMD_CD,id,n)
st_id2$id <- as.character(st_id2$id)
numofopp3<-map_pp2%>%left_join(st_id2,by="id")



ui <- fluidPage(
    
    titlePanel("Saejong move in/out"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            br(),
            br(),
            br(),
            
            radioButtons("dist", "select income or move out:",
                         c("income","out")),
            
            br(),
            br(),
            br(),
            br(),
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot"),
            plotOutput("distPlot1"),
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        if(input$dist=='income'){
            pt<- ggplot(pop,aes(YM,incomepop,group=1))+geom_line()+theme(axis.text.x=element_text(angle=90, hjust=1)) + ggtitle("전입자수 추이")
        }else
            if(input$dist=='out'){
                pt<- ggplot(pop2,aes(YM,outpop,group=1))+geom_line()+theme(axis.text.x=element_text(angle=90, hjust=1))+ ggtitle("전출자수 추이")
            }
        pt
    })
    
    output$distPlot1 <- renderPlot({
        if(input$dist=='income'){
            pt1<-ggplot(data = numofpp3, aes(x = long, y = lat, group = group,fill=n)) + geom_polygon(color='grey',size=0.5) +labs(fill="n")+ ggtitle("지역별 전입자수")+scale_fill_gradient(low='darkgrey',high = "yellow")
        }else
            if(input$dist=='out'){
                pt1<-ggplot(data = numofopp3, aes(x = long, y = lat, group = group,fill=n)) + geom_polygon(color='grey',size=0.5) +labs(fill="n")+ ggtitle("지역별 전출자수")+scale_fill_gradient(low='darkgrey',high = "yellow")
            }
        pt1
    })

    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
