library(openxlsx)
library(DT)
library(shiny)
library(shinydashboard)
library(dplyr)
library(scales)
library(plotly)

source("dat_rep.R")

header <- dashboardHeader(
  title = "ASEE Membership Dashboard",titleWidth=450
)
sidebar <- dashboardSidebar(
  
  selectInput("mem_type","Select Membership Type:",
              choices = unique(mem$member_type)),
  "The pulldown menu below is populated with the sub categories which are under the Membership Type chosen above.",
  selectInput("typ",
              "Select Type:",
              choices = "")
)

frow1<- bootstrapPage(
  textOutput("selected_var"),tags$head(tags$style("#selected_var{color: black;
                                 font-size: 30px;
                                 font-style: italic;
                                }")))

frow2 <- fluidRow(
  column(7,valueBoxOutput("churnrate", width = "100%")),
 column(5,infoBoxOutput("aml", width = "100%")))

  


frow3 <- fluidRow(
  infoBoxOutput("memgained"),
  infoBoxOutput("memlost"),
  
  infoBoxOutput("netnewmem")
  
)

frow4<-fluidRow(
  box("Touch graph points for more Information",background = "black", width=3))


frow5<- fluidRow(
  
  box(width=4,
      title = "Churn rate Over the Years- 2014 to 2018"
      ,status = "warning"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotlyOutput("chnrte", height = "200px")
  )
  
  ,box(width=4,
       title = "Net Members Gained- 2014 to 2018"
       ,status = "success"
       ,solidHeader = TRUE 
       ,collapsible = TRUE 
       ,plotlyOutput("nnmem",height ="200px")
  ),
  
  box(width=4,
      title = "Average Member Life- 2014 to 2018"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotlyOutput("amll",height ="200px")
  )
  
  
  
  
  
  
)

frow6<-conditionalPanel(
  condition="input.mem_type=='Individual'",
  fluidRow(
    box("Select tab for display of Graph",background = "black", width=3))
)

frow7<-conditionalPanel(
  condition="input.mem_type=='Individual'",
  fluidRow(
    box("Monthly Renewal Gaps of 0 Includes 3 months Grace Period",background = "olive", width=3))
)




frow8<-
  conditionalPanel(
    condition="input.mem_type=='Individual'",
    fluidRow(
      tabBox(
        
        
        id = "tabset1", height = "400px", width =100,
        tabPanel("Monthly Renewal Gaps", plotOutput("mrgaps", height = "300px")),
        tabPanel("Total Monthly Members For Years- 2016-2018", plotlyOutput("mthmem", height = "300px"))
        
      )
      
      
    ))




# combine the two fluid rows to make the body
body <- dashboardBody(frow1, frow2,frow3, frow4, frow5, frow6, frow7, frow8)




ui<-dashboardPage(
  header,
  sidebar,
  body,
  skin = "purple"
)


server <- function(input, output, session) {
  #some data manipulation to derive the values 
  dataa<-reactive({
   
    
    
    
    person_data1<-memm%>%filter(member_type==input$mem_type & typ==input$typ)
    a<-person_data1%>%filter(year==2012)
    a<-distinct(a,member_id, .keep_all= TRUE)
    
    aa<-a%>%group_by(year)%>%summarise(current_members=n())
    a2<-person_data1%>%filter(year==2013)
    a2<-distinct(a2,member_id, .keep_all= TRUE)
    
    a3=inner_join(a, a2, by = "member_id")
    a3<-a3%>%group_by(year.y)%>%summarise(cnt=n())
    a3<-if(nrow(a3) == 0){
      add_row(a3, year.y = 2013, cnt= 0)}else {
        a3
      }
    b<-person_data1%>%filter(year==2013)
    b<-distinct(b,member_id, .keep_all= TRUE)
    bb<-b%>%group_by(year)%>%summarise(current_members=n())
    b2<-person_data1%>%filter(year==2014)
    b2<-distinct(b2,member_id, .keep_all= TRUE)
    b3=inner_join(b, b2, by = "member_id")
    b3<-b3%>%group_by(year.y)%>%summarise(cnt=n())
    b3<-if(nrow(b3) == 0){
      add_row(b3, year.y = 2014, cnt= 0)}else {
        b3
      }
    c<-person_data1%>%filter(year==2014)
    c<-distinct(c,member_id, .keep_all= TRUE)
    cc<-c%>%group_by(year)%>%summarise(current_members=n())
    c2<-person_data1%>%filter(year==2015)
    c3=inner_join(c, c2, by = "member_id")
    c3<-c3%>%group_by(year.y)%>%summarise(cnt=n())
    c3<-if(nrow(c3) == 0){
      add_row(c3, year.y = 2015, cnt= 0)}else {
        c3
      }
    
    d<-person_data1%>%filter(year==2015)
    d<-distinct(d,member_id, .keep_all= TRUE)
    dd<-d%>%group_by(year)%>%summarise(current_members=n())
    d2<-person_data1%>%filter(year==2016)
    d2<-distinct(d2,member_id, .keep_all= TRUE)
    d3=inner_join(d, d2, by = "member_id")
    d3<-d3%>%group_by(year.y)%>%summarise(cnt=n())
    d3<-if(nrow(d3) == 0){
      add_row(d3, year.y = 2016, cnt= 0)}else {
        d3
      }
    e<-person_data1%>%filter(year==2016)
    e<-distinct(e,member_id, .keep_all= TRUE)
    ee<-e%>%group_by(year)%>%summarise(current_members=n())
    e2<-person_data1%>%filter(year==2017)
    e2<-distinct(e2,member_id, .keep_all= TRUE)
    e3=inner_join(e, e2, by = "member_id")
    e3<-e3%>%group_by(year.y)%>%summarise(cnt=n())
    e3<-if(nrow(e3) == 0){
      add_row(e3, year.y = 2017, cnt= 0)}else {
        e3
      }
    
    
    f<-person_data1%>%filter(year==2017)
    f<-distinct(f,member_id, .keep_all= TRUE)
    
    ff<-f%>%group_by(year)%>%summarise(current_members=n())
    f2<-person_data1%>%filter(year==2018)
    f2<-distinct(f2,member_id, .keep_all= TRUE)
    
    f3=inner_join(f, f2, by = "member_id")
    f3<-f3%>%group_by(year.y)%>%summarise(cnt=n())
    f3<-if(nrow(f3) == 0){
      add_row(f3, year.y = 2018, cnt= 0)}else {
        f3
      }
    g<-person_data1%>%filter(year==2018)
    g<-distinct(g,member_id, .keep_all= TRUE)
    
    gg<-g%>%group_by(year)%>%summarise(current_members=n())
    
    #churnrate, AML, Net new members
    total <- rbind(aa,bb,cc,dd,ee,ff,gg)
    total2 <- rbind(a3,b3,c3,d3,e3,f3)
    colnames(total2)[colnames(total2)=="year.y"] <-"year"
    colnames(total2)[colnames(total2)=="cnt"] <- "old_members_left"
    
    new_total<-full_join(total, total2, by="year")
    new_total<-new_total%>%mutate(members_lost=(current_members-lead(old_members_left)))
    new_total<-new_total%>%mutate(new_members=(current_members-old_members_left))
    new_total<-new_total%>%mutate(net_new_members=(new_members-members_lost))
    new_total<-new_total%>%mutate(churn_rate=(members_lost/current_members)*100)
    new_total<-new_total%>%mutate(churnrate=(members_lost/current_members))
    new_total<-new_total%>%mutate(AMLL=(1/churnrate))
    new_total<-new_total%>%mutate(AMLife=ifelse(AMLL == Inf, paste0("Ongoing"), paste0(round(AMLL, 0)," ", "Years")))
    new_total<-new_total%>%mutate(AML=ifelse(AMLL == Inf, 0, round(AMLL, 0)))
    
    
    
    
    
    current<-new_total%>%filter(year==2017)
    
  })
  
  alldat<-reactive({
    
    
    
    person_data1<-memm%>%filter(member_type==input$mem_type & typ==input$typ)
    a<-person_data1%>%filter(year==2012)
    a<-distinct(a,member_id, .keep_all= TRUE)
    
    aa<-a%>%group_by(year)%>%summarise(current_members=n())
    a2<-person_data1%>%filter(year==2013)
    a2<-distinct(a2,member_id, .keep_all= TRUE)
    
    a3=inner_join(a, a2, by = "member_id")
    a3<-a3%>%group_by(year.y)%>%summarise(cnt=n())
    a3<-if(nrow(a3) == 0){
      add_row(a3, year.y = 2013, cnt= 0)}else {
        a3
      }
    b<-person_data1%>%filter(year==2013)
    b<-distinct(b,member_id, .keep_all= TRUE)
    bb<-b%>%group_by(year)%>%summarise(current_members=n())
    b2<-person_data1%>%filter(year==2014)
    b2<-distinct(b2,member_id, .keep_all= TRUE)
    b3=inner_join(b, b2, by = "member_id")
    b3<-b3%>%group_by(year.y)%>%summarise(cnt=n())
    b3<-if(nrow(b3) == 0){
      add_row(b3, year.y = 2014, cnt= 0)}else {
        b3
      }
    c<-person_data1%>%filter(year==2014)
    c<-distinct(c,member_id, .keep_all= TRUE)
    cc<-c%>%group_by(year)%>%summarise(current_members=n())
    c2<-person_data1%>%filter(year==2015)
    c3=inner_join(c, c2, by = "member_id")
    c3<-c3%>%group_by(year.y)%>%summarise(cnt=n())
    c3<-if(nrow(c3) == 0){
      add_row(c3, year.y = 2015, cnt= 0)}else {
        c3
      }
    
    d<-person_data1%>%filter(year==2015)
    d<-distinct(d,member_id, .keep_all= TRUE)
    dd<-d%>%group_by(year)%>%summarise(current_members=n())
    d2<-person_data1%>%filter(year==2016)
    d2<-distinct(d2,member_id, .keep_all= TRUE)
    d3=inner_join(d, d2, by = "member_id")
    d3<-d3%>%group_by(year.y)%>%summarise(cnt=n())
    d3<-if(nrow(d3) == 0){
      add_row(d3, year.y = 2016, cnt= 0)}else {
        d3
      }
    e<-person_data1%>%filter(year==2016)
    e<-distinct(e,member_id, .keep_all= TRUE)
    ee<-e%>%group_by(year)%>%summarise(current_members=n())
    e2<-person_data1%>%filter(year==2017)
    e2<-distinct(e2,member_id, .keep_all= TRUE)
    e3=inner_join(e, e2, by = "member_id")
    e3<-e3%>%group_by(year.y)%>%summarise(cnt=n())
    e3<-if(nrow(e3) == 0){
      add_row(e3, year.y = 2017, cnt= 0)}else {
        e3
      }
    
    
    f<-person_data1%>%filter(year==2017)
    f<-distinct(f,member_id, .keep_all= TRUE)
    
    ff<-f%>%group_by(year)%>%summarise(current_members=n())
    f2<-person_data1%>%filter(year==2018)
    f2<-distinct(f2,member_id, .keep_all= TRUE)
    
    f3=inner_join(f, f2, by = "member_id")
    f3<-f3%>%group_by(year.y)%>%summarise(cnt=n())
    f3<-if(nrow(f3) == 0){
      add_row(f3, year.y = 2018, cnt= 0)}else {
        f3
      }
    g<-person_data1%>%filter(year==2018)
    g<-distinct(g,member_id, .keep_all= TRUE)
    
    gg<-g%>%group_by(year)%>%summarise(current_members=n())
    
    #churnrate, AML, Net new members
    total <- rbind(aa,bb,cc,dd,ee,ff,gg)
    total2 <- rbind(a3,b3,c3,d3,e3,f3)
    colnames(total2)[colnames(total2)=="year.y"] <-"year"
    colnames(total2)[colnames(total2)=="cnt"] <- "old_members_left"
    
    new_total<-full_join(total, total2, by="year")
    new_total<-new_total%>%mutate(members_lost=(current_members-lead(old_members_left)))
    new_total<-new_total%>%mutate(new_members=(current_members-old_members_left))
    new_total<-new_total%>%mutate(net_new_members=(new_members-members_lost))
    new_total<-new_total%>%mutate(churnrate=(members_lost/current_members))
    
    new_total<-new_total%>%mutate(churn_rate=(members_lost/current_members)*100)
    new_total<-new_total%>%mutate(AMLL=(1/churnrate))
    new_total<-new_total%>%mutate(AMLife=ifelse(AMLL == Inf, paste0("Ongoing"), paste0(round(AMLL, 0)," ", "Years")))
    new_total<-new_total%>%mutate(AML=ifelse(AMLL == Inf, 0, round(AMLL, 0)))
    new_total<-new_total%>%mutate(MCRR=churn_rate/100)
    new_total<-new_total%>%mutate(MCR=paste0(round(MCRR*100, 1), "%"))
    new_total<-new_total%>%mutate(a=net_new_members[nrow(new_total)-1])
    new_total<-new_total%>%mutate(b=ifelse(churn_rate[nrow(new_total)-1]<churn_rate[nrow(new_total)-2], 1, 0))
    new_total<-new_total%>%mutate(c=ifelse(net_new_members[nrow(new_total)-1]>net_new_members[nrow(new_total)-2], 1, 0))
    
    new_total <- new_total %>%mutate(Year = recode(year, 
                                                   "2012" = 2013,
                                                   "2013" = 2014,
                                                   "2014" = 2015,
                                                   "2015"=2016,
                                                   "2016"=2017,
                                                   "2017"=2018,
                                                   "2018"=2019
                                                   
    )
    )
    
  })
  
  
  
  
  monthly<-reactive({
    
   
    
    
    
    mem2<-memm%>%filter(member_type=="Individual")
    p2<-mem2%>%group_by(member_id,typ,year,month)%>%summarise(members=n())
    
    person_datam<-p2%>%select(member_id,typ,year, month) 
    #computing monthly gap
    person_datam2<-person_datam%>%group_by(member_id,typ)%>%mutate(prev=lag(month),n = 1, default = NA)
    person_datam2<-person_datam2%>%select(-n, -default)
    person_datam2<-person_datam2%>% mutate(monthly_gap=(month-prev))
    person_datam2<-person_datam2%>%group_by(member_id,typ)%>%mutate(prev1=lag(year),n = 1, default = NA)
    person_datam2<-person_datam2%>%select(-n, -default)
    person_datam2<-person_datam2%>% mutate(yearly_gap=(year-prev1))
    person_datam2<-person_datam2%>%select(-prev, -prev1)
    
    n<-person_datam2%>%filter(yearly_gap==1)
    mn3<-n%>%mutate(monthlygap= if (monthly_gap<= 3) { 
                        0
                      } else if (monthly_gap==4) {
                        1
                      } else if (monthly_gap==5) {
                        2 
                      } else if (monthly_gap==6) {
                        3
                        
                      } else if (monthly_gap==7) {
                        4
                        
                      } else if (monthly_gap==8) {
                        5 
                      } else if (monthly_gap==9) {
                        6
                      } else if (monthly_gap==10) {
                        7
                        
                      } else if (monthly_gap==11) {
                        8
                        
                      } else{
                        9
                      } 
                    
    )
    p4<-mn3%>%group_by(typ, monthlygap)%>%summarise(cnt=n())
    
    n1<- p4%>%filter(typ==input$typ)
    
  })
  
  monthmem<-reactive({
    
    pp<-memm%>%filter(member_type=="Individual")
    p3<-pp%>%group_by(member_id,typ,year,month)%>%summarise(members=n())
    pp2<-p3%>%select(member_id,typ,year, month) 
    #computing monthly members
    pp3<-pp2%>%filter(typ==input$typ)
    
    n1<-pp3%>%group_by(year, month,typ, member_id)%>%summarise(cnt=n())
    kok<-n1%>%group_by(year, month)%>%summarise(monthly_members= as.numeric(length(unique(member_id))))
    kok$year<-as.factor(kok$year)
    kok$month <- as.numeric(kok$month)
    sbt<-kok%>%filter(year %in% c(2018,2017,2016))
    
    
  })
  
  
  
  
 observe({
    updateSelectInput(
      session,
      "typ",
      choices = mem %>%
        filter(member_type == input$mem_type) %>%
        select(typ) %>%
        .[[1]]
      
    )
  } )
  
  
  output$selected_var <- renderText({ 
    paste("Membership Information About"," ",input$mem_type,"-", input$typ)
  })
  
  
  
  
  
  output$churnrate <- renderValueBox({
    infoBox( 
      title = NULL,
      value = tags$p(style = "font-size: 20px;
                     font-style: bold", "CURRENT CHURN RATE-2018"), paste0(round(dataa()$churn_rate,1), "%" ," " ,"-"," ",ifelse(dataa()$churn_rate<15, 
                                                                                                                         paste("Good"),
                                                                                                                         paste("Not Good" ))),
          color =if (dataa()$churn_rate< 15) { 
        'green'
      } else if (dataa()$churn_rate>=15 &dataa()$churn_rate<=40) {
        'yellow'
        
      } else{
        'maroon'
      } ,
      fill = TRUE,
      icon = icon("braille"), width=50
    )
    
  })
  output$memgained <- renderInfoBox({
    infoBox(
      title = NULL,
      value = tags$p(style = "font-size: 20px;
                     font-style: bold", "CURRENT MEMBERS GAINED-2018"),paste0(dataa()$new_members),
      icon = icon("users"),color = "blue",fill = TRUE
    )
  })
  
 
  
  
  
  
 
  output$memlost <- renderInfoBox({
    infoBox(
      title = NULL,
      value = tags$p(style = "font-size: 20px;
                     font-style: bold", "CURRENT MEMBERS LOST-2018"), paste0(dataa()$members_lost),
      icon = icon("user-alt-slash"),color = "blue",fill = TRUE
    )
  })
  output$netnewmem <- renderInfoBox({
    infoBox(
      title = NULL,
      value = tags$p(style = "font-size: 20px;
                     font-style: bold", "CURRENT NET NEW MEMBERS-2018"),paste0(dataa()$net_new_members), icon = icon("user-plus"),
      color = "blue",fill = TRUE
    )
  })
  output$aml <- renderInfoBox({
    infoBox(
      title = NULL,
      value = tags$p(style = "font-size: 20px;
                     font-style: bold", "AVERAGE MEMBER LIFE -2018"),paste0(dataa()$AMLife),
      icon = icon("user-shield"),
      color = "blue",fill = TRUE
    )
  })
  
  output$chnrte<- renderPlotly({
    ggplotly(ggplot(alldat(),aes(label=MCR, label2=AMLife))+geom_point(aes(x = Year, y =MCRR),color="black", size=2.0)+geom_line(aes(x = Year, y =MCRR),col = ifelse(alldat()$b<1,'red',"blue"), size=1, group=1)+
               scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0, 100/100),breaks = seq(0,100/100, by= 10/100))+
               scale_x_continuous(name = "Year",limits=c(2014, 2018), breaks = seq(2014,2018, by= 1)) +
               ylab("Churn Rate") +
               theme(plot.title = element_text(face="bold.italic", color="black", size =10),
                     axis.title.x = element_blank(),
                     axis.title.y = element_blank())+theme(legend.title=element_blank()))
    
  })
  
  output$nnmem<- renderPlotly({
    ggplotly( ggplot(alldat(),aes(label=current_members, label2=members_lost, label3=new_members))+
                geom_point(aes(x = Year, y =net_new_members),color="black", size=2.0)+
                geom_line(aes(x = Year, y =net_new_members),
                          col = ifelse(alldat()$c<1,'red',"green"),
                          size=1)+
                scale_x_continuous(name = "Year",limits=c(2014, 2018), breaks = seq(2014,2018, by= 1)) +
                ylab("Net New Members") +
                theme(plot.title = element_text(face="bold.italic", color="black", size =10),
                      axis.title.x = element_blank(),
                      axis.title.y = element_blank())+theme(legend.title=element_blank()))
    
  })
  
  
  output$amll<- renderPlotly({
    ggplotly( ggplot(alldat(),aes(label=AMLife))+
                geom_point(aes(x = Year, y =AML),color="black", size=2.0)+
                geom_line(aes(x = Year, y =AML),
                          col = ifelse(alldat()$b<1,'red',"purple"),
                          size=1)+
                scale_x_continuous(name = "Year",limits=c(2014, 2018), breaks = seq(2014,2018, by= 1)) +
                ylab("Net New Members") +
                theme(plot.title = element_text(face="bold.italic", color="black", size =10),
                      axis.title.x = element_blank(),
                      axis.title.y = element_blank())+theme(legend.title=element_blank()))
    
  })
  
  
  
  
  
  
  
  output$mrgaps<- renderPlot({
    ggplot(monthly(), aes(x =monthlygap, y = cnt)) +
      geom_bar(fill = "#0073C2FF", stat = "identity") +
      geom_text(aes(label = cnt), vjust = -0.3)+ scale_x_continuous(name = "Monthly Gaps",limits=c(-2, 9), breaks = seq(-2,9, by= 1))+
      theme(plot.title = element_text(face="bold.italic", color="black", size =10),
            
            axis.title.y = element_blank())+theme(legend.title=element_blank())
    
    
  })
  
  
  
  
  output$mthmem<- renderPlotly({
    ggplotly(ggplot(monthmem(), aes(x=month, y=monthly_members,color=year)) +
               geom_line() +
               geom_point()+scale_x_continuous(breaks = 1:12,
                                               label=c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                                       "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))+
               
               scale_y_continuous(name = "Monthly Members")+
               
               theme(plot.title = element_text(face="bold.italic", hjust = 0.5,color="black", size =10),
                     
                     axis.title.x = element_blank())
             
    )
    
    
  })
  
  
  
}


shinyApp(ui, server)



