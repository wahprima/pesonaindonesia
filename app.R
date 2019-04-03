#Name : Primaditaningtyas W
#Shinydashboard : https://wahprima.shinyapps.io/pesonaindonesia/

# Data source :
#https://www.bps.go.id/statictable/2009/04/06/1373/jumlah-akomodasi-rata-rata-pekerja-dan-jumlah-tamu-per-hari-menurut-provinsi-2009-2016-hotel-bintang-.html
#Polygon:
#https://github.com/superpikar/indonesia-geojson/blob/master/indonesia-edit.geojson , 
#with modification through : http://geojson.io/#map=2/20.0/0.0

# Importing library
library(readxl)
library(dplyr)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(DT)
library(leaflet)
library(leaflet.extras)
library(htmltools)
library(geojsonio)
library(reshape)
library(reshape2)


#rm(list=ls()) #To clear the environment when needed

# Data Pre-Processing
tourism <- read_excel("data/Hotel_2009-2016.xls", skip = 3) %>% 
  dplyr::rename(
    Province="..1",
    Hotel="Usaha..2",
    Room="Kamar..3",
    Bed="Tempat Tidur",
    Employee_hotel="Usaha..5",
    Employee_room="Kamar..6",
    Local_traveller="Indonesia",
    Foreign_traveller="Asing",
    Total_traveller="Jumlah"
  ) %>%
  na.omit() %>% 
  filter(Province != "Indonesia") %>% 
  mutate_all(funs(replace(.,.=="-","0"))) %>% 
  mutate(
    Year = c(1:266),
    Year = case_when(
      Year >=1 & Year <= 34 ~ 2016,
      Year >=35 & Year <= 68 ~ 2015,
      Year >=69 & Year <= 101 ~ 2014,
      Year >=102 & Year <= 134 ~ 2013,
      Year >=135 & Year <= 167 ~ 2012,
      Year >=168 & Year <= 200 ~ 2011,
      Year >=201 & Year <= 233 ~ 2010,
      TRUE ~ 2009
    ),
    Province = case_when(
      Province == "Kep Bangka Belitung" ~ "Kepulauan Bangka Belitung",
      TRUE ~ Province
    )
  ) %>% 
  rbind(c("Kalimantan Utara",0,0,0,0,0,0,0,0,2014),
        c("Kalimantan Utara",0,0,0,0,0,0,0,0,2013),
        c("Kalimantan Utara",0,0,0,0,0,0,0,0,2012),
        c("Kalimantan Utara",0,0,0,0,0,0,0,0,2011),
        c("Kalimantan Utara",0,0,0,0,0,0,0,0,2010),
        c("Kalimantan Utara",0,0,0,0,0,0,0,0,2009)) %>% 
  mutate_at(vars("Province"),funs(factor)) %>% 
   mutate_if(is.character,as.numeric)

# To set y scale of facet wrap
dummy <- data.frame(
  variable = c(
    "Employment","Employment","Hotel","Hotel","T.Tourists","T.Tourists"
  ),
  x=2009,
  y=c(0,7000,0,80,0,7000)
)

#To filter row based on year
slice_no <- function(yr){
  x <- if (yr == 2009) {
    1
  }
  else if (yr == 2010) {
    1:2
  }
  else if (yr == 2011) {
    1:3
  }
  else if (yr == 2012) {
    1:4
  }
  else if (yr == 2013) {
    1:5
  }
  else if (yr == 2014) {
    1:6
  }
  else if (yr == 2015) {
    1:7
  }
  else {
    1:8
  }
  return(x)
}

#Crosscor heatmap table
pre_cros <- tourism %>% 
  mutate(Employment = Hotel*Employee_hotel) %>% 
  group_by(Year) %>% 
  summarise(
    Bed = sum(Bed),
    Empl = sum(Employment),
    Frgn = mean(Foreign_traveller),
    Hotel = sum(Hotel),
    Locl =mean(Local_traveller),
    Room = sum(Room),
    TTrst = mean(Total_traveller)
  ) %>% 
  select(c(2:8,1))

#To make triangle matrix
lower_tri <- function(crostab){
  crostab[upper.tri(crostab)] <- NA
  return(crostab)
}

#Data frame for croscor heatmap
crostab <- round(cor(pre_cros),3) %>% 
  lower_tri() %>% 
  reshape::melt()

# To export processed data
#write.csv(tourism, "data/tourism.csv",row.names = FALSE)

#Polygon for leaflet and data merging

lfl <- geojson_read("data/pw_indonesia-edit.geojson", what = "sp")

# Editing data inside geojson dataframe
lfl@data <- lfl@data %>% 
  mutate(id = c(1:34)) %>% 
  mutate_at("state",as.character) %>%
  mutate(state = gsub("Yogyakarta","DI Yogyakarta",state)) %>% 
  mutate(state = gsub("Bangka-Belitung","Kepulauan Bangka Belitung",state)) %>%
  mutate(state = gsub("Irian Jaya Barat","Papua Barat",state)) %>% 
  mutate(state = gsub("Jakarta Raya","DKI Jakarta",state)) %>% 
  arrange(state) %>% 
  mutate_at("state",as.factor)

# List assignment for ggplotly layout
axy <- list(
  zeroline = TRUE,
  zerolinecolor = "black",
  zerolinewidth = 2
)


#  Shiny dashboard user interface

ui <- fluidPage(
  
  
  dashboardPage(
    
    # Header customization
    dashboardHeader(
      title = "Indonesia Tourism Statistics",
      titleWidth = 300
    ),
    
    skin = "green",
    
    # Sidebar customization
    dashboardSidebar(
      width = 300,
      sidebarMenu(
        menuItem(
          text = "Visualization Chart",
          badgeLabel = icon(name= "chart-line",
                            lib = "font-awesome"),
          tabName = "chart"
        ),
        menuItem(
          text = "Visualization Map",
          badgeLabel = icon(name= "globe-asia",
                            lib = "font-awesome"),
          tabName = "map"
        ),
        
        menuItem(
          text = "Data Table",
          badgeLabel = icon(name= "binoculars",
                            lib = "font-awesome"),
          tabName = "table"
        )
      )
    ),
    
    # Body customization
    dashboardBody(
      
      # First Tab
      tabItems(
        tabItem(
          tabName = "chart",
          fluidRow(
            
            # Box1  : visualisation
            box(
              title = "Visualization Chart",
              width = 12,
              status = "success",
              solidHeader = TRUE,
              collapsible = TRUE,
              tabsetPanel(
                type = "tabs",
                tabPanel(
                  title = "General Chart",
                  br(),
                  br(),
                  plotlyOutput(outputId = "p1"),
                  br(),
                  br(),
                  hr(),
                  tags$div(class="header", checked=NA,
                           tags$b("Customize your data: "),
                           tags$h6("Click play button to see annual changes")),
                  column(
                    10,offset = 1,
                    sliderInput(
                      animate = animationOptions(
                        loop = TRUE
                      ),
                      inputId = "yr",
                      label = "Year : ",
                      min = 2009, max = 2016,
                      value = 2009
                    )
                  ),
                  column(
                    6,
                    selectInput(
                      inputId = "xId",
                      label = "Accomodation category : ",
                      choices = c("Hotel","Room","Bed"),
                      selected = "Hotel"
                    ),
                    br(),
                    br(),
                    br()
                  ),
                  column(
                    6,
                    selectInput(
                      inputId = "yId",
                      label = "Tourist category : ",
                      choices = c("Local tourist", "Foreign tourist", "Total tourist"),
                      selected = "Local tourist"
                    ),
                    br(),
                    br(),
                    br()
                  ),
                  br(),
                  br(),
                  column(
                    12,
                    hr()
                  ),
                  column(
                    6,
                    plotlyOutput(outputId = "p3")
                  ),
                  column(
                    6,
                    plotlyOutput(outputId = "p4")
                  )
                ),
                tabPanel(
                  title = "Heatmap Crosscorrelation Chart",
                  br(),
                  br(),
                  plotlyOutput(outputId = "p2"),
                  br(),
                  br()
                )
              )
            )
          )
        ),
        
        #Second Tab
        tabItem(
          tabName = "map",
          fluidRow(
            box(
              title = "Visualization Map",
              width = 12,
              status = "success",
              solidHeader = TRUE,
              collapsible = TRUE,
              br(),
              br(),
              leafletOutput("mp1"),
              br(),
              br(),
              br(),
              hr(),
              column(
                10,offset = 1,
                sliderInput(
                  animate = animationOptions(
                    loop = TRUE
                  ),
                  inputId = "yr2",
                  label = "Year : ",
                  min = 2009, max = 2016,
                  value = 2009
                )
              ),
              
              column(
                2,offset = 2,
               
                radioButtons(
                  inputId = "thm",
                  label = "Theme : ",
                  choices = c("Bright","Dark"),
                  selected = "Bright"
                ),
                br(),
                br(),
                br(),
                br(),
                br(),
                br(),
                br()
              ),
              column(
                4,offset = 2,
                
                selectInput(
                  inputId = "zId",
                  label = "Data to display : ",
                  choices = c("Hotel","Local tourist","Foreign tourist","Total tourist","Tourist ratio","Total employee"),
                  selected = "Hotel"
                ),
                br(),
                br(),
                br(),
                br(),
                br(),
                br(),
                br()
              )
            )
          )
        ),
        
        # Third Tab
        tabItem(
          tabName = "table",
          fluidRow(
            
            # Structure of data used for visualization
            box(
              title = "Plot and regression line data",
              width = 12,
              status = "success",
              solidHeader = TRUE,
              verbatimTextOutput("vis")
            ),
            
            #Pie chart data
            box(
              title = "Pie chart data",
              width = 12,
              status = "success",
              solidHeader = TRUE,
              verbatimTextOutput("pie")
            ),
            
            #Line chart data
            
            box(
              title = "Line chart data",
              width = 12,
              status = "success",
              solidHeader = TRUE,
              verbatimTextOutput("lne")
            ),
            
            #Line chart data
            
            box(
              title = "Crosscorrelation Data",
              width = 12,
              status = "success",
              solidHeader = TRUE,
              verbatimTextOutput("crs")
            ),
            
            #Line chart data
            
            box(
              title = "Leaflet Map Data",
              width = 12,
              status = "success",
              solidHeader = TRUE,
              verbatimTextOutput("leafl")
            ),
            
            # "Raw" data imported from excel
            box(
              title = "Raw Data from BPS Indonesia",
              width = 12,
              status = "success",
              solidHeader = TRUE,
              dataTableOutput(
                outputId = "bpsdat"
              ),
              style = 'overflow-x : scroll'
            )
          )
        )
      )
    )
  )
)

server <- function(input,output,session){
  
  # Calling chart visualization into server
  output$p1 <- renderPlotly({
    
    # Processed table for plot chart
    tab1 <- tourism %>% 
      filter(Year == input$yr) %>% 
      select(-c(6)) %>% 
      mutate(
        travellerratio = case_when(
          round(Total_traveller/Hotel) == "NaN"~0,
          TRUE~round(Total_traveller/Hotel)
        ),
        Employment = Employee_hotel*Hotel
      ) %>% 
      mutate_at("Province",as.character) %>% 
      arrange(Province)
    
    # Graphic plot assignmet
    gplot1 <- tab1 %>% 
      ggplot(
        aes(
         x= case_when(
           input$xId == "Hotel"~Hotel,
           input$xId == "Room"~Room,
           TRUE~Bed
         ),
         y= case_when(
           input$yId == "Local tourist"~Local_traveller,
           input$yId == "Foreign tourist"~Foreign_traveller,
           TRUE~Total_traveller
         )
        )
      )+
      scale_x_continuous(
        limits = case_when(
          input$xId == "Hotel"~c(-1,350),
          input$xId == "Room"~c(-100,40000),
          TRUE~c(-100,55500)
        )
      )+
      scale_y_continuous(
        limits = c(-1000,55000)
      )+
      geom_point(
        aes(
          colour= Province,
          size=travellerratio,
          text =(
            paste(
              "Province : ",tab1$Province,"<br>",
              "Tourist ratio per hotel : ",tab1$travellerratio
            )
          ) 
        ),
        show.legend = FALSE
      )+
      geom_vline(xintercept = case_when(
        input$xId == "Hotel"~mean(tab1$Hotel),
        input$xId == "Room"~mean(tab1$Room),
        TRUE~mean(tab1$Bed)
      ),
      linetype = "dotted",
      col = "red"
      )+
      geom_smooth(method = "lm", se=T, fullrange=T, fill="blue", alpha=0.25)+
      annotate(
        x=350,
        y=40000,
        label=paste("R= ",round(
          cor(case_when(
            input$xId == "Hotel"~tab1$Hotel,
            input$xId == "Room"~tab1$Room,
            TRUE~tab1$Bed
          ),
              case_when(
                input$yId == "Local tourist"~tab1$Local_traveller,
                input$yId == "Foreign tourist"~tab1$Foreign_traveller,
                TRUE~tab1$Total_traveller
              )
        ),3)
        ),
        geom="text"
      )+
      geom_hline(
        yintercept = case_when(
          input$yId == "Local tourist"~mean(tab1$Local_traveller),
          input$yId == "Foreign tourist"~mean(tab1$Foreign_traveller),
          TRUE~mean(tab1$Total_traveller)
        ),
        linetype = "dotted",
        col ="red"
      )+
      
      labs(
        x=case_when(
          input$xId == "Hotel"~"Number of Hotels",
          input$xId == "Room"~"Number of Rooms",
          TRUE~"Number of Beds"
        ),
        y=case_when(
          input$yId == "Local tourist"~"Number of Local Tourists",
          input$yId == "Foreign tourist"~"Number of Foreign Tourists",
          TRUE~"Number of Tourists"
        ),
        title = paste(
          "Number of Accomodation Facilities (",input$xId,"s)"," Vs. ",input$yId,"s",", ",input$yr,sep = ""
        )
      )+
      theme(
        legend.position = "none",
        plot.margin = margin(5,30,0,30),
        panel.border = element_rect(colour = "#008d4c",fill=NA, size = 1)
      )
    
    ggplotly(gplot1, tooltip = c("text","xintercept","yintercept") ) %>% 
      layout(xaxis=axy, yaxis=axy)# %>% 
      #config(displayModeBar = F) #To disable menu display in plot
    
  })
  
  # Rendering pie chart
  output$p3 <- renderPlotly({
    
    #Data processing for pie chart
    pre_pie <- tourism %>% 
      group_by(Year) %>% 
      summarise(
        Frgn = round(mean(Foreign_traveller),0),
        Locl =round(mean(Local_traveller),0)
      ) %>% 
      filter(Year == input$yr) %>% 
      rbind(c(0,0,0)) %>% 
      mutate(
        group = c("Foreign","Local"),
        value= c(Frgn[1],Locl[1])
      ) %>% 
      select(c(4,5))
    
    plot_ly(
      pre_pie,
      labels=pre_pie$group,
      values=pre_pie$value,
      type = 'pie',
      textinfo = 'label+percent',
      insidetextfont = list(color = '#FFFFFF'),
      hoverinfo = 'text',
      text = ~paste(value, ' tourists per day'),
      marker = list(
        colors = c("orange","green"),
        line = list(color = 'white', width = 3)
      ),
      showlegend = FALSE
    ) %>% 
      layout(
        title =paste(
          "Tourists ratio per day, ",input$yr,sep = ""
        )
      ) %>% 
      config(displayModeBar = F)
    
  })
  
  #Rendering line chart
  
  output$p4 <- renderPlotly({
    
    #Data processing
    pre_line <- tourism %>% 
      mutate(
        Empl = Hotel*Employee_hotel
      ) %>%
      dplyr::group_by(Year) %>% 
      dplyr::summarise(
        Employment = round(mean(Empl)),
        Hotel = round(mean(Hotel)),
        T.Tourists = round(mean(Total_traveller))
      ) %>% 
      slice(
        slice_no(input$yr)
      )
    
    pre_line_melt <- reshape2::melt(pre_line, id.vars = "Year") 
    
    #Plotting line chart
    pline <- pre_line_melt %>% 
      ggplot(aes(x = Year, y = value, fill=variable))+
      facet_wrap(variable ~ ., scales = "free")+
      geom_col()+
      geom_line(size=2)+
      geom_point(
        aes(
          x = Year,
          y = value,
          fill=variable,
          text = (
            paste(
              "Year : ",pre_line_melt$Year,"<br>",
              "Ave.value : ",pre_line_melt$value
            )
          )
        )
      )+
      geom_blank(aes(x=x, y=y),data = dummy )+
      scale_x_continuous(limits = c(2007,2017),breaks = c(2010,2012,2014,2016))+
      scale_fill_manual(values = c("red","#008000","#ffa500"))+
      labs(y="Ave. value", x="")+
      theme_minimal()+
      theme(
        legend.position = "none",
        strip.background = element_rect(
          color = "black",
          fill = "#FC4E07",
          size=1.5,
          linetype = "solid"
        ),
        strip.text.x = element_text(
          size = 10,
          color = "white",
          face = "bold"
        ),
        axis.text.x = element_text(
          angle = 45
        )
      )
    
    ggplotly(pline, tooltip = "text")
    
    
  })
  
  #Rendering heatmap crosscorrelation
  output$p2 <- renderPlotly({
    
    gplot2 <- crostab %>% 
      ggplot(aes(X1,X2,fill=value))+
      geom_tile(color="white")+
      scale_fill_gradient2(
        low = "#ff9800",
        high = "#31a354",
        mid = "#f7fcb9",
        midpoint = 0.875,
        limit = c(0.75,1),
        space = "Lab",
        name="Pearson\nCorrelation"
      )+
      labs(title = "Tourism Correlation Matrix")+
      theme_minimal()+
      theme(
        axis.text.x = element_text(
          angle = 45,
          vjust = 1,
          size = 12,
          hjust = 1
        ),
        axis.text.y = element_text(
          vjust = 10,
          size = 12
        ),
        panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_rect(colour = "#008d4c",fill=NA, size = 1)
      )
    
    
  })
  
  
  
  #Rendering Choloropleth Map
  
  output$mp1 <- renderLeaflet({
    
    #Similar tab from first graph
    tab1 <- tourism %>% 
      filter(Year == input$yr2) %>% 
      select(-c(6)) %>% 
      mutate(
        travellerratio = case_when(
          round(Total_traveller/Hotel) == "NaN"~0,
          TRUE~round(Total_traveller/Hotel)
        ),
        Employment = round(Employee_hotel*Hotel)
      ) %>% 
      mutate_at("Province",as.character) %>% 
      arrange(Province)
    
    
    #Importing data to geojson
    lfl@data <- lfl@data %>% 
      mutate(
        state = tab1$Province,
        Hotel = tab1$Hotel,
        Room = tab1$Room,
        Bed = tab1$Bed,
        Local = tab1$Local_traveller,
        Foreign = tab1$Foreign_traveller,
        Total = tab1$Total_traveller,
        Employment = tab1$Employment,
        Trvl_ratio = tab1$travellerratio
      ) %>% 
      arrange(id)
    
    #Setting bin scale for colormap
    bin <- case_when(
      input$zId == "Hotel"~c(0,70,140,210,280,350),
      input$zId == "Tourist ratio"~c(0,35,70,105,140,200),
      input$zId == "Total employee"~c(0,500,1000,10000,20000,45000),
      TRUE~c(0,100,500,1000,10000,40000)
    )
    
    
    #Setting pal based on data input
    pal <- colorBin(
      "YlOrRd",
      domain = case_when(
        input$zId == "Hotel"~lfl$Hotel,
        input$zId == "Local tourist"~lfl$Local,
        input$zId == "Foreign tourist"~lfl$Foreign,
        input$zId == "Total tourist"~lfl$Total,
        input$zId == "Tourist ratio"~lfl$Trvl_ratio,
        TRUE~lfl$Employment
      ),
      bins = bin
    )
    
    
    #Labels when mouse hovering on the map
    labels <- sprintf(
      "<strong>%s</strong><br/>%g %s",
      lfl$state,
      case_when(
        input$zId == "Hotel"~lfl$Hotel,
        input$zId == "Local tourist"~lfl$Local,
        input$zId == "Foreign tourist"~lfl$Foreign,
        input$zId == "Total tourist"~lfl$Total,
        input$zId == "Tourist ratio"~lfl$Trvl_ratio,
        TRUE~lfl$Employment
      ),
      case_when(
        input$zId == "Hotel"~"Hotels",
        input$zId == "Total employee"~"Employees'",
        TRUE~"Tourists"
      )
    ) %>%
      lapply(HTML)
    
    
    
    #Creating leaflet map
    m <- leaflet(lfl) %>% 
      addProviderTiles(
        case_when(
          input$thm == "Bright"~providers$CartoDB.Positron,
          TRUE~providers$CartoDB.DarkMatter
        )
      ) %>% 
      addPolygons(
        fillColor = ~pal(
          case_when(
            input$zId == "Hotel"~Hotel,
            input$zId == "Local tourist"~Local,
            input$zId == "Foreign tourist"~Foreign,
            input$zId == "Total tourist"~Total,
            input$zId == "Tourist ratio"~Trvl_ratio,
            TRUE~Employment
          )
        ),
        color = case_when(
          input$thm == "Bright"~"grey",
          TRUE~"white"
        ),
        weight = 1,
        opacity = 1,
        dashArray = "1",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 2,
          color = "green",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list(
            "font-weight" = "normal", padding = "3px 8px"
          ),
          textsize = "15px",
          direction = "auto")
      ) %>% 
      addLegend(
        pal = pal,
        values = case_when(
          input$zId == "Hotel"~"~Hotel",
          input$zId == "Local tourist"~"~Local",
          input$zId == "Foreign tourist"~"~Foreign",
          input$zId == "Total tourist"~"~Total",
          input$zId == "Tourist ratio"~"~Trvl_ratio",
          TRUE~"~Employment"
        ),
        opacity = 0.7,
        title = case_when(
          input$zId == "Hotel"~"Number of Hotels",
          input$zId == "Local tourist"~"Number of Local Tourists",
          input$zId == "Foreign tourist"~"Number of Foreign Tourists",
          input$zId == "Total tourist"~"Total Tourists",
          input$zId == "Tourist ratio"~"Tourist Ratio per Hotel",
          TRUE~"Total Employee"
        ),
        position = "topright"
      )
    
    
    
  })
  
  # Visualize processed data structure into server
  output$vis <- renderPrint({
    
    tab1 <- tourism %>% 
      filter(Year == input$yr) %>% 
      select(-c(5,6)) %>% 
      mutate(
        travellerratio = case_when(
          round(Total_traveller/Hotel) == "NaN"~0,
          TRUE~round(Total_traveller/Hotel)
        )
      )
    
    tab1 %>%
      glimpse()
    
  })
  
  #Pie chart data
  
  output$pie <- renderPrint({
    
    pre_pie <- tourism %>% 
      group_by(Year) %>% 
      summarise(
        Frgn = round(mean(Foreign_traveller),0),
        Locl =round(mean(Local_traveller),0)
      ) %>% 
      filter(Year == input$yr) %>% 
      rbind(c(0,0,0)) %>% 
      mutate(
        group = c("Foreign","Local"),
        value= c(Frgn[1],Locl[1])
      ) %>% 
      select(c(4,5))
    
    pre_pie %>% 
      glimpse()
    
  })
  
  #Line chart data
  
  output$lne <- renderPrint({
    
    pre_line <- tourism %>% 
      mutate(
        Empl = Hotel*Employee_hotel
      ) %>%
      dplyr::group_by(Year) %>% 
      dplyr::summarise(
        Employment = round(mean(Empl)),
        Hotel = round(mean(Hotel)),
        T.Tourists = round(mean(Total_traveller))
      )
    
    pre_line_melt <- reshape2::melt(pre_line, id.vars = "Year")
    
    pre_line_melt %>% 
      glimpse()
    
  })
  
  #Croscorelation heatmap data
  
  output$crs <- renderPrint({
    
    crostab %>% glimpse()
    
  })
  
  #Leaflet map data
  
  output$leafl <- renderPrint({
    
    lfl %>% glimpse()
    
  })
  
  # Visualize raw data table into server
  output$bpsdat <- renderDataTable({
    
    datatable(
      tourism,
      options = list(
        lengthMenu = c(5,10,50,100)
      )
    )
    
  })
  
}

shinyApp(ui, server)