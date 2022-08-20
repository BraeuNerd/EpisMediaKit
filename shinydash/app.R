
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(treemapify)
library(rnaturalearth)
library(plotly)
library(leaflet)
library(ggraph)
library(igraph)

# LOAD DATA ---

streams <- read_csv("data/EpistemasStreams.csv")
streams$date <- as.Date(streams$date, "%d-%m-%y")

weeks <- as.Date(cut(streams$date,
                            breaks = "week",
                            start.on.monday = T))
desc <- streams$streamsSpot+streams$ivoox
weekly <- cbind.data.frame(weeks, desc)



guests <- read_csv("data/EpistemasGuests.csv")
#guests$pregrado <- as.factor(guests$pregrado)
#guests$stem <- as.factor(guests$stem)
guests$nacionalidad <- as.factor(guests$nacionalidad)

streams_paises <- read_csv("data/EpistemasPaisStr.csv")

streams_paises <- streams_paises %>%
  mutate(Total = Spotify+Ivoox)

redes <- read_csv("data/EpistemasRedes.csv")

edad <- read_csv("data/EpistemasEdad.csv")

# Define UI --------------------------


ui <- fluidPage(
  tags$h2(a(img(src = "epiSTEMas_Logo.png", href = "https://www.epistemas.com", height = 90, width = 90), "Epistemas Media Kit",href = "https://www.epistemas.com", style = "margin: 0; ")), 
  tags$text("última actualización de datos: 02 de agosto 2022", style = "font-size: 14px; color: #858585; "),
  
  setBackgroundColor(color = "#000000"),
  useShinydashboard(),

  ## modify infoBoxes colors
  tags$style(".small-box {border-color: #156458; }
             .info-box.bg-green { background-color: #1db954 !important; color: #ffffff !important; }
             .info-box.bg-purple { background-color: #986538 !important; color: #ffffff !important; }
             .info-box.bg-fuchsia { background-color: #E4405F !important; color: #ffffff !important; }
             .info-box.bg-aqua { background-color: #f5dd3a !important; color: #000000 !important; }
             .info-box.bg-blue { background-color: #1877F2 !important; color: #ffffff !important; }
             .info-box.bg-light-blue { background-color: #1DA1F2 !important; color: #ffffff !important; }
             .info-box.bg-yellow { background-color: #6bcbe3 !important; color: #ffffff !important; }
             .info-box.bg-navy { background-color: #0A66C2 !important; color: #ffffff !important; }"),
#  tags$style(HTML(".fa-spotify { font-size: 20px; }")),
#  tags$style(HTML(".fa-microphone { font-size: 20px; }")),
#  tags$style(HTML(".fa-link { font-size: 20px; }")),
# tags$style(HTML(".fa-newspaper { font-size: 20px; }")),
#  tags$style(HTML(".fa-instagram { font-size: 20px; }")),
#  tags$style(HTML(".fa-facebook { font-size: 20px; }")),
#  tags$style(HTML(".fa-twitter { font-size: 20px; }")),
#  tags$style(HTML(".fa-linkedin { font-size: 20px; }")),

  tags$style(".box.box-warning, .box.box-info, .box.box-success {border-color: #f5dd3a; background: #454545; }"),

  tags$style("h2 { color: #ffffff; }"),
  tags$style("h5 { color: #959595; }"),
  tags$style("h3 { color: #ffffff; padding: 0; margin: 0; }"),
  tags$style("h4 { color: #000000; font-weight: bold }"),
  tags$style("text {font-size: 25px; color: #151515;}"),

#  tags$style(".navbar {background-color: #6bcbe3; }
#             .navbar > .active > a {color: blue; background-color: purple; }
#             "),


# Start pages & tabs ---

navbarPage(title = h3("Rompiendo estereotipos de la ciencia en español", style = "font-size: 90%; color: #000000"),
           windowTitle = "EpiSTEMas Media Kit",
           
           tabPanel(tags$text("Quiénes nos escuchan", style = "font-size: 100%"), icon = icon("globe-americas"),
                    style = "padding: 10px; ",
                    
                    fluidRow(
                      column(7,
                             fluidRow(
                               plotlyOutput("streamspaises"),
                               #"Probando este texto",
                               plotlyOutput("descargas_mapa")
                             )),
                      column(5, 
                             fluidRow(
                               plotlyOutput("edad"),
                               plotlyOutput("donut")
                             ) #close fluidrow
                      ) #close column
                    ) #close fluidRow
           ), #close first tabPanel
           
           tabPanel(tags$text("Cómo nos encuentran", style = "font-size: 100%"), icon = icon("map"),
                    
                    fluidRow(style = "padding: 50px;",
                             column(6,
                                    box(title = "Seguidores en plataformas de Podcasts:",
                                        width = 12,
                                        status = "success",
                                        solidHeader = F,
                                        infoBox(title = "Spotify",
                                                value = redes[1,2],
                                                subtitle = "Seguidores",
                                                icon = icon("spotify"),
                                                color = "green",
                                                width = 6, 
                                                fill = T),
                                        infoBox(title = "Otras plataformas",
                                                value = sum(26+2+19+7),
                                                subtitle = "(Apple, Google, Castbox, y otras.)",
                                                icon = icon("microphone"),
                                                color = "purple",
                                                width = 6,
                                                fill = T)
                                    ), #close box1,
                                    box(title = "Alcance de nuestros sitios:",
                                        width = 12,
                                        status = "info",
                                        solidHeader = F,
                                        infoBox(title = "Sitio web",
                                                value = redes[12,2],
                                                subtitle = "Visitas promedio por mes",
                                                icon = icon("link"),
                                                color = "aqua",
                                                width = 6, 
                                                fill = T),
                                        infoBox(title = "Boletín",
                                                value = redes[2,2],
                                                subtitle = "Suscripciones (open rate %)",
                                                icon = icon("newspaper"),
                                                color = "yellow",
                                                width = 6,
                                                fill = T)
                                    ) #close box2
                             ), #close column
                             column(6,
                                    box(title = "Redes sociales:",
                                        width = 12,
                                        status = "warning",
                                        solidHeader = F,
                                        infoBox(title = "Instagram",
                                                value = redes[4,2],
                                                subtitle = "Seguidores",
                                                icon = icon("instagram"),
                                                color = "fuchsia",
                                                width = 6, 
                                                fill = T,
                                                href = "https://www.instagram.com/epistemas"),
                                        infoBox(title = "Instagram",
                                                value = redes[6,2],
                                                subtitle = "Alcance (# de cuentas del mes anterior)",
                                                icon = icon("instagram"),
                                                color = "fuchsia",
                                                width = 6,
                                                fill = T),
                                        infoBox(title = "Facebook",
                                                value = redes[7,2],
                                                subtitle = "Seguidores",
                                                icon = icon("facebook"),
                                                color = "blue",
                                                width = 6, 
                                                fill = T),
                                        infoBox(title = "Facebook",
                                                value = redes[9,2],
                                                subtitle = "Alcance (# cuentas del mes anterior)",
                                                icon = icon("facebook"),
                                                color = "blue",
                                                width = 6,
                                                fill = T),
                                        infoBox(title = "Twitter",
                                                value = redes[10,2],
                                                subtitle = "Seguidores",
                                                icon = icon("twitter"),
                                                color = "light-blue",
                                                width = 6, 
                                                fill = T),
                                        infoBox(title = "LinkedIn",
                                                value = redes[11,2],
                                                subtitle = "Seguidores",
                                                icon = icon("linkedin"),
                                                color = "navy",
                                                width = 6,
                                                fill = T)
                                    ) #close box
                             ) #close second column
                    ), #close main fluidRow
           ), #close first tabPanel
           
           tabPanel(tags$text("Cuándo nos escuchan", style = "font-size: 100%"), icon = icon("headphones-alt"),
                    style = "padding: 20px; ",
                    
                    fluidRow(
                      plotlyOutput("streamsts",
                                   height = "650px")
                    ) #close fluidrow
           ), #close tabPanel

          tabPanel(tags$text("Nuestr@s Invitad@s", style = "font-size: 100%"), icon = icon("globe-americas"),
                   style = "padding: 10px; ",
                   
                   fluidRow(
                     column(5,
                            fluidRow(
                              plotlyOutput("invitados")
                            )),
                     column(7, 
                            #offset = 1,
                            fluidRow(
                              plotOutput("treemap",
                                         height = "70vh")
                            ) #close fluidrow
                     ) #close column
                   ) #close fluidRow
          ), #close first tabPanel


) #close navbarPage
)


#TO DO> ADD HREF TO VALUEBOXES!!!


# Server component -------------------

server <- function(input, output){
  
  weeklystrts <- weekly %>%
    group_by(weeks) %>%
    summarize(value = sum(desc))
  
  
  output$streamspaises <- renderPlotly({
    
    streams_paises %>%
      filter(Total>40) %>%
      mutate(Pais = fct_reorder(Pais, Total)) %>%
      arrange(desc(Total)) %>%
      plot_ly(y = ~Pais, 
              x = ~Total, 
              color = ~Pais, 
              type = "bar", 
              colors = c("#f5dd3a","#6bcbe3"),
              showlegend = F,
              orientation = "h") %>%
      layout(font = list(color = "#858585"),
             title = list(text = "Paises desde donde más nos escuchan:",
                          x = 0.1,
                          font = list(size = 14, color = "#ffffff")),
             xaxis = list(title = "No. de Escuchas Totales",
                          font = list(size = 10),
                          gridcolor = "#353535"),
             yaxis = list(title = " "),
             paper_bgcolor = "#171717",
             plot_bgcolor = "#171717",
             margin = list(t= 50,r= 0,b= 30,l= 0))
    
  })

  
  output$descargas_mapa <- renderPlotly({
    #text on hover
    streams_paises$hover <- with(streams_paises, paste(Pais, '<br>',
                                                       "Spotify", Spotify, "<br>",
                                                       "Otras plataformas", Ivoox))

    # specify map projection/options
    g <- list(
      showframe = T,
      showland = T,
      landcolor = toRGB("grey70"),
      showocean = T,
      oceancolor = toRGB("LightBlue"),
      showlakes = T,
      lakecolor = toRGB("LightBlue"),
      showcoastlines = T,
      coastlinecolor = toRGB("#171717"),
      showcountries = T,
      countrycolor = toRGB("#171717"),
      countrywidth = 0.2,
      resolution = 20,
      fitbounds = "locations",
      center = list(lon = -55, lat = 14),
#     projection = list(type = 'natural earth')
#      projection = list(type = 'orthographic')
      projection = list(type = 'winkel tripel', scale=4)
    )
    
    map <- plot_geo(streams_paises,
                    type = "choropleth",
                    locations = streams_paises$code,
                    z = streams_paises$Total,
                    text = streams_paises$hover,
                    showscale = F,
                    colors = c("#f5dd3a","#6bcbe3"),
                    marker = list(line = list(color = toRGB("#986538"), width = 0.5)),
                    colorbar = list(xpad = 0,
                                    ypad = 0)) %>%
      layout(geo=g,
             margin = list(t=30,r= 0,b= 0,l= 0),
             paper_bgcolor = "#171717",
             plot_bgcolor = "#171717",
             title = list(text = "Todos los paises en donde nos han escuchado:",
                          x = 0.2,
                          font = list(size = 14, color = "#ffffff")))
    map
  })
  
  
  
  
  output$edad <- renderPlotly({
    

    edad[-c(2:6)] %>%
      pivot_longer(!`Rango Edad`, names_to = "genero", values_to = "n") %>%
      mutate(Percentage = round((n/sum(n))*100)) %>%
      mutate(hover = paste(Percentage, "%")) %>%
      plot_ly(x = ~`Rango Edad`,
              y = ~Percentage,
            type = "bar",
            name = ~genero,
            text = ~hover,
            color = ~genero,
            colors = c(Masculino = "#986538", Femenino = "#6bcbe3", `No binario` = "#f5dd3a", `No especifica` = "white")) %>%
      layout(barmode = "stack", bargap = 0.05,
             xaxis = list(title = "Rangos de edades"),
             yaxis = list(title = "%"),
             paper_bgcolor = "#171717",
             plot_bgcolor = "#171717",
             font = list(color = "#858585"),
             legend = list(x = 0.7, y = 1, bgcolor = 'rgba(255, 255, 255, 0)', bordercolor = 'rgba(255, 255, 255, 0)')
             ) #close layout
    
  })
  
  output$donut <- renderPlotly({
    
    edad[-c(1:6)] %>%
      pivot_longer(cols = everything(), names_to = "genero", values_to = "n") %>%
      group_by(genero) %>%
      summarize(n = sum(n)) %>%
      mutate(Porcentaje = round((n/sum(n))*100),
             hover = paste(Porcentaje, "%")) %>%
    plot_ly(labels = ~`genero`,
            values = ~Porcentaje, 
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'label+percent',
            text = ~paste(Porcentaje, ' %'),
            marker = list(colors = c("#6bcbe3", "#986538", "white", "#f5dd3a")),
            showlegend = F,
        insidetextorientation = "radial") %>%
      add_pie(hole = 0.5) %>%
      layout(paper_bgcolor = "#171717",
           plot_bgcolor = "#171717",
           title = list(text = "En su mayoría nos escuchan hombres y mujeres entre 22 y 35 años de edad",
                        x = 0.1,
                        font = list(size = 14, color = "#ffffff")))
  
  })
  
  output$streamsts <- renderPlotly({
    
    streams$hover <- with(streams, paste("Escuchas diarias en:", '<br>',
                                         "Spotify:", streamsSpot, "<br>",
                                         "Otras plataformas:", ivoox))

    weeklystrts <- weekly %>%
      group_by(weeks) %>%
      summarize(value = sum(desc))

    summary(weeklystrts) # en promedio desde abril 2021 tenemos 100 escuchas semanales
    
    weeklystrts %>%
      filter(weeks <= "2021-12-31") %>%
      summary() # en promedio en 2021 tuvimos 87 escuchas semanales
    
    weeklystrts %>%
      filter(weeks > "2021-12-31") %>%
      summary() # en promedio en 2022 tenemos 116 escuchas semanales
    
    streams %>%
      mutate(StreamsTot = streamsSpot + ivoox,
             cums = cumsum(StreamsTot)) %>%
      plot_ly(x = ~date,
              y = ~StreamsTot,
              type = "scatter",
              mode = "lines",
              text = ~hover,
              name = "Descargas diarias",
              line = list(color = "#6bcbe3",
                          width = 1.8)) %>%
      add_trace(x = ~date,
                y = ~cums,
                type = "scatter",
                mode = "lines",
                name = "Descargas acumuladas",
                yaxis = "y2",
                line = list(color = "#f5dd3a",
                            width = 1.5,
                            dash = "dot")) %>%
      add_trace(data = weeklystrts,
                x = ~weeks,
                y = ~value,
                type = "scatter",
                mode = "lines",
                name = "Semanales",
                inherit = F,
                line = list(color = "#6bcbe3",
                            width = 1.5,
                            dash = "dot")) %>%
      layout(xaxis = list(title = "",
                          tickangle = 90),
             yaxis = list(tickfont = list(color = "#6bcbe3"),
                          title = list(text = "Descargas",
                                       standoff = 25)),
             yaxis2 = list(tickfont = list(color="#f5dd3a"),
                           overlaying = "y",
                           side = "right",
                           title = list(text = "Descargas acumuladas",
                                        standoff = 25))) %>%
      layout(xaxis = list(
        zerolinecolor = 'ffff',
        zerolinewidth = 2,
        gridcolor = '#353535'),
        yaxis = list(
          zerolinecolor = '#171717',
          zerolinewidth = 2,
          gridcolor = '#171717'),
        paper_bgcolor = "#171717",
        plot_bgcolor = "#171717",
        font = list(color = "#858585"),
        legend = list(x = 0.05, y = 0.95),
        title = list(text = "En promedio tenemos  100 descargas por semana", 
                     font = list(size = 14, color = "#858585")),
        margin = list(t= 50,r= 80,b= 20,l= 70))

  })
  
  
  output$invitados <- renderPlotly({
    
    guests %>%
      group_by(nacionalidad) %>%
      summarize(n = n()) %>%
      arrange(desc(n)) %>%
      plot_ly(y = ~nacionalidad, 
              x = ~n, 
              color = ~nacionalidad, 
              type = "bar", 
              colors = c("#f5dd3a","#6bcbe3"),
              showlegend = F) %>%
      layout(font = list(color = "#858585"),
             title = list(text = "Nuestros invitados son de:",
                          x = 0.1,
                          font = list(size = 14, color = "#ffffff")),
             xaxis = list(title = "Personas",
                          font = list(size = 10),
                          gridcolor = "#353535"),
             yaxis = list(title = " ",
                          font = list(color = "#858585")),
             paper_bgcolor = "#171717",
             plot_bgcolor = "#171717",
             margin = list(t= 50,r= 10,b= 30,l= 10))

  })
  
  
  
  output$treemap <- renderPlot({
    
    treestem <-  guests %>%
      group_by(stem, pregrado) %>%
      summarize(n = n(),
                prop = (n/66)*100)
    
    treestem %>%
      ggplot(aes(area = n, fill = stem, subgroup = stem, subgroup2 = pregrado, label = pregrado)) +
      geom_treemap(start = "topleft", layout = "srow") +
      geom_treemap_subgroup2_border(color = "#171717", size = 0.1, start = "topleft", layout = "srow") +
      geom_treemap_subgroup_border(color = "#171717", size = 0, start = "topleft", layout = "srow") +
      geom_treemap_subgroup_text(color = "#000000", alpha = 0.1, angle = 30, grow=T, place = "center", fontface="italic", start = "topleft", layout = "srow") +
      geom_treemap_subgroup2_text(color = "#000000", alpha = 0.9, size = 15, reflow=T, place = "center", fontface="bold", start = "topleft", layout = "srow") +
      scale_fill_manual(values = c("#f5dd3a", "#986538", "#f5dd3a", "#6bcbe3"))+
      theme(legend.position = "none",
            plot.background = element_rect(fill = "#000000", color = "#000000"),
            panel.background = element_rect(fill = "#000000")
            )
  })
  
}


# Call shinyApp ----------------------

shinyApp(ui = ui, server = server)