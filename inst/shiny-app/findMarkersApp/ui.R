# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com

shinyUI(fluidPage(

  # Application title
  titlePanel("Stability Gel Processing"),

  # layout
  fluidRow(
      column(width = 3,
            fileInput('gel_file', 'Choose jpeg File',
                       accept=c('jpg/jpeg','.jpg','.jpeg')),
            numericInput('lanes','Enter Number of Lanes in the Gel',
                          value = 12, min = 1, max = 14, step = 1)),
            checkboxGroupInput("ladder_lanes", "Select Lanes with Ladders",
                         choices = list("Lane 1" = 1, "Lane 2" = 2, "Lane 3" = 3,
                                        "Lane 4" = 4, "Lane 5" = 5, "Lane 6" = 6,
                                        "Lane 7" = 7, "Lane 8" = 8, "Lane 9" = 9,
                                        "Lane 10" = 10, "Lane 11" = 11, "Lane 12" = 12),
                         selected = c(1))
    ),
    fluidRow(
    column(width = 6,
           plotOutput('gel')),
    column(width = 6,
           plotOutput('lanes'))#use offset = X to offset to left
  ),
  fluidRow(
      column(width = 4,
             sliderInput("bw_R1", "Bin Width Region 1",
                         min = 1, max = 100, value = 5),
             sliderInput("bw_R2", "Bin Width Region 2",
                         min = 1, max = 100, value = 5),
             sliderInput("bw_R3", "Bin Width Region 3",
                         min = 1, max = 100, value = 5)),
      column(width = 4,
             sliderInput("npeaks_R1", "Number of Peaks Region 1",
                         min = 1, max = 10, value = 4),
             sliderInput("npeaks_R2", "Number of Peaks Region 2",
                         min = 1, max = 10, value = 3),
             sliderInput("npeaks_R3", "Number of Peaks Region 3",
                         min = 1, max = 10, value = 2)),
      column(width = 4,
             sliderInput("leftset", "Left Offset", min = 0, max = 500, value = 10),
             sliderInput("R1", "Region 1 Right Bound",
                         min = 0, max = 2000, value = 500),
             sliderInput("R2", "Region 2 Right Bound",
                         min = 0, max = 2000, value = 1000),
             sliderInput("R3", "Region 3 Right Bound",
                         min = 0, max = 2000, value = 2000))
      ),
  fluidRow(
      column(width = 8,
             plotOutput('ladder')),
      # image dowloaded from
      # https://www.neb.com/products/n0350-low-range-pfg-marker
      column(2, img(src = "ladder.png")),
      column(width = 2,
        h6("Select same number of markers as number of peaks"),
        checkboxGroupInput('markers', 'Marker Size in kb',
                         choices = c(194,145.5,97,48.5,23.1,9.42,6.55,4.36,2.32,2.03),
                         selected = 194),
        downloadButton('downloadData', 'Download Data'),
        downloadButton('downloadParams', 'Download Rdata'))
      ),
  fluidRow(
      column(width = 12,
             dataTableOutput('markerTable')))
  )
)
