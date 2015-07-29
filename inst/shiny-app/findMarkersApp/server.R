# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:

library(peprDnaStability)

shinyServer(function(input, output) {
    gel_image <- NULL; gel_cols <- NULL; lane_edge_df <- NULL
    norm_df <- NULL; peak_df <- NULL; gel_path <- NULL

    gel_image <- reactive({
                    if(!is.null(input$gel_file)){
                        gel <- input$gel_file
                        return(jpeg::readJPEG(gel$datapath) %>% gel_to_mat())
                    }
                    return(NULL)
                })
    lanes <- reactive({input$lanes})

    lane_edge_list <- reactive({
        if(is.null(gel_image())) return(NULL)
        lane_edges(gel_mat = gel_image(), lanes = lanes())
    })
    intensity_df <- reactive({
        if(is.null(gel_image())) return(NULL)
        lane_intensities(lane_edge_df = lane_edge_list()[['lane_edges_df']],
                         gel_mat = gel_image(),
                         lanes = lanes())
    })
    norm_df <- reactive({
        if(is.null(gel_image())) return(NULL)
        norm_intensity(intensity_df())
    })

    ladder_lanes <- reactive({input$ladder_lanes})
    bw_R1 <- reactive({input$bw_R1})
    bw_R2 <- reactive({input$bw_R2})
    bw_R3 <- reactive({input$bw_R3})
    npeaks_R1 <- reactive({input$npeaks_R1})
    npeaks_R2 <- reactive({input$npeaks_R2})
    npeaks_R3 <- reactive({input$npeaks_R3})
    leftset <- reactive({input$leftset})
    R1 <- reactive({input$R1})
    R2 <- reactive({input$R2})
    R3 <- reactive({input$R3})
    markers <- reactive({input$markers})

    peak_df <- reactive({
        if(is.null(gel_image())) return(NULL)
        r1_list = list(bw=bw_R1(),npeaks = npeaks_R1(),
                       leftset = leftset(), rightset = R1())
        r2_list = list(bw=bw_R2(),npeaks = npeaks_R2(),
                       leftset = R1(), rightset = R2())
        r3_list = list(bw=bw_R3(),npeaks = npeaks_R3(),
                       leftset = R2(), rightset = R3())
        calc_ladder_markers(norm_df(), ladder_lanes(), r1_list, r2_list, r3_list)})
    labeled_peaks <- reactive({
        if(is.null(gel_image())) return(NULL)
        add_marker(peak_df(), markers())
    })

    binned_df <- reactive({
        if(is.null(gel_image())) return(NULL)
        add_marker_bins(norm_df(), peak_df(), markers())
    })
    output$gel <- renderPlot({
        if(is.null(gel_image())) return(NULL)
        print(grid::grid.raster(gel_image()))
    })

    output$lanes <- renderPlot({
        if(is.null(gel_image())) return(NULL)
        print(check_lane_plot(gel_cols = lane_edge_list()[['col_intensity']],
                              lane_edges_df = lane_edge_list()[['lane_edges_df']]))
    })

     output$ladder <- renderPlot({
         if(is.null(gel_image())) return(NULL)
         print(check_marker_plot(norm_df(), peak_df(), ladder_lanes()))
     })

    output$markerTable <- renderDataTable({labeled_peaks()})

    output$downloadData <- downloadHandler(
        filename = function(){
            paste(input$gel_file$name, '.csv', sep='')
        },
        content = function(file){
            write.csv(filename=input$gel_file$name, labeled_peaks(), file, row.names = FALSE)
        }
    )
    output$downloadParams <- downloadHandler(
        filename = function(){
            paste(input$gel_file$name, '.Rdata', sep='')
        },
        content = function(file){
            filename <- input$gel_file$name
            lanes <- lanes()
            r1_list = list(bw=bw_R1(),npeaks = npeaks_R1(),
                           leftset = leftset(), rightset = R1())
            r2_list = list(bw=bw_R2(),npeaks = npeaks_R2(),
                           leftset = R1(), rightset = R2())
            r3_list = list(bw=bw_R3(),npeaks = npeaks_R3(),
                           leftset = R2(), rightset = R3())
            ladder_lanes <- ladder_lanes()
            marker_labels <- markers()
            labeled_peaks_df <- labeled_peaks()
            save(filename, lanes, marker_labels, labeled_peaks_df,
                 r1_list, r2_list, r3_list, ladder_lanes, file = file)
        }
    )
})
