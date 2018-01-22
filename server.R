## server.R

library(shiny)

function(input, output, session) {
  
  ################################################################################
  # REACTIVE VALUES                                                              #
  ################################################################################
  v <- reactiveValues(
    prov_df = NULL,
    state_df = NULL,
    data_files_df = get_datafiles_info()
  )
  
  ################################################################################
  # POPULATE VALUE BOXES                                                         #
  ################################################################################
  output$numFacilities <- renderValueBox({
    valueBox(
      withProgress(message = 'Counting...', detail = 'facilities', {nrow(get_dataset('PROVIDER_INFO'))}),
      'Number Of Facilities', icon = icon('building'), color = 'blue')
  })
  output$numResidents <- renderValueBox({
    valueBox(
      withProgress(message = 'Counting...', detail = 'residents', {sum(get_dataset('PROVIDER_INFO')$RESTOT)}),
      'Number Of Residents', icon = icon('users'), color = 'green')
  })
  output$numImmediateJeopardy <- renderValueBox({
    valueBox(
      withProgress(message = 'Counting...', detail = 'penalties', {sum(get_dataset('SURVEY_SUMMARY')$H_IJ_N)}),
      'Number Of Immediate Jeopardy Penalties', icon = icon('thumbs-down'), color = 'red')
  })
  
  ################################################################################
  # POPULATE SELECTIZE CHOICES                                                   #
  ################################################################################
  updateSelectizeInput(
    session, 'prov_dataset', choices = provnum_datasets, selected = 'PROVIDER_INFO', server = FALSE
  )
  
  ################################################################################
  # DOWNLOAD HANDLERS                                                            #
  ################################################################################
  output$downloadMetaData <- downloadHandler(
    filename = 'metadata.xls',
    contentType = 'application/vnd.ms-excel',
    content <- function(file) {
      # TODO: Locate this file dynamically
      file.copy(paste0(app_data, 'DataMedicareGov_MetadataAllTabs_v12.xls'), file)
    }
  )
  
  output$downloadStateAverages <- downloadHandler(
    filename = basename(get_filename('STATE_AVG')),
    content <- function(file) { file.copy(get_filename('STATE_AVG'), file) }
  )
  
  output$downloadProviderData <- downloadHandler(
    filename <- function() { basename(get_filename(input$prov_dataset)) },
    content <- function(file) { file.copy(get_filename(input$prov_dataset), file) }
  )
  
  ################################################################################
  # MODAL DIALOG HANDLERS                                                        #
  ################################################################################
  observeEvent(
    input$providerDetails,  {
      if (!is.null(input$provDataTable_rows_selected)) {
        provider_num <- v$prov_df$PROVNUM[[input$provDataTable_rows_selected]]
        #print(provider_num)
        rec <- get_dataset('PROVIDER_INFO') %>% filter(PROVNUM == provider_num)
        if (nrow(rec) == 1) {
          showModal(
            modalDialog(
              title = 'PROVIDER DETAILS',
              size = 'l', easyClose = TRUE,
              div(
                box (
                  width = 12, solidHeader = TRUE, status = 'info',
                  fluidRow(
                    column(
                      width = 8,
                      h3(paste0('OVERALL RATING: ', five_star(rec$OVERALL_RATING))),
                      h4(
                        paste0('NAME: ', rec$PROVNAME), br(),
                        paste0('ADDRESS: ', rec$ADDRESS), br(),
                        paste0('STATE: ', rec$STATE), br(),
                        paste0('COUNTY: ', rec$COUNTY_NAME), br(),
                        paste0('CITY: ', rec$CITY), br(),
                        paste0('ZIP: ', rec$ZIP), br()
                      )
                    ),
                    column(
                      width = 4,
                      shiny::img(src = paste0('images/nf-', as.character(as.numeric(rec$PROVNUM) %% 10), '.jpeg'), width = '100%', class = 'rightAlign')
                    )
                  )
                ),
                box(
                  width = 12, solidHeader = TRUE, title = 'Additional Information', status = 'info', collapsible = TRUE,
                  div(
                    paste0('OWNERSHIP: ', rec$OWNERSHIP), br(),
                    paste0('CERTIFICATION: ', rec$CERTIFICATION), br(),
                    paste0('PARTICIPATION DATE: ', rec$PARTICIPATION_DATE), br(),
                    paste0('CERTIFIED BEDS: ', rec$BEDCERT), br(),
                    paste0('RESIDENTS: ', rec$RESTOT), br(),
                    paste0('OCCUPANCY: ', 100 * rec$RESTOT / rec$BEDCERT, '%'), br(),
                    paste0('PARTICIPATION DATE: ', rec$PARTICIPATION_DATE), br(),
                    paste0('FEDERAL PROVIDER NUMBER: ', rec$PROVNUM), br()
                  )
                )
              )
            )
          )
        } else {
          showModal(
            modalDialog(
              title = 'PROVIDER DETAILS: NOT FOUND',
              size = 'm', easyClose = TRUE,
              h2('PROVIDER INFORMATION NOT FOUND :(')
            )
          )
        }
      }
    }
  )
  
  ################################################################################
  # REFRESH DATA HANDLER                                                         #
  ################################################################################
  observeEvent(
    input$refreshData, withProgress(message = 'Refreshing data...', { 
      refresh_data()
      v$data_files_df <- get_datafiles_info()
    })
  )
  
  ################################################################################
  # DATA TABLE RENDERERS                                                         #
  ################################################################################
  output$dataFilesTable <- DT::renderDataTable({
    withProgress(message = 'Processing data files...', detail = app_data, {
      subset(v$data_files_df, select = -c(mode, isdir) )
    })},
    server = FALSE,
    class = 'cell-border stripe',
    filter = 'top',
    selection = 'single',
    colnames = c('Filename' = 1),
    extensions = c('Buttons', 'Scroller'),
    options = list(
      dom = 'Bfrtip', buttons = list(list(extend = 'colvis')),
      scrollX = TRUE, scrollY = 600, scroller = TRUE, searchHighlight = TRUE, autoWidth = TRUE
    )
  )
  
  output$provDataTable <- DT::renderDataTable({
    withProgress(message = 'Processing dataset...', detail = input$prov_dataset, {
      result_df <- get_dataset(input$prov_dataset)
      numeric_columns <- names(result_df[sapply(result_df, is.numeric)])  # TODO:  I'm sure there is a better way to do this
      updateSelectizeInput(session, 'xcol', choices = numeric_columns, selected = numeric_columns[[1]])
      updateSelectizeInput(session, 'ycol', choices = numeric_columns, selected = numeric_columns[[2]])
      shiny::validate(need(is.data.frame(result_df), 'No data.'))
      if (!is.null(input$states_of_interest)) result_df <- filter(result_df, STATE %in% get_state_abb(input$states_of_interest))
      v$prov_df <- result_df
      v$prov_df
    })},
    server = TRUE,
    class = 'cell-border stripe',
    filter = 'top',
    selection = 'single',
    colnames = c('ROW_ID' = 1),
    extensions = c('Buttons', 'Scroller'),
    options = list(
      dom = 'Bfrtip', buttons = list(list(extend = 'colvis')),
      scrollX = TRUE, scrollY = 600, scroller = TRUE, searchHighlight = TRUE, autoWidth = TRUE
    )
  )
  
  output$hasProvData <- reactive({is.data.frame(get_dataset(input$prov_dataset))})
  outputOptions(output, 'hasProvData', suspendWhenHidden = FALSE)
  
  output$stateAveragesDataTable <- DT::renderDataTable({
    withProgress(message = 'Processing dataset...', detail = 'STATE_AVG', {
      result_df <- get_dataset('STATE_AVG')
      shiny::validate(need(is.data.frame(result_df), 'No data.'))
      if (!is.null(input$states_of_interest)) result_df <- filter(result_df, STATE %in% get_state_abb(input$states_of_interest))
      v$state_df <- result_df
      v$state_df
    })},
    server = FALSE,
    class = 'cell-border stripe',
    filter = 'top',
    selection = 'single',
    colnames = c('ROW_ID' = 1),
    extensions = c('Buttons', 'Scroller'),
    options = list(
      dom = 'Bfrtip', buttons = list(list(extend = 'colvis')),
      scrollX = TRUE, scrollY = 600, scroller = TRUE, searchHighlight = TRUE, autoWidth = TRUE
    )
  )
  
  ################################################################################
  # BAR GRAPH RENDERERS                                                          #
  ################################################################################
  output$dashBoardDeficienciesByStateBar <- renderPlotly({
    depiction <- paste(input$dashboard_depiction, input$dashboardAggregateBy, sep = '_')
    withProgress(
      message='Rendering bar graph...', detail = depiction, {
        filteredScope <- deficiencies.all
        if (input$dashboard_depiction == 'SUBSTANDARD_DEFICIENCIES') filteredScope <- deficiencies.substandard
        df <- transform(
          merge(get_dataset('DEFICIENCIES') %>%
                  filter(STATE %in% get_state_abb(input$dashboardFilterStates)) %>%
                  filter(SCOPE %in% filteredScope) %>%
                  count(STATE, SCOPE),
                get_dataset('RESIDENTS_BY_STATE'), by = 'STATE', all.x = TRUE),
          DEFICIENCIES_BY_STATE_PER_RESIDENT = n / RESIDENTS_BY_STATE,
          DEFICIENCIES_BY_STATE_PER_FACILITY = n / FACILITIES_BY_STATE
        )
        df$SCOPE <- factor(df$SCOPE, filteredScope)
        if (input$dashboardAggregateBy == 'BY_STATE_PER_RESIDENT') {
          plot_ly(
            df,
            x =  ~ STATE,
            y =  ~ DEFICIENCIES_BY_STATE_PER_RESIDENT,
            color =  ~ SCOPE,
            source = 'dashBoardDeficienciesByStateBar',
            colors = brewer.pal(length(filteredScope), 'Set3'),
            marker = list(line = list(color = '#FFFFFF', width = 1)),
            type = 'bar'
          ) %>% plotly::config(displaylogo = FALSE, collaborate = FALSE) %>%
            plotly::layout(
              barmode = 'stack', legend = list(orientation = 'v', borderwidth = 1),
              title = str_title_case(tolower(depiction)),
              xaxis = list(title = 'US State or Territory'), yaxis = list(title = 'Deficiencies Per Resident')
            )
        } else if (input$dashboardAggregateBy == 'BY_STATE_PER_FACILITY') {
          plot_ly(
            df,
            x =  ~ STATE,
            y =  ~ DEFICIENCIES_BY_STATE_PER_FACILITY,
            color =  ~ SCOPE,
            source = 'dashBoardDeficienciesByStateBar',
            colors = brewer.pal(length(filteredScope), 'Set3'),
            marker = list(line = list(color = '#FFFFFF', width = 1)),
            type = 'bar'
          ) %>% plotly::config(displaylogo = FALSE, collaborate = FALSE) %>%
            plotly::layout(
              barmode = 'stack', legend = list(orientation = 'v', borderwidth = 1),
              title = str_title_case(tolower(depiction)),
              xaxis = list(title = 'US State or Territory'), yaxis = list(title = 'Deficiencies Per Facility')
            )
        } else {
          plot_ly(
            df,
            x =  ~ STATE,
            y =  ~ n,
            color =  ~ SCOPE,
            source = 'dashBoardDeficienciesByStateBar',
            colors = brewer.pal(length(deficiencies.all), 'Set3'),
            marker = list(line = list(color = '#FFFFFF', width = 1)),
            type = 'bar'
          ) %>% plotly::config(displaylogo = FALSE, collaborate = FALSE) %>%
            plotly::layout(
              barmode = 'stack', legend = list(orientation = 'v', borderwidth = 1),
              title = str_title_case(tolower(depiction)),
              xaxis = list(title = 'US State or Territory'), yaxis = list(title = 'Deficiencies')
            )
        }
      })
  })
  
  ################################################################################
  # PIE GRAPH RENDERERS                                                          #
  ################################################################################
  output$dashBoardDeficienciesPie <- renderPlotly({
    withProgress(
      message='Rendering pie graph...', detail = input$dashboard_depiction,  {
        plot_ly(
          {
            filteredScope <- deficiencies.all
            if (input$dashboard_depiction == 'SUBSTANDARD_DEFICIENCIES') filteredScope <- deficiencies.substandard
            df <- get_dataset('DEFICIENCIES') %>%
              filter(STATE %in% get_state_abb(input$dashboardFilterStates)) %>% filter(SCOPE %in% filteredScope) %>%
              count(SCOPE)
            df$SCOPE <- factor(df$SCOPE, filteredScope)
            df
          }, labels =  ~ SCOPE, values =  ~ n, type = 'pie',
          textposition = 'inside',
          textinfo = 'label+percent',
          insidetextfont = list(color = '#FFFFFF'),
          hoverinfo = 'label+text+percent',
          source = 'dashBoardDeficienciesPie',
          text =  ~ paste(n),
          marker = list(
            colors = brewer.pal(length(deficiencies.all), 'Set3'),
            line = list(color = '#FFFFFF', width = 1)
          )
        ) %>% plotly::config(displaylogo = FALSE, collaborate = FALSE) %>%
          layout(
            showlegend = T,
            legend = list(orientation = 'v', borderwidth = 1),
            title = paste0(str_title_case(tolower(input$dashboard_depiction)), ' In Selected States and Territories'),
            xaxis = list(
              showgrid = FALSE,
              zeroline = FALSE,
              showticklabels = FALSE
            ),
            yaxis = list(
              showgrid = FALSE,
              zeroline = FALSE,
              showticklabels = FALSE
            )
          )
      })
  })
  
  ################################################################################
  # CHOROPLETH MAP RENDERERS                                                     #
  ################################################################################
  makeLabels <- function(dataSVO) {
    sprintf(
      '<strong>%s</strong><img src="images/flags-mini/%s.png" class = "rightAlign"/><br/>
      Residents: %g<br/>
      Facilities: %g<br/>
      Residents Per-Facility: %g<br/>
      Occupancy: %g%%<br/>
      Deficiencies: %g<br/>
      Deficiencies Per-Facility: %g<br/>
      Deficiencies Per-Resident: %g<br/>
      Substandard Deficiencies: %g<br/>
      Substandard Deficiencies Per-Facility: %g<br/>
      Substandard Deficiencies Per-Resident: %g<br/>
      ',
      dataSVO$NAME10, tolower(dataSVO$STUSPS10),
      dataSVO$RESIDENTS_BY_STATE,
      dataSVO$FACILITIES_BY_STATE,
      dataSVO$RESIDENTS_BY_STATE_PER_FACILITY,
      dataSVO$OCCUPANCY_BY_STATE,
      dataSVO$DEFICIENCIES_BY_STATE,
      dataSVO$DEFICIENCIES_BY_STATE_PER_FACILITY,
      dataSVO$DEFICIENCIES_BY_STATE_PER_RESIDENT,
      dataSVO$SUBSTANDARD_DEFICIENCIES_BY_STATE,
      dataSVO$SUBSTANDARD_DEFICIENCIES_BY_STATE_PER_FACILITY,
      dataSVO$SUBSTANDARD_DEFICIENCIES_BY_STATE_PER_RESIDENT
    ) %>% lapply(htmltools::HTML)
  }

  addShadeToChoropleth <- function(mapId, dataSVO, field) {
    withProgress(
      message='Rendering choropleth...', detail = field, {
        dataSVO$histoField <- dataSVO[[field]]
        paletteFn <- colorNumeric(palette = 'YlOrRd', domain = dataSVO$histoField, na.color = 'white')
        #View(dataSVO@data)
        leafletProxy('dashBoardChoropleth', data = dataSVO) %>% clearControls() %>%
          addLegend(
            values = dataSVO$histoField, title = str_title_case(tolower(field)),
            pal = paletteFn, position = 'topright', opacity = 0.6
          ) %>% clearShapes() %>% addPolygons(
            group = field, fillColor =  ~ paletteFn(histoField), label = makeLabels(dataSVO),
            weight = 2, opacity = 1, color = 'white', dashArray = '3', fillOpacity = 0.7,
            highlight = highlightOptions(weight = 4, color = '#666', dashArray = '', fillOpacity = 0.7, bringToFront = TRUE),
            labelOptions = labelOptions(style = list('font-weight' = 'normal', padding = '3px 8px'), textsize = '15px', direction = 'auto')
          )
      }
    )
  }
  
  observeEvent(c(input$dashboard_depiction, input$dashboardAggregateBy, input$dashboardFilterStates), withProgress(
    message = 'Rendering choropleth...', detail = input$dashboard_depiction, {
      addShadeToChoropleth(
        'dashBoardChoropleth', histoSVO[histoSVO$STUSPS10 %in% get_state_abb(input$dashboardFilterStates), ],
        paste(input$dashboard_depiction, input$dashboardAggregateBy, sep = '_')
      )
    }))
  
  output$dashBoardChoropleth <- renderLeaflet({isolate(
    withProgress(
      message='Rendering choropleth...', detail = 'DEFICIENCIES', {
        field <- 'DEFICIENCIES_BY_STATE'
        dataSVO <- histoSVO
        dataSVO$histoField <- dataSVO[[field]]
        paletteFn <- colorNumeric(palette = 'YlOrRd', domain = dataSVO$histoField, na.color = 'white')
        #View(dataSVO@data)
        base_choropleth %>% setView(-98, 40, 4) %>%
          addLegend(
            values = dataSVO$histoField, title = str_title_case(tolower(field)),
            pal = paletteFn, position = 'topright', opacity = 0.6
          ) %>% addPolygons(
            group = field, data = dataSVO, fillColor =  ~ paletteFn(histoField), label = makeLabels(dataSVO),
            weight = 2, opacity = 1, color = 'white', dashArray = '3', fillOpacity = 0.7,
            highlight = highlightOptions(weight = 4, color = '#666', dashArray = '', fillOpacity = 0.7, bringToFront = TRUE),
            labelOptions = labelOptions(style = list('font-weight' = 'normal', padding = '3px 8px'), textsize = '15px', direction = 'auto')
          )
      }
    )
  )})
  
  output$dashBoardDeficienciesMap <- renderUI({
    leafletOutput('dashBoardChoropleth', width = '100%', height = 640)
  })
  
  ################################################################################
  # K-MEANS RENDERER                                                             #
  ################################################################################
  observeEvent(
    input$plotKMeans,  withProgress(message = 'Clustering  dataset...', detail = input$prov_dataset, {
      result_df <- v$prov_df
      selectedData <- na.omit(result_df[, c(input$xcol, input$ycol)])
      clusters <- kmeans(selectedData, input$clusters)
      output$plotKM <- renderPlot({
        palette(c('#E41A1C', '#377EB8', '#4DAF4A', '#984EA3', '#FF7F00', '#FFFF33', '#A65628', '#F781BF', '#999999'))
        par(mar = c(5, 4, 0, 1))
        plot(selectedData, col = clusters$cluster, pch = 20, cex = 3)
        points(clusters$centers, pch = 4, cex = 4, lwd = 4)
      })
    })
  )
  
}