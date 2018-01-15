## ui.R

library(shiny)

################################################################################
# USER INTERFACE                                                               #
################################################################################
title <- 'CMS - NF'

header <- dashboardHeader(title = title, titleWidth = 256)

sidebar <- dashboardSidebar(
  width = 256,
  useShinyjs(),
  tags$head(tags$style(".rightAlign{float:right;}")),
  sidebarMenu(
    id = 'mainTabs',
    menuItem('Dashboard', tabName = 'dashboard', icon = icon('dashboard'), selected = TRUE),
    menuItem('Explore', tabName = 'explore', icon = icon('table')),
    menuItem('Administer', tabName = 'admin', icon = icon('coffee'))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = 'dashboard',
      fluidPage(
        box(
          width = '100%',
          status = 'primary',
          valueBoxOutput('numFacilities'),
          withSpinner(valueBoxOutput('numResidents')),
          valueBoxOutput('numImmediateJeopardy'), hr(),
          box(
            width = 12,
            fluidRow(
              column(
                width = 4,
                selectizeInput(
                  'dashboard_depiction', label = h4('Depict'), 
                  choices = c('Deficiencies' = 'DEFICIENCIES', 'Substandard Deficiencies' = 'SUBSTANDARD_DEFICIENCIES'),
                  selected = 'DEFICIENCIES')
              ),
              column(
                width = 4,
                selectizeInput(
                  'dashboardAggregateBy', label = h4('Aggregated'),
                  choices = c('By State' = 'BY_STATE', 'By State Per Resident' = 'BY_STATE_PER_RESIDENT', 'By State Per Facility' = 'BY_STATE_PER_FACILITY'))
              )
            ),
            box(
              width = 12, solidHeader = TRUE, status='warning', title = 'Filters', collapsible = TRUE, collapsed = TRUE,
              fluidRow(
                column(width = 4, sliderInput(
                  'dashboardTimeRange', h4('Time Range'), min = 2013, max = 2018, sep = '', value = c(2013, 2017)
                )),
                column(width = 4, pickerInput(
                  'dashboardFilterStates', label = h4('States / Territories'), multiple = TRUE, options = list('actions-box' = TRUE),
                  choices = sort(c(state.name, 'Puerto Rico', 'Guam')), selected = c(state.name, 'Puerto Rico', 'Guam')
                )),
                column(width = 4, pickerInput(
                  'dashboardFilterProviders', label = h4('Provider'), multiple = TRUE, options = list('actions-box' = TRUE),
                  choices = provider.filters, selected = provider.filters
                ))
              )
            ),
            box(
              width=12, solidHeader = TRUE, status='warning', title = 'Choropleth', collapsible = TRUE,
              withSpinner(uiOutput('dashBoardDeficienciesMap'))
            ),
            box(
              width=12, solidHeader = TRUE, status='warning', title = 'Charts', collapsible = TRUE,
              fluidRow(
                column(width = 4, shiny::img(src = 'images/scope-and-severity-grid.jpg', height = 400)),
                column(
                  width = 8,
                  tabsetPanel(
                    tabPanel(
                      'US Deficiencies', icon = icon('pie-chart'),
                      wellPanel(fluidPage(
                        box(width = 12, solidHeader = TRUE, withSpinner(plotlyOutput('dashBoardDeficienciesPie')))
                      ))
                    ),
                    tabPanel(
                      'Deficiencies By State', icon = icon('bar-chart'),
                      wellPanel(fluidPage(
                        box(width = 12, solidHeader = TRUE, withSpinner(plotlyOutput('dashBoardDeficienciesByStateBar')))
                      ))
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    tabItem(
      tabName = 'explore',
      fluidPage(fluidRow(
        column(width = 10, pickerInput(
          'states_of_interest', label = h4('States / Territories Of Interest'), multiple = TRUE, options = list('actions-box' = TRUE),
          choices = sort(c(state.name, 'Puerto Rico', 'Guam')), selected = c(state.name, 'Puerto Rico', 'Guam')
        )),
        column(2, downloadButton('downloadMetaData', label = 'Download Dataset Metadata'))
      ),
      tabsetPanel(
        tabPanel(
          'Provider Data Sets', icon = icon('building'),
          wellPanel(
            fluidPage(
              box(
                width = '100%',
                fluidRow(
                  column(
                    width = 12, solidHeader = TRUE,
                    fluidRow(
                      column(6, selectizeInput('prov_dataset', label = h4('Provider Data Set'), choices = NULL)),
                      column(
                        width = 6,
                        conditionalPanel(
                          condition = "(output.hasProvData)", br(), br(),
                          downloadButton('downloadProviderData', label = 'Download This Data Set'),
                          actionButton('providerDetails', label = 'Provider Details', icon = icon('eye'))
                        )
                      )
                    ),
                    withSpinner(DT::dataTableOutput('provDataTable')),
                    conditionalPanel(
                      condition = "(output.hasProvData)", br(),
                      tabsetPanel(
                        tabPanel(
                          'Clustering', icon = icon('th-large', lib = 'glyphicon'),
                          fluidRow(
                            column(
                              width = 2, br(), wellPanel(
                                selectizeInput('xcol', label = h4('X Variable'), choices = NULL),
                                selectizeInput('ycol', label = h4('Y Variable'), choices = NULL),
                                numericInput('clusters', label = h4('Cluster count'), 3, min = 1, max = 9),
                                actionButton('plotKMeans', label = 'Compute Clusters', icon = icon('calculator')))),
                            column(10, br(), br(), plotOutput('plotKM'))
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        ),
        tabPanel(
          'State Averages', icon = icon('tasks'),
          wellPanel(
            fluidPage(
              box(
                width = 12, solidHeader = TRUE,
                withSpinner(DT::dataTableOutput('stateAveragesDataTable')), hr(),
                downloadButton('downloadStateAverages',  label = 'Download State Averages')
              )
            )
          )
        )
      ))
    ),
    tabItem(
      tabName = 'admin',
      wellPanel(
        fluidPage(
          box(
            width = '100%', solidHeader = TRUE, status = 'warning',
            shiny::img(src = 'images/medicare_data.png'), br(),
            helpText('Refresh the medicare data with the latest version from data.medicare.gov.'),
            actionButton('refreshData', label = 'Refresh Medicare Data', icon = icon('refresh')),
            # textOutput('refreshResponse')
            hr(),
            box(
              width = '100%', solidHeader = TRUE, status = 'success', collapsible = TRUE, title = 'Data Files',
              DT::dataTableOutput('dataFilesTable')
            )
          ),
          box(
            width = '100%', solidHeader = TRUE, status = 'danger',
            h4(sprintf('Lines of code: %g', app_cloc()))
          )
        )
      )
    )
  )
)

ui <- function(request) {
  dashboardPage(header, sidebar, body, title = title, skin = 'purple')
}
