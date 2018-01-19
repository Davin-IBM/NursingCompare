## global.R

# Detect and install missing packages before loading them
if (!require('pacman')) install.packages('pacman')
pacman::p_load(
  'shiny', 'shinyjs', 'shinydashboard', 'shinyWidgets', 'shinycssloaders', 'leaflet',
  'rgdal', 'plotly', 'RColorBrewer', 'dplyr', 'lettercase'
)

# Bring in configuration
source('./config.R')

choropleth.map.providers <- providers[names(providers) %in% c('Stamen.TonerLite', 'CartoDB.Positron', 'OpenStreetMap', 'Esri.WorldTopoMap', 'Esri.WorldGrayCanvas')]
provider.filters <- c('In Hospital' = 'IN_HOSP', 'Not In Hospital' = 'NOT_IN_HOSP', 'For-Profit' = 'FOR_PROFIT', 'Non-Profit' = 'NON_PROFIT', 'Medicare' = 'MEDICARE', 'Medicaid' = 'MEDICAID', 'Medicare and Medicaid' = 'MEDICARE_AND_MEDICAID')
choropleth.shade <- c('Deficiencies', 'Substandard Deficiencies', 'Penalties')

# Deficiencies by level
deficiencies.level_1 <- c('A', 'B', 'C')
deficiencies.level_2 <- c('D', 'E', 'F')
deficiencies.level_3 <- c('G', 'H', 'I')
deficiencies.level_4 <- c('J', 'K', 'L')
deficiencies.all <- c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L')
deficiencies.substandard <- append(c('F', 'H', 'I'), deficiencies.level_4)

# Global data
us_states_geo <- NULL
CACHE_ <- list()

provnum_datasets <- sort(c(
  'Deficiencies' = 'DEFICIENCIES', 'Ownership' = 'OWNERSHIP', 'Penalties' = 'PENALTIES', 'Provider Information' = 'PROVIDER_INFO',
  'Quality Measure Claims' = 'QUAL_CLAIMS', 'Quality Measure MDS' = 'QUAL_MDS', 'Survey Summary' = 'SURVEY_SUMMARY',
  'SNF PUF Provider Information' = 'SNF_PUF_PROVIDER_INFO'
))

all_datasets <- sort(c(provnum_datasets, c('State Averages' = 'STATE_AVG')))

# R can read gzipped compressed CSV files, so lets do that to save space (about 10:1)
compress_csv <- function(path) {
  for (f in list.files(path, pattern = '*_Download.csv$')) {
    print(paste0('compressing: ', f))
    unlink(paste0(path, f, '.gz'))
    system(paste0('gzip ', path, f))
  }
}

refresh_data <- function() {
  unlink(app_tmp, recursive = TRUE)
  dir.create(app_tmp, recursive = TRUE)
  # Nursing Home compare data
  curl::curl_download(
    'https://data.medicare.gov/views/bg9k-emty/files/d3d503f4-776e-436d-a57f-caab74eeb59f?content_type=application%2Fzip%3B%20charset%3Dbinary&filename=NursingHome_Revised_Flatfiles.zip',
    paste0(app_tmp, 'temp.zip'))
  if(!file.exists(paste0(app_data, 'us-states.json'))) {
    # US States GeoJSON data
    curl::curl_download(
      'https://github.com/jgoodall/us-maps/raw/master/geojson/state.geo.json',
      paste0(app_data, 'us-states.json'))
  }
  utils::unzip(paste0(app_tmp, 'temp.zip'), exdir = app_data)
  compress_csv(app_data)
  unlink(app_tmp, recursive = TRUE)
  # Expire the cache
  CACHE_ <- list()
}

get_datafiles_info <- function() {
  file.info(list.files(app_data, pattern="*.gz", full.names = TRUE))
}

get_filename <- function(data_set) {
  #print(paste0('get_filename: ', data_set))
  if (data_set == 'DEFICIENCIES') return(paste0(app_data, 'Deficiencies_Download.csv.gz'))
  if (data_set == 'OWNERSHIP') return(paste0(app_data, 'Ownership_Download.csv.gz'))
  if (data_set == 'PENALTIES') return(paste0(app_data, 'Penalties_Download.csv.gz'))
  if (data_set == 'PROVIDER_INFO') return(paste0(app_data, 'ProviderInfo_Download.csv.gz'))
  if (data_set == 'QUAL_CLAIMS') return(paste0(app_data, 'QualityMsrClaims_Download.csv.gz'))
  if (data_set == 'QUAL_MDS') return(paste0(app_data, 'QualityMsrMDS_Download.csv.gz'))
  if (data_set == 'STATE_AVG') return(paste0(app_data, 'StateAverages_Download.csv.gz'))
  if (data_set == 'SURVEY_SUMMARY') return(paste0(app_data, 'SurveySummary_Download.csv.gz'))
  if (data_set == 'SNF_PUF_PROVIDER_INFO') return(paste0(app_data, 'SNF_PUF_Provider.csv.gz'))
}

get_dataset <- function(data_set) {
  #print(paste0('get_dataset: ', data_set))
  if (data_set == 'US_STATES_GEO') {
    if (is.null(us_states_geo)) {
      us_states_geo <<- rgdal::readOGR(paste0(app_data, 'us-states.json'), 'OGRGeoJSON')
    }
    return(us_states_geo)
  }
  if (data_set == 'DEFICIENCIES') {
    if (is.null(CACHE_$DEFICIENCIES)) {
      df <- read.csv(file = get_filename(data_set))
      names(df) <- toupper(names(df))
      CACHE_$DEFICIENCIES <<- transform(df[grep('^\\d+$', df$PROVNUM),,drop=FALSE], PROVNUM = as.numeric(as.character(PROVNUM)))
    }
    return(CACHE_$DEFICIENCIES)
  }
  if (data_set == 'OWNERSHIP') {
    if (is.null(CACHE_$OWNERSHIP)) {
      df <- read.csv(file = get_filename(data_set))
      names(df) <- toupper(names(df))
      CACHE_$OWNERSHIP <<- transform(df[grep('^\\d+$', df$PROVNUM),,drop=FALSE], PROVNUM = as.numeric(as.character(PROVNUM)))
    }
    return(CACHE_$OWNERSHIP)
  }
  if (data_set == 'PENALTIES') {
    if (is.null(CACHE_$PENALTIES)) {
      df <- read.csv(file = get_filename(data_set))
      names(df) <- toupper(names(df))
      CACHE_$PENALTIES <<- transform(df[grep('^\\d+$', df$PROVNUM),,drop=FALSE], PROVNUM = as.numeric(as.character(PROVNUM)))
    }
    return(CACHE_$PENALTIES)
  }
  if (data_set == 'PROVIDER_INFO') {
    if (is.null(CACHE_$PROVIDER_INFO)) {
      df <- read.csv(file = get_filename(data_set))
      names(df) <- toupper(names(df))
      CACHE_$PROVIDER_INFO <<- transform(df[grep('^\\d+$', df$PROVNUM),,drop=FALSE], PROVNUM = as.numeric(as.character(PROVNUM)))
    }
    return(CACHE_$PROVIDER_INFO)
  }
  if (data_set == 'QUAL_CLAIMS') {
    if (is.null(CACHE_$QUAL_CLAIMS)) {
      df <- read.csv(file = get_filename(data_set))
      names(df) <- toupper(names(df))
      CACHE_$QUAL_CLAIMS <<- transform(df[grep('^\\d+$', df$PROVNUM),,drop=FALSE], PROVNUM = as.numeric(as.character(PROVNUM)))
    }
    return(CACHE_$QUAL_CLAIMS)
  }
  if (data_set == 'QUAL_MDS') {
    if (is.null(CACHE_$QUAL_MDS)) {
      df <- read.csv(file = get_filename(data_set))
      names(df) <- toupper(names(df))
      CACHE_$QUAL_MDS <<- transform(df[grep('^\\d+$', df$PROVNUM),,drop=FALSE], PROVNUM = as.numeric(as.character(PROVNUM)))
    }
    return(CACHE_$QUAL_MDS)
  }
  if (data_set == 'SURVEY_SUMMARY') {
    if (is.null(CACHE_$SURVEY_SUMMARY)) {
      df <- read.csv(file = get_filename(data_set))
      names(df) <- toupper(names(df))
      CACHE_$SURVEY_SUMMARY <<- transform(df[grep('^\\d+$', df$PROVNUM),,drop=FALSE], PROVNUM = as.numeric(as.character(PROVNUM)))
    }
    return(CACHE_$SURVEY_SUMMARY)
  }
  if (data_set == 'STATE_AVG') {
    if (is.null(CACHE_$STATE_AVG)) {
      df <- read.csv(file = get_filename(data_set))
      names(df) <- toupper(names(df))
      CACHE_$STATE_AVG <<- df
    }
    return(CACHE_$STATE_AVG)
  }
  if (data_set == 'SNF_PUF_PROVIDER_INFO') {
    if (is.null(CACHE_$SNF_PUF_PROVIDER_INFO)) {
      df <- read.csv(file = get_filename(data_set))
      names(df) <- toupper(names(df))
      rename(df, PROVNUM = PROVIDER.ID)
      CACHE_$SNF_PUF_PROVIDER_INFO <<- transform(df[grep('^\\d+$', df$PROVNUM),,drop=FALSE], PROVNUM = as.numeric(as.character(PROVNUM)))
    }
    return(CACHE_$SNF_PUF_PROVIDER_INFO)
  }
  if (data_set == 'RESIDENTS_BY_STATE') {
    if (is.null(CACHE_$RESIDENTS_BY_STATE)) {
      # derive this data set
      CACHE_$RESIDENTS_BY_STATE <<- merge(
        get_dataset('PROVIDER_INFO') %>% group_by(STATE) %>% summarise_at(c('RESTOT', 'BEDCERT'), sum),
        get_dataset('PROVIDER_INFO') %>% count(STATE),
        by = 'STATE', all.x = TRUE
      )  %>% rename(FACILITIES_BY_STATE = n, RESIDENTS_BY_STATE = RESTOT, BEDCERT_BY_STATE = BEDCERT)
    }
    return(CACHE_$RESIDENTS_BY_STATE)
  }
  if (data_set == 'DASHBOARD_DEFICIENCIES') {
    if (is.null(CACHE_$nf.dashboard_deficiencies)) {
      CACHE_$DASHBOARD_DEFICIENCIES <<- transform(
        merge(
          merge(
            get_dataset('RESIDENTS_BY_STATE'),
            filter(get_dataset('DEFICIENCIES'), SCOPE %in% deficiencies.all) %>% count(STATE) %>% rename(DEFICIENCIES_BY_STATE = n),
            by = 'STATE', all.x = TRUE
          ),
          filter(get_dataset('DEFICIENCIES'), SCOPE %in% deficiencies.substandard) %>% count(STATE) %>% rename(SUBSTANDARD_DEFICIENCIES_BY_STATE = n),
          by = 'STATE', all.x = TRUE
        ),
        RESIDENTS_BY_STATE_PER_FACILITY = RESIDENTS_BY_STATE / FACILITIES_BY_STATE,
        DEFICIENCIES_BY_STATE_PER_FACILITY = DEFICIENCIES_BY_STATE / FACILITIES_BY_STATE,
        DEFICIENCIES_BY_STATE_PER_RESIDENT = DEFICIENCIES_BY_STATE / RESIDENTS_BY_STATE,
        SUBSTANDARD_DEFICIENCIES_BY_STATE_PER_FACILITY = SUBSTANDARD_DEFICIENCIES_BY_STATE / FACILITIES_BY_STATE,
        SUBSTANDARD_DEFICIENCIES_BY_STATE_PER_RESIDENT = SUBSTANDARD_DEFICIENCIES_BY_STATE / RESIDENTS_BY_STATE,
        OCCUPANCY_BY_STATE = 100 * RESIDENTS_BY_STATE / BEDCERT_BY_STATE
      )
    }
    return(CACHE_$DASHBOARD_DEFICIENCIES)
  }
  NULL
}

get_state_abb <- function(state_names) {
  result <- state.abb[state.name %in% state_names]
  if ('Puerto Rico' %in% state_names) result <- c(result, 'PR')
  if ('Guam' %in% state_names) result <- c(result, 'GU')
  result
}

five_star <- function(num_stars) {
  if_else(
    is.na(num_stars),
    '[MISSING]',
    paste0(strrep('\U2605', num_stars), strrep('\U2606', 5 - num_stars))
  )
}

app_cloc <- function() {
  R.utils::countLines(paste0(app_home, 'ui.R')) + 
    R.utils::countLines(paste0(app_home, 'server.R')) + 
    R.utils::countLines(paste0(app_home, 'global.R'))
}

histoSVO <- {
  sp::merge(get_dataset('US_STATES_GEO'), get_dataset('DASHBOARD_DEFICIENCIES'), by.x = 'STUSPS10', by.y = 'STATE') %>% na.omit()
}

histoSVO_by_state <- function(states_to_remove = c()) {
  if_else ((!is.null(states_to_remove) && length(states_to_remove)), filter(histSVO, !(STUSPS10 %in% states_to_remove)), histoSVO)
}

base_choropleth <- {
  m <- leaflet() %>%
    addLayersControl(baseGroups = names(choropleth.map.providers), position = 'topleft') %>%
    addMiniMap(tiles = choropleth.map.providers[[1]], toggleDisplay = TRUE, position = 'bottomleft') %>%
    htmlwidgets::onRender("
                          function(el, x) {
                          var myMap = this;
                          myMap.on('baselayerchange', function (e) { myMap.minimap.changeLayer(L.tileLayer.provider(e.name));})
                          }")
  for (provider in choropleth.map.providers) {
    m <- addProviderTiles(m, provider, group = provider, options = providerTileOptions(minZoom = 2))
  }
  m
}
