# Draft / Unused code snippets, for future use

###############################################################################################
# Summarize data for use in plots
traf_sum_nbdate <- traf1 %>%
  group_by(neighborhood_id, incident_date) %>%
  summarise(nTotal = n(),
            nFatal = sum(FATALITIES > 0,na.rm=T),
            nSBI = sum(SERIOUSLY_INJURED > 0,na.rm=T),
            nPedInv = sum(pedestrian_ind > 0,na.rm=T),
            nBicInv = sum(bicycle_ind > 0,na.rm=T),
            incident_yr = year(incident_date),
            incident_mo = month(incident_date,label=TRUE),
            .groups = "keep") %>%
  ungroup()

traf_sum_nbmY <- traf1 %>%
  group_by(neighborhood_id, incident_mY) %>%
  summarise(nTotal = n(),
            nFatal = sum(FATALITIES > 0,na.rm=T),
            nSBI = sum(SERIOUSLY_INJURED > 0,na.rm=T),
            nPedInv = sum(pedestrian_ind > 0,na.rm=T),
            nBicInv = sum(bicycle_ind > 0,na.rm=T),
            incident_yr = year(incident_date),
            incident_mo = month(incident_date,label=TRUE),
            .groups = "keep") %>%
  ungroup()

traf_sum_yrmo <- traf1 %>%
  group_by(incident_mo, incident_yr) %>%
  summarise(nTotal = n(),
            nFatal = sum(FATALITIES > 0,na.rm=T),
            nSBI = sum(SERIOUSLY_INJURED > 0,na.rm=T),
            nPedInv = sum(pedestrian_ind > 0,na.rm=T),
            nBicInv = sum(bicycle_ind > 0,na.rm=T),
            .groups = "keep") %>%
  ungroup()

traf_sum_date <- traf1 %>%
  group_by(incident_date) %>%
  summarise(nTotal = n(),
            nFatal = sum(FATALITIES > 0,na.rm=T),
            nSBI = sum(SERIOUSLY_INJURED > 0,na.rm=T),
            nPedInv = sum(pedestrian_ind > 0,na.rm=T),
            nBicInv = sum(bicycle_ind > 0,na.rm=T),
            .groups = "keep") %>%
  # filter(nTotal > 0, nFatal > 0) %>%
  mutate(oddsFatal = nFatal/(nTotal - nFatal),
         oddsSBI = nSBI/(nTotal - nSBI),
         loddsFatal = log(oddsFatal),
         loddsSBI = log(oddsSBI)) %>%
  ungroup()

traf_sum_dow <- traf1 %>%
  group_by(incident_dow,incident_yr) %>%
  summarise(nTotal = n(),
            nFatal = sum(FATALITIES > 0,na.rm=T),
            nSBI = sum(SERIOUSLY_INJURED > 0,na.rm=T),
            nPedInv = sum(pedestrian_ind > 0,na.rm=T),
            nBicInv = sum(bicycle_ind > 0,na.rm=T),
            .groups = "keep") %>%
  filter(nTotal > 0, nFatal > 0) %>%
  mutate(oddsFatal = nFatal/(nTotal - nFatal),
         oddsSBI = nSBI/(nTotal - nSBI),
         loddsFatal = log(oddsFatal),
         loddsSBI = log(oddsSBI)) %>%
  ungroup()


traf_sum_nb <- traf1 %>%
  group_by(neighborhood_id) %>%
  summarise(nTotal = n(),
            nFatal = sum(FATALITIES>0,na.rm=T))

traf_sum_map <- traf_sum_nbdate %>%
  group_by(neighborhood_id) %>%
  summarise(nTotal = sum(nTotal),
            nFatal = sum(nFatal))
traf_inc_map <- traf1 %>%
  filter(FATALITIES > 0) %>%
  mutate(FatalityNum = as.numeric(FATALITIES),
         PedestrianInvolved = case_when(
           pedestrian_ind > 0 ~ TRUE,
           pedestrian_ind == 0 | is.na(pedestrian_ind) ~ FALSE
         ))

# Join incident and geospatial data by neighborhood
traf_plotdata_dennb <- left_join(dennb,traf_plotdata, by=c("NBHD_NAME" = "neighborhood_id"))
st_geometry(traf_plotdata_dennb)


###############################################################################################
## DRAFT
## SHINY APP CODE

ui <- dashboardPage(
  skin="red",
  dashboardHeader(
    title = "Denver Traffic Incidents"
  ),
  dashboardSidebar(
    dateRangeInput(inputId = "in_daterange",
                   label = "Choose date range:",
                   start = "2019-01-01",
                   end = Sys.Date(),
                   format = "yyyy-mm-dd",
                   startview = "year"),
    pickerInput(inputId = "in_nbhds",
                label = "Select neighborhoods:",
                choices = nbhd_choices,
                multiple=TRUE,
                options = list(`actions-box` = TRUE),
                selected = nbhd_choices
    ),
    actionButton(inputId = "do", label="Update")
    # ,
    # menuItem("Table",tabName="inc_table"),
    # menuItem("Incidents by Neigbhorhood", tabName = "inc_nb"),
    # menuItem("Incidents by Date", tabName="inc_timeyr")
  ),
  dashboardBody(
    tabItems(
      tabItem("inc_table",
              box(tableOutput("data_table"),width=500))
      # ,
      # tabItem("inc_nb",
      #         h2("Incidents by Neighborhood: ",textOutput("in_daterange", inline=TRUE)),
      #         box(plotlyOutput("p_map"), width= 500),
      #         box(plotlyOutput("p_nb_barchart"),width=500),
      #         box(plotlyOutput("p_timeseries_nb_nTot"),width=500)
      # ),
      # tabItem("inc_timeyr",
      #         h2("Incidents by Date or Time of Year: ",textOutput("in_daterange", inline=TRUE)),
      #         box(plotlyOutput("p_timeseries_nTot"), width= 500),
      #         box(plotlyOutput("p_dow_nTot"), width= 500),
      #         box(plotlyOutput("p_moy_nTot"),width=500)
      #         
      # )
    )
  )
)

server <- function(input, output){
  ## Input: Data
  dataInput <- reactive({
    in_daterange <- input$in_daterange
    traf1 %>% 
      filter(incident_date >= in_daterange[1] & incident_date <= in_daterange[2])
  })
  
  
  ## Output: Data
  output$data_table <- renderTable({
    dataInput()
  })
  
  
  # ## Output: MAP
  # output$p_map <- renderPlotly({
  #   
  #   # get date range
  #   in_daterange <- reactive({input$in_daterange})
  #   #in_daterange <- c("2019-01-01","2022-03-01")
  #   
  #   # filter data on daterange
  #   traf2 <- traf1 %>% 
  #     filter(incident_date >= in_daterange[1] & incident_date <= in_daterange[2])
  #   # summarize data for map
  #   traf_sum_map <- traf2 %>%
  #     group_by(neighborhood_id) %>%
  #     summarise(nTotal = n(),
  #               nFatal = sum(FATALITIES > 0,na.rm=T)) %>%
  #     ungroup()
  #   traf_inc_map <- traf2 %>%
  #     filter(FATALITIES > 0) %>%
  #     mutate(FatalityNum = as.numeric(FATALITIES),
  #            PedestrianInvolved = case_when(
  #              pedestrian_ind > 0 ~ TRUE,
  #              pedestrian_ind == 0 | is.na(pedestrian_ind) ~ FALSE))
  #   # Join incident and geospatial data by neighborhood
  #   traf_plotdata_dennb <- left_join(dennb,traf_sum_map, by=c("NBHD_NAME" = "neighborhood_id"))
  #   # Plot Denver neighborhoods and roads
  #   p_map1 <- ggplot(data = traf_plotdata_dennb) +
  #     geom_sf(aes(fill = nTotal, label= NBHD_NAME)) +
  #     geom_sf(data = denrd, aes(label = FULLNAME), color="white", alpha=0.5) +
  #     geom_point(data = traf_inc_subset, aes(x=geo_lon,y=geo_lat, label=c(FatalityNum), color=PedestrianInvolved), alpha=0.80) +
  #     scale_color_manual(values = c("orange","red")) +
  #     theme_minimal() + 
  #     # coord_sf(ylim = c(39.6,39.92), xlim=c(-105.15,-104.6)) +
  #     labs(title=paste("Total Incidents by Neighborhood:",min(traf_sum_nbdate$incident_date),"through",max(traf_sum_nbdate$incident_date)),
  #          x = "", y="", fill="Total Number of Accidents",
  #          color="Pedestrian Involved")
  #   p_map1 <- ggplotly(p_map1) %>% config(scrollZoom = TRUE)
  #   p_map1
  # })
}


shinyApp(ui = ui, server = server)

###############################################################################################

# Load data
# Load traffic incidents data from Denver website
# traf <- read.csv("https://www.denvergov.org/media/gis/DataCatalog/traffic_accidents/csv/traffic_accidents.csv")

# for development/testing, faster to load from local copy
traf <- read.csv("data/traffic_accidents.csv")

# Prepare data for presentation as table
# columns to omit
drop_cols <- c("POINT_X","POINT_Y","shape","first_occurrence_date",
               "last_occurrence_date","reported_date","incident_id",
               "offense_id","offense_code","offense_code_extension",
               "HARMFUL_EVENT_SEQ_1","HARMFUL_EVENT_SEQ_2","HARMFUL_EVENT_SEQ_3",
               "TU1_TRAVEL_DIRECTION","TU1_DRIVER_ACTION","TU1_VEHICLE_MOVEMENT")
drop_colnums <- c(1:5,7:9,20:22,28:39,42:47)
factor_cols <- c("top_traffic_accident_offense","district_id","neighborhood_id","LIGHT_CONDITION")

table(as.factor(traf1$ROAD_CONDITION),traf1$SERIOUSLY_INJURED>0)

traf1 <- traf %>%
  filter(neighborhood_id != "",
         LIGHT_CONDITION != "",
         LIGHT_CONDITION != "UNDER INVESTIGATION",
         ROAD_CONDITION != "",
         ROAD_CONDITION != "UNDER INVESTIGATION") %>%
  mutate_if(is.character,str_trim) %>%
  mutate(incident_yr = year(first_occurrence_date),
         incident_mo = month(first_occurrence_date,label=TRUE),
         incident_mY = as.Date(first_occurrence_date,format="%M-%Y"),
         incident_date = date(first_occurrence_date),
         incident_datetime = as_datetime(first_occurrence_date),
         incident_hr = hour(incident_datetime),
         incident_dow = wday(incident_date,label=TRUE,week_start=1)) %>%
  mutate(across(factor_cols,as.factor)) %>%
  #filter(incident_yr > 2020) %>%
  select(-all_of(drop_colnums)) %>%
  mutate(road_cond = case_when(
    ROAD_CONDITION == "FOREIGN MATERIAL" ~ "FOREIGN MATERIAL",
    # ROAD_CONDITION == "UNDER INVESTIGATION" ~ "UNDER INVESTIGATION",
    # ROAD_CONDITION == "" ~ "",
    TRUE ~ word(ROAD_CONDITION,1)
  ),
  road_cond = as.factor(road_cond))

# Prepare geospatial data for Denver neighborhoods
# this was downloaded from Denver 
dennb <- read_sf(dsn="data/statistical_neighborhoods.gdb",layer="statistical_neighborhoods")
# Change geometries to work with plotly
dennb <- st_cast(dennb, to="MULTIPOLYGON")

# Prepare geospatial data for Denver roads
# Road data
# co_rds <- primary_secondary_roads(state="Colorado", year=2020)

# Street centerline data from Denver OpenData website
denrd <- read_sf(dsn="data/street_centerline.gdb",layer="street_centerline")
# Change geometries to work with plotly
denrd <- st_cast(denrd, to="MULTILINESTRING") %>%
  filter(VOLCLASS == "ARTERIAL" | VOLCLASS == "COLLECTOR")
# str(denrd)
# unique(denrd$TRAVLANES)
# table(denrd$TYPE)
# table(denrd$VOLCLASS)

###############################################################################################

# Summarizing Data
# the traffic incident data set is enormous
# so what do we want to look at?
# response values of interest:
# - number of incidents (of any kind)
# - number of fatalities, or number of incidents with at least one fatality
# - number of SBIs, or number of incidents with at least one SBI
# - number of incidents involving pedestrians (fatal, SBI, or otherwise)
# - number of incidents involving bicycles (fatal, SBI, or otherwise)

# predictors / covariates of interest:
# - time of year
# - light conditions
# - road conditions
# - location: neighborhood
# - location: near/on freeways or other roadways

# to do this, we will need to summarize the data in order to have response (counts)
traf_sum_nbdate <- traf1 %>%
  group_by(neighborhood_id, incident_date) %>%
  summarise(nTotal = n(),
            nFatal = sum(FATALITIES > 0,na.rm=T),
            nSBI = sum(SERIOUSLY_INJURED > 0,na.rm=T),
            nPedInv = sum(pedestrian_ind > 0,na.rm=T),
            nBicInv = sum(bicycle_ind > 0,na.rm=T),
            incident_yr = year(incident_date),
            incident_mo = month(incident_date,label=TRUE),
            .groups = "keep") %>%
  ungroup()

traf_sum_nbmY <- traf1 %>%
  group_by(neighborhood_id, incident_mY) %>%
  summarise(nTotal = n(),
            nFatal = sum(FATALITIES > 0,na.rm=T),
            nSBI = sum(SERIOUSLY_INJURED > 0,na.rm=T),
            nPedInv = sum(pedestrian_ind > 0,na.rm=T),
            nBicInv = sum(bicycle_ind > 0,na.rm=T),
            incident_yr = year(incident_date),
            incident_mo = month(incident_date,label=TRUE),
            .groups = "keep") %>%
  ungroup()

traf_sum_yrmo <- traf1 %>%
  group_by(incident_mo, incident_yr) %>%
  summarise(nTotal = n(),
            nFatal = sum(FATALITIES > 0,na.rm=T),
            nSBI = sum(SERIOUSLY_INJURED > 0,na.rm=T),
            nPedInv = sum(pedestrian_ind > 0,na.rm=T),
            nBicInv = sum(bicycle_ind > 0,na.rm=T),
            .groups = "keep") %>%
  ungroup()

traf_sum_date <- traf1 %>%
  group_by(incident_date) %>%
  summarise(nTotal = n(),
            nFatal = sum(FATALITIES > 0,na.rm=T),
            nSBI = sum(SERIOUSLY_INJURED > 0,na.rm=T),
            nPedInv = sum(pedestrian_ind > 0,na.rm=T),
            nBicInv = sum(bicycle_ind > 0,na.rm=T),
            .groups = "keep") %>%
  # filter(nTotal > 0, nFatal > 0) %>%
  mutate(oddsFatal = nFatal/(nTotal - nFatal),
         oddsSBI = nSBI/(nTotal - nSBI),
         loddsFatal = log(oddsFatal),
         loddsSBI = log(oddsSBI)) %>%
  ungroup()

traf_sum_dow <- traf1 %>%
  group_by(incident_dow,incident_yr) %>%
  summarise(nTotal = n(),
            nFatal = sum(FATALITIES > 0,na.rm=T),
            nSBI = sum(SERIOUSLY_INJURED > 0,na.rm=T),
            nPedInv = sum(pedestrian_ind > 0,na.rm=T),
            nBicInv = sum(bicycle_ind > 0,na.rm=T),
            .groups = "keep") %>%
  filter(nTotal > 0, nFatal > 0) %>%
  mutate(oddsFatal = nFatal/(nTotal - nFatal),
         oddsSBI = nSBI/(nTotal - nSBI),
         loddsFatal = log(oddsFatal),
         loddsSBI = log(oddsSBI)) %>%
  ungroup()


traf_sum_nb <- traf1 %>%
  group_by(neighborhood_id) %>%
  summarise(nTotal = n(),
            nFatal = sum(FATALITIES>0,na.rm=T))

###############################################################################################

# Plot bar chart of incidents by neighborhood
in_nbhds
in_daterange <- c("2014-01-01","2015-01-03")
traf_plotdata <- traf_sum_nb %>%
  filter(neighborhood_id %in% in_nbhds)

p_nb_barchart <- ggplot(data = traf_plotdata, aes(y = nTotal))+
  geom_col(aes(x = neighborhood_id, fill=neighborhood_id))+
  scale_fill_viridis(discrete = TRUE) +
  labs(x = "",y="Total Number of Incidents",fill="Neighborhood",
       title = paste("Total Incidents by Neighborhood:",min(in_daterange),"through",max(in_daterange)))
p_nb_barchart


# Plot time series
traf_plotdata <- traf_sum_date #[which(year(traf_sum_date$incident_date)=="2020"),]

# Total incidents
p_timeseries_nTot <- ggplot(data = traf_plotdata, aes(y=nTotal))+
  geom_path(aes(x = incident_date)) +
  labs(x = "Date",y = "Number of Incidents",
       title = paste("Total Number of Incidents by Date:",min(traf_plotdata$incident_date),"through",max(traf_plotdata$incident_date))) +
  theme_bw() %>%
  p_timeseries_nTot <- ggplotly(p_timeseries_nTot) %>% 
  layout(xaxis = list(rangeslider = list(visible=T)))
p_timeseries_nTot

# Fatal incidents
p_timeseries_nFat <- ggplot(data = traf_plotdata, aes(y=nFatal))+
  geom_path(aes(x = incident_date)) +
  labs(x = "Date",y = "Number of Fatal Incidents",
       title = paste("Fatal Incidents by Date:",min(traf_plotdata$incident_date),"through",max(traf_plotdata$incident_date))) +
  theme_bw()
p_timeseries_nFat <- ggplotly(p_timeseries_nFat) %>% 
  layout(xaxis = list(rangeslider = list(visible=T)))
p_timeseries_nFat


# Plot time series BY NEIGHBORHOOD
in_nbhds <- c("Athmar Park","Capitol Hill","West Colfax")

traf_plotdata <- traf_sum_nbdate %>%
  filter(neighborhood_id %in% in_nbhds)
# group_by(neighborhood_id,incident_yr) %>%
# summarise(nTotal = sum(nTotal),
#           nFatal = sum(nFatal)) %>%
# ungroup()
in_daterange <- c("2013-01-01","2014-10-01")

if(diff(date(in_daterange))>365){
  multiyear <- TRUE
  traf_plotdata <- traf_plotdata %>%
    mutate(date_group = incident_yr) %>%
    group_by(date_group,neighborhood_id) %>%
    summarise(nTotal = sum(nTotal),
              nFatal = sum(nFatal)) %>%
    ungroup()
}else{
  multiyear<- FALSE
  traf_plotdata <- traf_plotdata %>%
    mutate(Ym = as.Date(incident_date,format="%Y-%m"),
           date_group = Ym) %>%
    group_by(date_group,neighborhood_id) %>%
    summarise(nTotal = sum(nTotal),
              nFatal = sum(nFatal)) %>%
    ungroup()
}



# Total incidents BY NEIGHBORHOOD
p_timeseries_nTot_nb <- ggplot(data = traf_plotdata, aes(y=nTotal))+
  geom_path(aes(x = date_group, color=neighborhood_id)) +
  labs(x = "Date",y = "Number of Incidents",
       title = paste("Total Number of Incidents by Date and Neighborhood:",min(traf_plotdata$date_group),"through",max(traf_plotdata$date_group))) +
  theme_bw()+
  scale_x_continuous(breaks = unique(as.numeric(str_sub(traf_plotdata$date_group,1,4))))
p_timeseries_nTot_nb <- ggplotly(p_timeseries_nTot_nb) %>% 
  layout(xaxis = list(rangeslider = list(visible=T)))
p_timeseries_nTot_nb

# Fatal incidents BY NEIGHBORHOOD
p_timeseries_nFat_nb <- ggplot(data = traf_plotdata, aes(y=nFatal))+
  geom_path(aes(x = date_group, color=neighborhood_id)) +
  labs(x = "Date",y = "Number of Incidents",
       title = paste("Fatal Incidents by Date and Neighborhood:",min(traf_plotdata$date_group),"through",max(traf_plotdata$date_group))) +
  theme_bw()+
  scale_x_continuous(breaks = unique(as.numeric(str_sub(traf_plotdata$date_group,1,4))))
p_timeseries_nFat_nb <- ggplotly(p_timeseries_nFat_nb) %>% 
  layout(xaxis = list(rangeslider = list(visible=T)))
p_timeseries_nFat_nb


# Plot by day of week
traf_plotdata <- traf_sum_dow #[which(traf_sum_dow$incident_yr==2019),]
p_dow_nTot <- ggplot(data = traf_plotdata, aes(y = nTotal))
if(length(unique(traf_plotdata$incident_yr))>1){
  p_dow_nTot <- p_dow_nTot +
    geom_path(aes(x = incident_yr, color = as.factor(incident_dow)), size=1) +
    labs(x = "Year",y = "Number of Incidents", color="Day of Week",
         title = paste("Total Number of Incidents Per Year by Day of Week: ",
                       min(traf_plotdata$incident_yr),"-",max(traf_plotdata$incident_yr))) +
    scale_color_viridis(discrete = TRUE) +
    theme_bw()+
    scale_x_continuous(breaks = unique(traf_plotdata$incident_yr))
}else{
  p_dow_nTot <- p_dow_nTot +
    geom_path(aes(x = incident_dow, group=1), size=1) +
    labs(x = "Day of Week",y = "Number of Incidents",
         title = paste("Total Number of Incidents By Day of Week for",unique(traf_plotdata$incident_yr))) +
    # scale_x_discrete(labels = levels(traf_plotdata$incident_dow),
    #                  values = 0:6) +
    # #scale_x_continuous(labels = levels(traf_plotdata$incident_dow),values = 0:6) +
    theme_bw()
}
p_dow_nTot


# Plot by month of year
traf_plotdata <- traf_sum_yrmo #[which(traf_sum_yrmo$incident_yr==2019),]

p_moy_nTot <- ggplot(data = traf_plotdata, aes(y = nTotal))
if(length(unique(traf_plotdata$incident_yr))>1){
  p_moy_nTot <- p_moy_nTot +
    geom_path(aes(x = incident_yr, color = as.ordered(incident_mo)), size=1) +
    labs(x = "Year",y = "Number of Incidents", color="Month",
         title = paste("Total Number of Incidents Per Year by Month of Year: ",min(traf_plotdata$incident_yr),"-",max(traf_plotdata$incident_yr))) +
    #scale_color_viridis(discrete = TRUE) +
    theme_bw()+
    scale_x_continuous(breaks = unique(traf_plotdata$incident_yr))
}else{
  p_moy_nTot <- p_moy_nTot +
    geom_path(aes(x = incident_mo, group=1), size=1) +
    labs(x = "Month",y = "Number of Incidents",
         title = paste("Total Number of Incidents Per Month for",unique(traf_plotdata$incident_yr))) +
    #scale_color_viridis(discrete = TRUE) +
    theme_bw()
}

p_moy_nTot


###############################################################################################

# MAP
traf_plotdata <- traf_sum_nbdate %>%
  group_by(neighborhood_id) %>%
  summarise(nTotal = sum(nTotal),
            nFatal = sum(nFatal))
traf_inc_subset <- traf1 %>%
  filter(FATALITIES > 0) %>%
  mutate(FatalityNum = as.numeric(FATALITIES),
         PedestrianInvolved = case_when(
           pedestrian_ind > 0 ~ TRUE,
           pedestrian_ind == 0 | is.na(pedestrian_ind) ~ FALSE
         ))

# Join incident and geospatial data by neighborhood
traf_plotdata_dennb <- left_join(dennb,traf_plotdata, by=c("NBHD_NAME" = "neighborhood_id"))
st_geometry(traf_plotdata_dennb)

ggplot(data = traf_plotdata_dennb) + geom_sf()

# Plot Denver neighborhoods and roads
p_map1 <- ggplot(data = traf_plotdata_dennb) +
  geom_sf(aes(fill = nTotal, label= NBHD_NAME)) +
  geom_sf(data = denrd, aes(label = FULLNAME), color="white", alpha=0.5) +
  geom_point(data = traf_inc_subset, aes(x=geo_lon,y=geo_lat, label=c(FatalityNum), color=PedestrianInvolved), alpha=0.80) +
  scale_color_manual(values = c("orange","red")) +
  theme_minimal() + 
  # coord_sf(ylim = c(39.6,39.92), xlim=c(-105.15,-104.6)) +
  labs(title=paste("Total Incidents by Neighborhood:",min(traf_sum_nbdate$incident_date),"through",max(traf_sum_nbdate$incident_date)),
       x = "", y="", fill="Total Number of Accidents",
       color="Pedestrian Involved")
p_map1 <- ggplotly(p_map1) %>% config(scrollZoom = TRUE)
p_map1
