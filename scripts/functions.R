get_runs <- function (file_coord, ctrl) {
  
  location <- read_excel(file_coord)
  location<-convert_Belgian_decimal(coord=location)
  x_crd <- location$x
  y_crd <- location$y
  
  #Simulation period
  START <- location$start
  END <- location$end
  
  #Input layers from control file
  soil_layer <- get_record(file = ctrl, item = "SOIL")
  gwl_layer <- get_record(file = ctrl, item = "AVGGWL")
  ghg_layer <- get_record(file = ctrl, item = "GHG")
  glg_layer <- get_record(file = ctrl, item = "GLG")
  meteo_layer <- get_record(file = ctrl, item = "METEO")
  
  db <- data.frame(x_crd, y_crd)%>%
    mutate(
      soil_id = get_data_asc(file = soil_layer, x_crd = x_crd, y_crd = y_crd),
      crop_id = location$crop_id,    
      irrigation_id = 0,
      meteo_id = get_data_asc(file = meteo_layer, x_crd = x_crd, y_crd = y_crd),
      GWLEVEL = get_data_asc(file = gwl_layer, x_crd = x_crd, y_crd = y_crd),
      GHG = get_data_asc(file = ghg_layer, x_crd = x_crd, y_crd = y_crd),
      GLG = get_data_asc(file = glg_layer, x_crd = x_crd, y_crd = y_crd),
      scenario_id = "direct",
      run_id = 1:nrow(.),
      gw_id = run_id
    )
  
  try(if(db$soil_id == -9999 |db$meteo_id ==-9999 | db$GWLEVEL == -9999) stop("No data available for this coordinate"))
  return (db)
}

#Calculate GWL time series
get_groundwater <- function (database, START, END) {
  
  gwl_criteria <- database%>%
    mutate(GWL = if_else(GWLEVEL>3 | GHG < (-500) | GLG < (-500)| GHG < GLG, GWLEVEL, -1))
  # Average groundwater levels are assumed when:
  # GWLEVEL>3 : groundwater levels deeper than 3 m  
  # GHG < (-500) | GLG < (-500): in locations where there is no data (-99999) or when GHG or GLG are deeper 
  # than 500 cm (soil profile is 600 cm)
  # GHG < GLG: locations where there are errors in the maps and GHG is deeper than GLG
  
  #First criteria: average groundwater levels transformed to pressure head (SWBOTB = 5)
  gwl_avg <- gwl_criteria%>%
    filter(GWL != -1)%>%
    mutate(HBOT5 = if_else(GWL<=5,((6-GWL)*100),100.0)) # soil profile has 6 m, max gwl allowed = 5m
  
  start <- gwl_avg %>%
    mutate(
      DATE5 = as.Date(START),
      scenario_id = "direct"
    )%>%
    select(scenario_id, gw_id, DATE5, HBOT5)
  
  end <- gwl_avg %>%
    mutate(
      DATE5 = as.Date(END),
      scenario_id = "direct"
    )%>%
    select(scenario_id, gw_id, DATE5, HBOT5)
  
  GWL_avg <- rbind(start, end) 
  
  #Second criteria: Groundwater level fluctuations approximated with a sinus function and GHG and GLG (SWBOTB = 5)
  
  gwl_sinus<-gwl_criteria %>%
    filter(GWL == -1)%>%
    mutate (Amp = abs(GLG-GHG)/2)%>%
    select (run_id, gw_id, GHG, GLG, Amp)
  
  run_id <- sort (gwl_sinus$run_id)
  days<-seq(0,365,30)
  years <- seq (year(START), year(END),1)
  GWL_sinus<-NULL
  
  for (s_run_id in run_id){
    #message("Run", s_run_id,"...")
    GHG <-gwl_sinus%>%
      filter (run_id == s_run_id)%>%
      select (GHG)%>%
      deframe()
    
    Amp <-gwl_sinus%>%
      filter (run_id == s_run_id)%>%
      select (Amp)%>%
      deframe()
    
    GWLEVEL <-GHG-Amp+sin((days+80)*pi/180)*Amp
    HBOT5 <-600+GWLEVEL # GWLEVEL is a negative value
    GWL_fluc<-NULL
    
    for(year in years){
      GWL_fluc_tmp <- data.frame(s_run_id, days,HBOT5)%>%
        mutate(DATE5 = days+as.Date(str_c(year,"-01-01")))%>%
        rename(run_id = s_run_id)%>%
        select(run_id, days,DATE5, HBOT5)
      
      GWL_fluc <- rbind(GWL_fluc, GWL_fluc_tmp )
      
      GWL_tmp <- left_join( x = GWL_fluc, y = gwl_sinus, by = c("run_id"))%>%
        mutate(scenario_id = "direct")%>%
        select(scenario_id, gw_id, DATE5, HBOT5)
      
    }
    
    GWL_sinus <- rbind(GWL_sinus, GWL_tmp)
    
  }
  
  GWL <- rbind(GWL_avg,GWL_sinus)%>%
    arrange(gw_id)
  
  write_csv (x=GWL, file= "./data/grondwater.csv")
  
  return (GWL)
}

#Create the main dataframe of Runs 

get_main_database <- function(database) {
  db <- database %>% 
    select(run_id, x_crd, y_crd, soil_id, crop_id, irrigation_id, meteo_id, gw_id, scenario_id)
  
  # Create unique combinations (indirect)
  
  if (indirect) {
    
    max_id <- nrow(db)
    db_ind <- db %>%
      select(crop_id, soil_id, meteo_id) %>%
      unique()
    db_ind <- db_ind %>%
      mutate(
        run_id = max_id + 1:nrow(db_ind),
        x_crd = NA_real_,
        y_crd = NA_real_,
        irrigation_id = 0,
        scenario_id = "indirect",
        gw_id = NA_real_
      )
    
    # combine and update run_id 
    db <- rbind(db, db_ind)%>%   
      mutate(
        run_id = 1:nrow(.)
      )
    
  }
  
  return (db)
}

#Create the sqlite database
create_sqlite <- function(list_data, database, START, END) {
  
  suppressWarnings(dir.create("./model/sqlite_database"))
  
  file_sql <-"./model/sqlite_database/input_data.sqlite"
  suppressWarnings(if (file_exists(file_sql)) {
    file_delete(file_sql)
  } else {
    message ("\nCreating an input database... \n")
  })
  
  #open and writing the sqlite file
  
  conn <- dbConnect(RSQLite::SQLite(), dbname = file_sql) 
  dbWriteTable(conn = conn, name = "Runs", value = database)
  
  for (name in list_data){
    file_name <- str_c("./data/", name, ".csv")
    data <- read_csv(file = file_name, show_col_types = FALSE, progress = FALSE )
    
    if (name == "meteo") {
      data<-data%>%
        mutate(
          TSTART = START,
          TEND = END
        )
      
    }else if (name == "gewaskalender") {
      data<-data%>%
        filter(CROPSTART>=START & CROPEND <= END)
    }
    
    # upload static data in the sqlite file
    
    dbWriteTable(conn = conn, name = name, value = data) 
  }
  
  # close database
  dbDisconnect(conn = conn)
  
}

#Get the run ids of the direct runs

get_runs_direct<-function(file_sql){
  conn <- dbConnect(RSQLite::SQLite(), dbname = file_sql)
  name<-dbListTables(conn)
  db<- as_tibble(dbReadTable(conn = conn, name = "Runs"))%>%
    filter(scenario_id=="direct")
  
  ids<-db$run_id
  
  dbDisconnect(conn = conn)
  
  return(ids)
}


get_model_results <- function (dir_run, file_sql, run_id) {
  
  suppressWarnings(dir.create("./output"))
  folder<-"./output"
  
  data<-NULL
  for (s_run_id in run_id) {
    
    path <- str_c(dir_run, "/run_", formatC(x = s_run_id, format = "d", width = 9, flag = "0"))
    new_path <- str_c(folder, "/run_", formatC(x = s_run_id, format = "d", width = 9, flag = "0"))
    
    suppressWarnings(if (file.exists(new_path)) {
      message("\nYour previous run will be overwritten\n")
      file_delete(new_path)
    })
    
    #Read data
    file_model <- str_c(path,"/result_output.csv")
    
    if (file.exists(file_model)) {
      crop_id = get_run_info_SQL(file_sql, s_run_id)$crop_id
      if (crop_id<=5) {
        db_tmp <- read_csv_SWAP(file = file_model, variable = c("PMOWDM","MOWDM","TREDDRY",	"TREDWET", "GWL"))%>%
          dplyr::rename(
            PDM = PMOWDM,
            DM = MOWDM
          )%>%
          mutate(
            run_id=s_run_id, 
            crop_id=crop_id,
          )
      } else if (crop_id==6) {
        db_tmp <- read_csv_SWAP(file = file_model, variable = c("CPWDM","CWDM", "TREDDRY",	"TREDWET", "GWL"))%>%
          dplyr::rename(
            PDM = CPWDM,
            DM = CWDM,
          )%>%
          mutate(
            run_id=s_run_id, 
            crop_id=crop_id,
          )
      } else if (crop_id>6){
        db_tmp <- read_csv_SWAP(file = file_model, variable = c("CPWSO","CWSO","TREDDRY",	"TREDWET", "GWL"))%>%
          dplyr::rename(
            PDM = CPWSO,
            DM = CWSO
          )%>%
          mutate(
            run_id=s_run_id, 
            crop_id=crop_id
           
          )
      }
      
    }
    
    db_tmp <- db_tmp %>%
      mutate(
        x_crd = get_run_info_SQL(file_sql, s_run_id)$x_crd,
        y_crd = get_run_info_SQL(file_sql, s_run_id)$y_crd
      )
    
    data <- rbind(data, db_tmp)
    
    #Send results folder to output folder
    file_move(path = path, new_path = new_path)
    
    #Erase unnecessary files
    pattern<-"*.wwl"
    filetoremove<-list.files(new_path, recursive = TRUE, pattern = pattern)
    file.remove(suppressWarnings(file.path(new_path, filetoremove)))
  }
  #Delete indirect folders
  unlink(dir_run, recursive = TRUE)
  
  return (data)
  
}



get_yield_avg <- function(data_procc) {
  #Yield data
  yield_data<-data_procc%>%
    mutate(
      Y_pot = hrvpotbio,
      Y_act = (Y_pot*(100-dmgtot)/100), #Actual yield considering direct and indirect effects
      year=as.integer(year)
    )
  
  #Average whole area
  yield_data_avg <-yield_data%>%
    group_by(year)%>%
    mutate(
      Y_pot_avg = mean(Y_pot),#Potential yield 
      Y_act_avg=mean(Y_act), #Actual yield considering direct and indirect effects
      dmgtot_avg= mean(dmgtot),
      dmgind_avg= mean(dmgind),
      dmgwet_avg= mean(dmgwet),
      dmgdry_avg= mean(dmgdry),
      ghg_avg= mean(ghg),
      glg_avg= mean(glg)
    )%>%
    select(year, Y_pot_avg, Y_act_avg, dmgtot_avg, dmgind_avg, dmgwet_avg, dmgdry_avg, ghg_avg, glg_avg)%>%
    unique()
  
  
  return(yield_data_avg)
}



#Average precipitation deficit based on meteo file
prec_deficit<- function(file_sql, dir_met, start, end, discr) {
  # get grid no
  conn <- dbConnect(RSQLite::SQLite(), dbname = file_sql)
  name<-dbListTables(conn)
  db1<- as_tibble(dbReadTable(conn = conn, name = "Runs"))%>%
    filter(scenario_id=="direct")
  
  dbDisconnect(conn = conn)
  
  grid_no <-unique(db1$meteo_id)
  
  pattern <-"*.met"
  folders<-list.files(dir_met, recursive = TRUE, pattern = pattern)
  paths<-file.path(dir_met, folders)
  
  #Read meteo data 
  
  db <- read_csv(file = paths,  show_col_types = FALSE, progress = FALSE)%>%
    filter(YYYY>=year(start), YYYY<=year(end))%>%
    mutate(Station=substr(Station, 2,7))%>%
    filter(Station %in% grid_no)
  
  
  #Calculate precipitation deficit
  
  deficit <- db%>%
    mutate(P_ET = RAIN - ETref) %>%
    select(Station, DD, MM, YYYY, P_ET)
  
  if (discr=="monthly"){
    monthly_def <-deficit%>%
      group_by(Station, MM, YYYY)%>%
      mutate( P_ET_monthly= sum(P_ET)) 
    
    #Select only unique/distinct rows from the data frame
    def <- distinct(monthly_def, P_ET_monthly, .keep_all = TRUE)%>%
      rename(
        meteo_id=Station,
        year=YYYY
      )%>%
      select(meteo_id, MM, year, P_ET_monthly)
    
  } else if (discr=="yearly"){
    yearly_def <-deficit%>%
      group_by(Station, YYYY)%>%
      mutate( P_ET_yearly = sum(P_ET)) 
    
    #Select only unique/distinct rows from the data frame
    def <- distinct(yearly_def, P_ET_yearly, .keep_all = TRUE)%>%
      rename(
        meteo_id=Station,
        year=YYYY
      )%>%
      select(meteo_id, year, P_ET_yearly)
  }
  return (def)
} 

convert_Belgian_decimal <- function(coord){
  
  #Input parameters
  #coordinates in decimals, lat, lon
  
  #Returns
  #coordinates in Belgian Lambert 72 (EPSG:31370)
  
  
  coordinates(coord) <- c("lon", "lat")
  proj4string(coord) <- CRS("+init=epsg:4326") # WGS 84
  
  wgs84 <- CRS("EPSG:4326")
  Belgian <- CRS("EPSG:31370")
  
  coord_belgian <- spTransform(coord, Belgian)
  coord_belgian<-as.data.frame(coord_belgian)%>%
    rename(x=lon, y=lat)
  
  return(coord_belgian)
}





