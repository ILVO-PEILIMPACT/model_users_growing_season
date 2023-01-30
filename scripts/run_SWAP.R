# SWAP-WOFOST model for Flanders in the context of the project PEILIMPACT
# Flanders Research Institute for Agriculture, Fisheries and Food (ILVO) & Wageningen Environmental Research
# Original code received from Martin Mulder on March 2022, License: GPL (>= 3)
# Changes for project PEILIMPACT made by Diana Estrella


#This code is organized in four parts:
# 1) Loading libraries and reading arguments
# 2) Model input generation
# 3) Run SWAP-WOFOST model
# 4) Post processing of the model results

#------------------------------------------------
# set modus
test <- FALSE

# read arguments
#------------------------------------------------

if (!test) {
  command_args <- commandArgs(trailingOnly = TRUE)
  if (length(command_args) != 1) {
    stop("wrong usage of script, arguments should be:\n- ctrl")
  }
} else {
  command_args <- NULL
  command_args[1] <- "./model/control.inp"
  setwd("C:/SWAP/swap_wofost_users")
}
ctrl <- command_args[1]

# load libraries
#------------------------------------------------
online <- FALSE                                # try to load the packages in case there is no connection to the internet
update <- FALSE                                # check for package updates
source("./scripts/libraries.R")
source("./scripts/functions.R")


# run R-script
#------------------------------------------------
if(!interactive()) pdf(NULL)

message(str_c("\nProgram R-PROG started...\n"))


# INPUT GENERATION
# This section creates the sqlite database with the data for the specific coordinate

message(str_c("\nGenerate input data...\n"))

#Calculation of indirect effects

indirect <- TRUE

#Reading coordinates 
file_coord <- "./input_user.xlsx"
location <- read_excel(file_coord)
location<-convert_Belgian_decimal(coord=location)
x_crd <- location$x
y_crd <- location$y

#Simulation period
START <- as_date(location$start)
END <- as_date(location$end)


#Get dataframe of runs

db_tmp <- get_runs(file_coord=file_coord, ctrl=ctrl)

#Dynamic information

#Groundwater levels

GWL <- get_groundwater(database=db_tmp, START=START, END=END)

# Initial soil moisture is assumed equal to the GWL at the start of simulation 
GWLI <-GWL%>%
  group_by(gw_id)%>%
  filter (DATE5 == START)%>% 
  mutate(scenario_id="direct")%>%
  mutate(GWLI = HBOT5-600)%>% #To transform again to GWL
  select(scenario_id, gw_id, GWLI)

write_csv (x=GWLI, file= "./data/initial_con.csv")

if (indirect){
  GWL_ind <- tibble(scenario_id = "indirect", DATE5 = c(START, END), HBOT5=100.0)
  GWLI_ind <- tibble(scenario_id = "indirect", GWLI = -500.0)
 
  write_csv(x=GWL_ind, file="./data/grondwater_ind.csv")
  write_csv(x=GWLI_ind, file="./data/initial_con_ind.csv")
}


# Main dataframe 

db <- get_main_database (database=db_tmp)

#Sqlite database

# Input data
input_data <- c(
  "discretisatie", 
  "eigenschappen", 
  "gewasontwikkeling",
  "gewaskalender",
  "gewasweerstand",
  "grasverlies",
  "grondwater",
  "grondwater_ind",
  "initial_con",
  "initial_con_ind",
  "irrigatie",
  "management",
  "output",
  "scenario",
  "meteo",
  "wortelzone"
)

#Create sqlite database and save it in the folder "database"

create_sqlite (list_data= input_data, database=db, START=START, END=END)


# ---- RUN-SWAP ----

# set start of program
TIMPRGSTART <- Sys.time()

# This section run the model SWAP using the sqlite database 

# set directories
dir_run <- get_dir(file = ctrl, item = "DIRRUN")

# set SWAP program and template
command <- get_record(file = ctrl, item = "PRGSWP")
tmplt_swp <- get_record(file = ctrl, item = "FILSWP")
tmplt_dra <- get_record(file = ctrl, item = "FILDRA", item_exists = FALSE)
dir_met <- get_record(file = ctrl, item = "DIRMET")
dir_ini <- get_record(file = ctrl, item = "DIRINI", item_exists = FALSE)
dir_crp <- get_record(file = ctrl, item = "DIRCRP")

# set database
file_sql <- get_record(file = ctrl, item = "FILSQL")

# set run_id
run_id <- get_record(file = ctrl, item = "RUNID")
if (run_id == "ALL") {
  run_id <- get_run_id_SQL(file_sql = file_sql)
} else {
  run_id <- string_sequence(string = run_id)
}


message(str_c("\nSimulate SWAP runs...\n"))
  
# run SWAP
pb <- progress_bar$new(total = length(run_id), format = " SWAP run: :what (:percent)", clear = TRUE)

for (s_run_id in run_id) {
  
  # prepare swap run
  file_swp <- str_c(dir_run, "/run_", formatC(x = s_run_id, format = "d", width = 9, flag = "0"), "/swap.swp")
  create_SWAP(file_swp = file_swp, file_sql = file_sql, run_id = s_run_id, tmplt_swp = tmplt_swp, dir_met = dir_met, dir_ini = dir_ini, dir_crp = dir_crp, tmplt_dra = tmplt_dra, quiet = TRUE)
  
  # execute swap
  pb$tick(tokens = list(what = s_run_id))
  run_SWAP(command = command, file_swp = file_swp, quiet = TRUE)
  
  #Remove unnecessary files
  dir_folder<-str_c(dir_run,"/run_",formatC(x = s_run_id, format = "d", width = 9, flag = "0"))
  pattern<-"*.exe|*.ok|*.log|*.met|*.co2"
  filetoremove<-list.files(dir_folder, recursive = TRUE, pattern = pattern)
  file.remove(file.path(dir_folder, filetoremove))
  
  
}
  
# ---- return part of procedure ----

TIMPRGEND <- Sys.time()
TIMPRGCALC <- as.numeric(difftime(time1 = TIMPRGEND, time2 = TIMPRGSTART, units = "secs"))
message(paste0("\nProgram R-PROG successfully ended in ", floor(TIMPRGCALC / 60), " minutes and ", ceiling(TIMPRGCALC %% 60), " seconds"))

#----- POSTPROCESSING-------
#This section analyses the SWAP output and calculates a summary table and example plots

message(str_c("\nAnalyse results...\n"))


#run ids
run_id <- get_runs_direct(file_sql = file_sql)

#Growing curve

data_raw <- get_data_raw(run_id=run_id, dir_run=dir_run, file_sql=file_sql)

crop_id<-unique(data_raw$crop_id)

data_filter<-data_raw%>%
  mutate(year = year(datetime))%>%
  filter(if_else(crop_id==1, PDM!=700, PDM!=0))

#Get summary table with yearly yields
data_procc <- get_data_procc(run_id=run_id, dir_run=dir_run, file_sql=file_sql)

dir_run<-"./output"

#Yield data
yield_data<-data_procc%>%
  mutate(
    Y_pot = hrvpotbio/1000,
    Y_act = (Y_pot*(100-dmgtot)/100), #Actual yield considering direct and indirect effects
    year=as.integer(year)
  )

#Soil texture
soil_model<-read_csv(file="./data/soil_classification.csv", show_col_types = FALSE)
conn <- dbConnect(RSQLite::SQLite(), dbname = file_sql)
name<-dbListTables(conn)
db_runs <- as_tibble(dbReadTable(conn = conn, name = "Runs"))%>%
  filter(scenario_id=="direct")

dbDisconnect(conn = conn)

db_soil<-left_join(db_runs, soil_model, by = "soil_id")
 
#Precipitation deficit

def_prec<-prec_deficit(file_sql=file_sql, dir_met=dir_met, start=START, end=END, discr="yearly")%>%
  mutate(meteo_id=as.numeric(meteo_id))

#Summary table

yield_data_user <- cbind(yield_data, P_ET=def_prec$P_ET_yearly, soil_texture=db_soil$soil_texture)%>%
  select(run_id, year, crop, ghg, glg, P_ET, soil_texture, Y_pot, Y_act, hrvpotvem, hrvpotdve, dmgtot, dmgind, dmgdir, dmgwet, dmgdry)


#PLOTS

for (s_run_id in run_id){

  dir_plots<-str_c(dir_run,"/run_",formatC(x = s_run_id, format = "d", width = 9, flag = "0"))
  
  data_run<-data_filter%>%
    filter(run_id==s_run_id)
  
  yield_run<-yield_data_user%>%
    filter(run_id==s_run_id)
  

  size=20
  # Growing curve
  
  yield <-c("Potential","Actual")
  plot_growing <- ggplot(data=data_run)%>%
    + geom_line(aes(x = datetime, y = PDM/1000,  color = yield[1], group=year)) %>% 
    + geom_line(aes(x = datetime, y = DM/1000, color = yield[2], group=year)) %>% 
    + scale_color_manual(values=c("Potential"="azure3","Actual"= "lightseagreen"))%>%
    + labs(x="", y = "Dry matter yield (ton/ha)", color="") %>%
    + theme_light()%>%
    + theme(text=element_text(size=size))%>%
    + theme(legend.position="top")
  
  ggsave(filename = str_c(dir_plots,"/growing_curve.png"), plot=plot_growing, width = 10, height = 6)
  
  
  #Yearly yield
  plot_yield <-ggplot(data=yield_run)%>%
    + geom_col(aes(x = as.character(year), y = Y_pot, fill = yield[1]), width = 0.5) %>% 
    + geom_col(aes(x = as.character(year), y = Y_act, fill = yield[2]), width = 0.5) %>% 
    + scale_fill_manual(values=c("Potential"="azure3","Actual"= "lightseagreen"))%>%
    + labs(x="", y = "Dry matter yield (ton/ha)", fill="") %>%
    + theme_light()%>%
    + theme(text=element_text(size=size))%>%
    + scale_x_discrete(labels = function(x) 
      stringr::str_wrap(x, width = 8))%>%
    + theme(legend.position="top")
  
  ggsave(filename =str_c(dir_plots,"/average_yield.png"), plot =plot_yield, width = 10, height = 6)
  
  #Transpiration reduction
  
  yield_stack<-yield_run%>%
    select(year, dmgdry, dmgwet,dmgind)%>%
    rename(
      indirect=dmgind,
      wet = dmgwet,
      dry = dmgdry
    )
  
  yield_reduction<-melt(yield_stack, id.var = c('year'), variable.name = 'yield_red')
  
  Transp <- ggplot(data = yield_reduction) %>%
    + geom_col(aes(x = as.character(year), y = value, fill = yield_red), alpha=0.8 , width = 0.5) %>%
    + scale_fill_manual(values=c("dry"="red3", "wet"="dodgerblue3","indirect"="navajowhite3"))%>%
    + labs(x="", y = "Yield reduction(%)", fill = "Stress type") %>%
    + theme_light()%>%
    + theme(text=element_text(size=size), legend.position="top")
  
  #Precipitation deficit
  
  plot0<-ggplot(yield_run)%>%
    + geom_col( aes(x = as.factor(year), y = P_ET), fill="dodgerblue3", width = 0.5)%>% 
    + labs(x="", y = "Average P-ET (mm)") %>%
    + theme_light()%>%
    + theme(text=element_text(size=20))
  
  boxplot_deficit<-ggarrange(plot0,
                             Transp,
                             nrow = 2,
                             ncol = 1)

  ggsave(filename = str_c(dir_plots,"/yield_reduction.png"), plot =boxplot_deficit, width = 10, height = 8)

  #Saving summary table 
  
  
  file_data<-str_c(dir_plots,"/summary.csv")
  write_csv(x=yield_run, file=file_data, progress=FALSE)
  
  
}


message(str_c("\nDone! \n"))

q(save = "no")



 