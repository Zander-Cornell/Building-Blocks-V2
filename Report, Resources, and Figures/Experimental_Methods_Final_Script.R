#loading the required packages
library(magrittr)
library(VWPre)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)

#GENERAL NOTES
#The data were extracted from Data Viewer, as individual Sample Reports for each participant, the data should be in .txt format!
#We extracted data for all the variables that we can possibly get from Data Viewer.
#But in the codes below, we will be selecting the columns that we need to make the size of the files more manageable.

################
#LOOP FOR PROCESSING EACH PARTICIPANT'S DATA FILE

# Get a list of all the files in the directory
file_list <- list.files(path = "C:/Users/lynch/Documents/Linguistics Projects and Papers/Adjective and Eye Tracking Project V2/Building Blocks V2 Data", pattern = ".txt", full.names = TRUE)

# Create an empty list to store the modified dataframes
modified_data_list <- list()

# Loop over the files and apply the functions to each file
for (file_name in file_list) {
  
  # Read the file into a dataframe
  df <- read.delim(file_name)
  
  #For some reason in extracting the files symbols are being added to this column, name so this corrects that
  colnames(df)[1] = "RECORDING_SESSION_LABEL"
  
  # selecting required columns
  data_c <- df %>% select(c("RECORDING_SESSION_LABEL", "IP_END_TIME", "IP_START_TIME", "LEFT_INTEREST_AREA_ID", 
                            "LEFT_INTEREST_AREA_LABEL", "RIGHT_GAZE_X", "RIGHT_GAZE_Y",
                            "RIGHT_INTEREST_AREA_ID", "RIGHT_INTEREST_AREA_LABEL", "RIGHT_IN_BLINK", "RIGHT_IN_SACCADE",
                            "SAMPLE_INDEX", "SAMPLE_MESSAGE", "TIMESTAMP", "TRIAL_INDEX", "TRIAL_START_TIME", 
                            "RESPONSE.2.", "RT.2.", "condition","item", "list"))
  
  #First step of VWPre package, checking for the required columns
  #converting the columns into the right format
  #relabeling Subject and Item columns
  df_p <- prep_data(data = data_c, Subject = "RECORDING_SESSION_LABEL", Item = "item")
  
  df_na <- relabel_na(data = df_p, NoIA = 5)
  check_ia(data = df_na)
  
  #selecting only exp items
  #unique(df_na$condition) for checking condition names
  df_exp <- filter(df_na, condition %in% c("Simp_Pat", "Simp_Nat", "Simp_Sub", "Size_Pat", "Comp_Sub", "Comp_Nat", "Nat_Pat", "Size_Nat", "Pat_Nat", "Comp_Pat"))

  #to align for reciprocal onset time, this sets reciprocal onset milisecond as 0 for each event
  unique(df_exp$SAMPLE_MESSAGE)
  df_aligned <- align_msg(data = df_exp, Msg = "PLAY_SOUND") 
  
  #create time series
  df_time <- create_time_series(data = df_aligned, Adjust = 0)
  
  #we need this for bin_drop & trackloss steps
  #this creates 9 new columns for the data from right eye, which is to be used in later functions
  df_eye <- select_recorded_eye(data = df_time, Recording = "R", WhenLandR = "Right")
  
  #trackloss
  df_mark_trackloss <- mark_trackloss(df_eye, Type = "Both", ScreenSize = c(1920, 1200)) #two new columns showing offscreen and trackloss
  df_trackloss_removed <- rm_trackloss_events(df_mark_trackloss, RequiredData = 50) #choose the percentage and remove the events
  
  #to bin the data for plots
  df_bin <- bin_prop(df_trackloss_removed, NoIA = 5, BinSize = 25, SamplingRate = 1000)
  
  # Store the modified dataframe in the list
  modified_data_list[[file_name]] <- df_bin
}

# Concatenate the modified dataframes into a single dataframe
final_data <- do.call(rbind, modified_data_list)
min(final_data$Time)
max(final_data$Time)

#plotting the data 
data_Size_Nat <- filter(final_data, condition %in% c("Size_Nat"))

plot_Size_Nat <- plot_avg(data = data_Size_Nat, type = "proportion", xlim = c(0, 3000),
                          IAColumns = c(IA_0_P = "Outside", IA_1_P = "Target", IA_2_P = "Size Comp", 
                                        IA_3_P = "Nationality Comp", IA_4_P = "Pattern Comp"),
                          Condition1 = "condition", Condition2 = NULL, Cond1Labels = NA, Cond2Labels = NA, 
                          ErrorBar = FALSE, ErrorBand = TRUE, ErrorType = "CI", ConfLev = 95, CItype = "simultaneous", VWPreTheme = FALSE) + theme_minimal() + 
  scale_color_manual(values = c("#999999", "#F0E442", "#56B4E9", "#E69F00", "#009E73"),
                     labels = c("Outside", "Target", "Size Comp", "Nationality Comp", "Pattern Comp")) + 
  ggtitle("Click on the large British triangle")  +
  ylim(0,1) +
geom_vline(xintercept = 649.07175)+
geom_vline(xintercept = 963.865) +
geom_vline(xintercept = 1343.31375) 
plot_Size_Nat

data_Size_Pat <- filter(final_data, condition %in% c("Size_Pat"))

plot_Size_Pat <- plot_avg(data = data_Size_Pat, type = "proportion", xlim = c(0, 3000),
                          IAColumns = c(IA_0_P = "Outside", IA_1_P = "Target", IA_2_P = "Size Comp", 
                                        IA_3_P = "Nationality Comp", IA_4_P = "Pattern Comp"),
                          Condition1 = "condition", Condition2 = NULL, Cond1Labels = NA, Cond2Labels = NA, 
                          ErrorBar = FALSE, ErrorBand = TRUE, ErrorType = "CI", ConfLev = 95, CItype = "simultaneous", VWPreTheme = FALSE) + theme_minimal() + 
  scale_color_manual(values = c("#999999", "#F0E442", "#56B4E9", "#E69F00", "#009E73"),
                     labels = c("Outside", "Target", "Size Comp", "Nationality Comp", "Pattern Comp")) + 
  ggtitle("Click on the large checkered triangle")  +
  ylim(0,1) +
  geom_vline(xintercept = 623.88675)+
  geom_vline(xintercept = 940.8925) +
  geom_vline(xintercept = 1358.3025) 
plot_Size_Pat

Sub_Int <- plot_grid(plot_Size_Pat, plot_Size_Nat, rel_widths = c(1,1), rel_heights = c(1,1))
Sub_Int

data_Pat_Nat <- filter(final_data, condition %in% c("Pat_Nat"))

plot_Pat_Nat <- plot_avg(data = data_Pat_Nat, type = "proportion", xlim = c(0, 3000),
                          IAColumns = c(IA_0_P = "Outside", IA_1_P = "Target", IA_2_P = "Size Comp", 
                                        IA_3_P = "Nationality Comp", IA_4_P = "Pattern Comp"),
                          Condition1 = "condition", Condition2 = NULL, Cond1Labels = NA, Cond2Labels = NA, 
                          ErrorBar = FALSE, ErrorBand = TRUE, ErrorType = "CI", ConfLev = 95, CItype = "simultaneous", VWPreTheme = FALSE) + theme_minimal() + 
  scale_color_manual(values = c("#999999", "#F0E442", "#56B4E9", "#E69F00", "#009E73"),
                     labels = c("Outside", "Target", "Size Comp", "Nationality Comp", "Pattern Comp")) + 
  ggtitle("Click on the checkered British triangle")  +
  ylim(0,1) +
  geom_vline(xintercept = 429.285)+
  geom_vline(xintercept = 831.4916667) +
  geom_vline(xintercept = 1200.873333) 
plot_Pat_Nat

data_Nat_Pat <- filter(final_data, condition %in% c("Nat_Pat"))

plot_Nat_Pat <- plot_avg(data = data_Nat_Pat, type = "proportion", xlim = c(0, 3000),
                         IAColumns = c(IA_0_P = "Outside", IA_1_P = "Target", IA_2_P = "Size Comp", 
                                       IA_3_P = "Nationality Comp", IA_4_P = "Pattern Comp"),
                         Condition1 = "condition", Condition2 = NULL, Cond1Labels = NA, Cond2Labels = NA, 
                         ErrorBar = FALSE, ErrorBand = TRUE, ErrorType = "CI", ConfLev = 95, CItype = "simultaneous", VWPreTheme = FALSE) + theme_minimal() + 
  scale_color_manual(values = c("#999999", "#F0E442", "#56B4E9", "#E69F00", "#009E73"),
                     labels = c("Outside", "Target", "Size Comp", "Nationality Comp", "Pattern Comp")) + 
  ggtitle("Click on the British checkered triangle")  +
  ylim(0,1) +
  geom_vline(xintercept = 449.3156667)+
  geom_vline(xintercept = 837.2736667) +
  geom_vline(xintercept = 1114.029) 
plot_Nat_Pat

Int_Int <- plot_grid(plot_Pat_Nat, plot_Nat_Pat, rel_widths = c(1,1), rel_heights = c(1,1))
Int_Int

data_one_adj_simp_Nat <- filter(final_data, condition %in% c( "Simp_Nat"))

plot_Simp_Nat <- plot_avg(data = data_one_adj_simp_Nat, type = "proportion", xlim = c(0, 3000),
                         IAColumns = c(IA_0_P = "Outside", IA_1_P = "Target", IA_2_P = "Size Comp", 
                                       IA_3_P = "Nationality Comp", IA_4_P = "Pattern Comp"),
                         Condition1 = "condition", Condition2 = NULL, Cond1Labels = NA, Cond2Labels = NA, 
                         ErrorBar = FALSE, ErrorBand = TRUE, ErrorType = "CI", ConfLev = 95, CItype = "simultaneous", VWPreTheme = FALSE) + theme_minimal() + 
  scale_color_manual(values = c("#999999", "#F0E442", "#56B4E9", "#E69F00", "#009E73"),
                     labels = c("Outside", "Target", "Nationality Comp", "Contrast", "Distractor")) + 
  ggtitle("Click on the German triangle")  +
  ylim(0,1) +
  geom_vline(xintercept = 473.0916667)+
  geom_vline(xintercept = 817.511)
plot_Simp_Nat

data_one_adj_simp_Pat <- filter(final_data, condition %in% c( "Simp_Pat"))

plot_Simp_Pat <- plot_avg(data = data_one_adj_simp_Pat, type = "proportion", xlim = c(0, 3000),
                          IAColumns = c(IA_0_P = "Outside", IA_1_P = "Target", IA_2_P = "Size Comp", 
                                        IA_3_P = "Nationality Comp", IA_4_P = "Pattern Comp"),
                          Condition1 = "condition", Condition2 = NULL, Cond1Labels = NA, Cond2Labels = NA, 
                          ErrorBar = FALSE, ErrorBand = TRUE, ErrorType = "CI", ConfLev = 95, CItype = "simultaneous", VWPreTheme = FALSE) + theme_minimal() + 
  scale_color_manual(values = c("#999999", "#F0E442", "#56B4E9", "#E69F00", "#009E73"),
                     labels = c("Outside", "Target", "Pattern Comp", "Contrast", "Distractor")) + 
  ggtitle("Click on the solid triangle")  +
  ylim(0,1) +
  geom_vline(xintercept = 460.425)+
  geom_vline(xintercept = 833.706)
plot_Simp_Pat

data_one_adj_simp_Sub <- filter(final_data, condition %in% c( "Simp_Sub"))

plot_Simp_Sub <- plot_avg(data = data_one_adj_simp_Sub, type = "proportion", xlim = c(0, 3000),
                         IAColumns = c(IA_0_P = "Outside", IA_1_P = "Target", IA_2_P = "Contrast", 
                                       IA_3_P = "Shape Comp", IA_4_P = "Dist"),
                         Condition1 = "condition", Condition2 = NULL, Cond1Labels = NA, Cond2Labels = NA, 
                         ErrorBar = FALSE, ErrorBand = TRUE, ErrorType = "CI", ConfLev = 95, CItype = "simultaneous", VWPreTheme = FALSE) + theme_minimal() + 
  scale_color_manual(values = c("#999999", "#F0E442", "#56B4E9", "#E69F00", "#009E73"),
                     labels = c("Outside", "Target", "Size Competitor", "Contrast", "Distractor")) + 
  ggtitle("Click on the large triangle")  +
  ylim(0,1) +
  geom_vline(xintercept = 507.8723333)+
  geom_vline(xintercept = 799.3625)
plot_Simp_Sub

Simp <- plot_grid(plot_Simp_Sub, plot_Simp_Pat, plot_Simp_Nat, rel_widths = c(1,1), rel_heights = c(1,1))
Simp

data_one_adj_comp_sub <- filter(final_data, condition %in% c("Comp_Sub"))

plot_Comp_Sub <- plot_avg(data = data_one_adj_comp_sub, type = "proportion", xlim = c(0, 3000),
                              IAColumns = c(IA_0_P = "Outside", IA_1_P = "Target", IA_2_P = "Size Comp", 
                                            IA_3_P = "Nationality Comp", IA_4_P = "Pattern Comp"),
                              Condition1 = "condition", Condition2 = NULL, Cond1Labels = NA, Cond2Labels = NA, 
                              ErrorBar = FALSE, ErrorBand = TRUE, ErrorType = "CI", ConfLev = 95, CItype = "simultaneous", VWPreTheme = FALSE) + theme_minimal() + 
  scale_color_manual(values = c("#999999", "#F0E442", "#56B4E9", "#E69F00", "#009E73"),
                     labels = c("Outside", "Target", "Contrast", "Size Competitor", "Distractor")) + 
  ggtitle("Click on the large triangle")  +
  ylim(0,1) +
  geom_vline(xintercept = 507.8723333)+
  geom_vline(xintercept = 799.3625)
plot_Comp_Sub

data_one_adj_comp_Nat <- filter(final_data, condition %in% c("Comp_Nat"))

plot_Comp_Nat <- plot_avg(data = data_one_adj_comp_Nat, type = "proportion", xlim = c(0, 3000),
                          IAColumns = c(IA_0_P = "Outside", IA_1_P = "Target", IA_2_P = "Size Comp", 
                                        IA_3_P = "Nationality Comp", IA_4_P = "Pattern Comp"),
                          Condition1 = "condition", Condition2 = NULL, Cond1Labels = NA, Cond2Labels = NA, 
                          ErrorBar = FALSE, ErrorBand = TRUE, ErrorType = "CI", ConfLev = 95, CItype = "simultaneous", VWPreTheme = FALSE) + theme_minimal() + 
  scale_color_manual(values = c("#999999", "#F0E442", "#56B4E9", "#E69F00", "#009E73"),
                     labels = c("Outside", "Target", "Nationality Competitor", "Contrast", "Distractor")) + 
  ggtitle("Click on the British triangle")  +
  ylim(0,1) +
  geom_vline(xintercept = 473.0916667)+
  geom_vline(xintercept = 817.511)
plot_Comp_Nat

data_one_adj_comp_Pat <- filter(final_data, condition %in% c("Comp_Pat"))

plot_Comp_Pat <- plot_avg(data = data_one_adj_comp_Pat, type = "proportion", xlim = c(0, 3000),
                         IAColumns = c(IA_0_P = "Outside", IA_1_P = "Target", IA_2_P = "Size Comp", 
                                       IA_3_P = "Nationality Comp", IA_4_P = "Pattern Comp"),
                         Condition1 = "condition", Condition2 = NULL, Cond1Labels = NA, Cond2Labels = NA, 
                         ErrorBar = FALSE, ErrorBand = TRUE, ErrorType = "CI", ConfLev = 95, CItype = "simultaneous", VWPreTheme = FALSE) + theme_minimal() + 
  scale_color_manual(values = c("#999999", "#F0E442", "#56B4E9", "#E69F00", "#009E73"),
                     labels = c("Outside", "Target", "Pattern Competitor", "Contrast",  "Distator")) + 
  ggtitle("Click on the striped triangle")  +
  ylim(0,1) +
  geom_vline(xintercept = 460.425)+
  geom_vline(xintercept = 833.706) 
plot_Comp_Pat

Comp <- plot_grid(plot_Comp_Sub, plot_Comp_Pat, plot_Comp_Nat, rel_widths = c(1,1), rel_heights = c(1,1))
Comp