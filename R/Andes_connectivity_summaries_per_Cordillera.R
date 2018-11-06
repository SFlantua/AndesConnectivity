########################################
#
#   Suzette Flantua 
#   June 2018
#
########################################

# Aim:
# Summarize ECA connectivity through time per cordillera
# Create different plots to compare connectivity among cordilleras


# Clear objects and environment
ls()
remove(list = ls())

# Load packages
library(tidyverse)
tidyverse_update()
# Libraries included in tidyverse
#library(ggplot2)
#library(tibble)
#library(tidyr)
#library("readr") # The readr package contains functions for reading i) delimited files, ii) lines and iii) the whole file. 
#library(dplyr)
library(scales)
library(gridExtra)
library(readxl)
library(raster)
library(lubridate)
library(stargazer)
library(directlabels)
library(Hmisc)

install.packages("Himsc")


#### GET ECA VALUES PER UFL PER CORDILLERA

# Set working directory
# setwd ('D:/R_scripts/Conefor_Summaries_Andes/Data')


# create a list of files in the directory and get a named list
file.list <- list.files(pattern='*.txt')
file.list

#### import UFL vs time file (Funza)
funza_ufl<-read_tsv("Time_vs_UFL_vs_Duration.txt", col_names = TRUE,col_types = NULL, quote = "\"") 
head(funza_ufl)



#### import EC files per cordillera and delete unnecessary columns ####

############################# Cordillera Central ####
COC_PC<-read_tsv("COC_results_all_EC(PC).txt", col_names = TRUE,col_types = NULL, quote = "\"")

#delete unnessary columns
COC_PC <- COC_PC %>%
  dplyr::select(-Distance,-Probability,-X5)

# EC(PC) causes problems later on
COC_PC <- rename(COC_PC, EC_PC = 'EC(PC)') 

# Add region column
COC_PC <-add_column(COC_PC,REGION="Central cordillera")
head(COC_PC)



#### #####################Cordillera Occidental ####
COCC_PC<-read_tsv("COCC_results_all_EC(PC).txt", col_names = TRUE,col_types = NULL, quote = "\"") 

#delete unnessary columns
COCC_PC <- COCC_PC %>%
  dplyr::select(-Distance,-Probability,-X5)

# EC(PC) causes problems later on
COCC_PC <- rename(COCC_PC, EC_PC = 'EC(PC)') 

# Add region column
COCC_PC <-add_column(COCC_PC,REGION="Western cordillera")
head(COCC_PC)


######################## Cordillera oriental ####
COR_PC<-read_tsv("COR_results_all_EC(PC).txt", col_names = TRUE,col_types = NULL, quote = "\"")

# delete unnessary columns
COR_PC <- COR_PC %>%
  dplyr::select(-Distance,-Probability,-X5)

# EC(PC) causes problems later on
COR_PC <- rename(COR_PC, EC_PC = 'EC(PC)') 

# Add region column
COR_PC <-add_column(COR_PC,REGION="Eastern cordillera")
head(COR_PC)


#################################### Merida ####
MER_PC<-read_tsv("MER_results_all_EC(PC).txt", col_names = TRUE,col_types = NULL, quote = "\"")

# delete unnessary columns
MER_PC <- MER_PC %>%
  dplyr::select(-Distance,-Probability,-X5)

# EC(PC) causes problems later on
MER_PC <- rename(MER_PC, EC_PC = 'EC(PC)') 

# Add region column
MER_PC <-add_column(MER_PC,REGION="Merida")
head(MER_PC)


################################# Ecuador ####
ECU_PC<-read_tsv("ECU_results_all_EC(PC).txt", col_names = TRUE,col_types = NULL, quote = "\"")

# delete unnessary columns
ECU_PC <- ECU_PC %>%
  dplyr::select(-Distance,-Probability,-X5)

# EC(PC) causes problems later on
ECU_PC <- rename(ECU_PC, EC_PC = 'EC(PC)') 

# Add region column
ECU_PC <-add_column(ECU_PC,REGION="Ecuador")
head(ECU_PC)


################################### SNSM ####
SNSM_PC<-read_tsv("SNSM_results_all_EC(PC).txt", col_names = TRUE,col_types = NULL, quote = "\"")

# delete unnessary columns
SNSM_PC <- SNSM_PC %>%
  dplyr::select(-Distance,-Probability,-X5)

# EC(PC) causes problems later on
SNSM_PC <- rename(SNSM_PC, EC_PC = 'EC(PC)') 

# Add region column
SNSM_PC <-add_column(SNSM_PC,REGION="SNSM")
head(SNSM_PC)




## Join files from cordilleras into one ULF vs PC - file
CORD_ALL_UFL_PC <- bind_rows(COC_PC,COCC_PC) %>%
  bind_rows(.,COR_PC) %>%
  bind_rows(.,ECU_PC) %>%
  bind_rows(.,MER_PC) %>%
  bind_rows(.,SNSM_PC)

CORD_ALL_UFL_PC
  

 
#### Link EC(PC) files to Funza file

# 1. Import file that combines between EC(PC) file and FUNZA file
prefix_ufl<-read_tsv("Prefix_UFL.txt", col_names = TRUE,col_types = NULL, quote = "\"")
prefix_ufl

# 2. Join prefix_ufl to the EC files of each cordillera
CORD_ALL_UFL_PC2 <-full_join(CORD_ALL_UFL_PC, prefix_ufl, by = "Prefix", copy = TRUE, suffix = c(".x", ".y"))
CORD_ALL_UFL_PC2

# 3. Join
funza_ufl_pc_cord <-full_join(funza_ufl, CORD_ALL_UFL_PC2, by = "UFL_m", copy = TRUE, suffix = c(".x", ".y"))

funza_ufl_pc_cord

# drop NA rows
funza_ufl_pc_cord <- funza_ufl_pc_cord %>% drop_na()

glimpse(funza_ufl_pc_cord)



## Add new column that calculates connectivity vs duration
funza_ufl_pc_cord_dur <- funza_ufl_pc_cord %>%
  mutate(EC_PC_duration = EC_PC*Duration_kyr)

funza_ufl_pc_cord_dur <- funza_ufl_pc_cord_dur %>% drop_na()

funza_ufl_pc_cord_dur


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
################################## PLOTS ####################################
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#                          THEMES & COLORS 


###################################################### COLOR PALETTE ###

# The palette with colors for each mountain range:
cbPalette <- c("#4F81BD", # Central Cordillera
               "#9BBB59", # Eastern Cordillera
               "#8064A2", # Ecuador
               "#4BACC6", # Merida
               "#F79646", # SNSM
               "#C0504D") # Western Cordillera
# "#4F81BD": dark blue
# "#9BBB59": green
# "#4BACC6": light blue
# "#C0504D": red
# "#F79646": Orange
# "#8064A2": purple
                 
cp <-  scale_fill_manual(values=cbPalette)
cp2 <-  scale_fill_manual(values=cbPalette, guide=FALSE)



###################################################### THEME ############

th <-  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size = 10), axis.title.x = element_text(size = rel(1)), axis.title.y = element_text(size = rel(1))) 




# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
############################## PLOT FCS per cordillera #####################
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

pretty_br = seq(0,3e+10,length.out=4) # this is for the vertical bar of ECA

# Plot 1: X = time, Y = Connectivity

pl1_FCS_vs_time <- ggplot(funza_ufl_pc_cord, aes(x = Time_ka, y = EC_PC, colour = REGION)) +
  geom_line() +
  scale_x_reverse(expand = c(0.05, 0, 0.2, 0)) +
  scale_y_continuous(breaks = pretty_br, labels=c('0',expression(1%*%10^10),expression(2%*%10^10),expression(3%*%10^10))) +
  guides(color=FALSE) +
  theme(legend.title = element_blank()) + 
  labs(x = "Years before present (ka)", y = "Connectivity (ECA)")

pl1_FCS_vs_time +
  th +
  cp +
  geom_dl(aes(label = REGION), method = list(dl.trans(x = x + .3), "last.bumpup"))

# geom_dl(aes(label = REGION), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8))
# direct.label(pl1_FCS_vs_time,"last.qp")


# Plot 2: X = Connectivity, Y = time 
pl2_FCS_vs_time <- ggplot(funza_ufl_pc_cord, aes(x = Time_ka, y = EC_PC, colour = REGION)) +
  geom_line() +
  coord_flip() +
  scale_x_reverse() +
  theme(legend.title = element_blank()) + 
  labs(x = "Years before present (ka)", y = "Connectivity (ECA)") +
  ggtitle("Flickering connectivity system (FCS) per mountain range") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(colour = "Mountain range")

pl2_FCS_vs_time +
  cp


# Plot 3: X = time, Y = Connectivity, separated in columns
pl3_FCS_vs_time <- ggplot(funza_ufl_pc_cord, aes(x = Time_ka, y = EC_PC, colour = REGION)) +
  geom_line() +
  scale_x_reverse() +
  scale_y_continuous(breaks = pretty_br, labels=c('0',expression(1%*%10^10),expression(2%*%10^10),expression(3%*%10^10))) +
  facet_wrap(~REGION,ncol=1) + # this is where the graph is
  theme(legend.position="top") +
  guides(color=FALSE) +
  theme(legend.title = element_blank()) + 
  labs(x = "Years before present (ka)", y = "Connectivity (ECA)")

pl3_FCS_vs_time +
  th +
  cp
  


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
############################## PLOT Density curve per cordillera #######
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


# plot 1: Plot density Connectivity (ECA)

pl1_FCS_density <-ggplot(funza_ufl_pc_cord, aes(EC_PC, color = REGION)) +
  geom_density(adjust = 1/2) +
  scale_x_continuous(trans='log2') +
  theme(legend.position= c(0.3, 0.8)) +
  theme(legend.title = element_blank()) + 
  labs(x = "Degree of connectivity", y = "Density") +
  ggtitle("Frequency of connectivity")

pl1_FCS_density  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(colour = "Mountain range") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_fill_manual(values=cbPalette)




# plot 2: Plot density Connectivity (ECA) log scale

pl2_FCS_density <-ggplot(funza_ufl_pc_cord, aes(EC_PC, color = REGION)) +
  geom_density() +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  theme(legend.position= c(0.3, 0.8)) +
  theme(legend.title = element_blank()) + 
  labs(x = "Connectivity (ECA)", y = "Density") +
  ggtitle("Frequency of connectivity") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(colour = "Mountain range")

pl2_FCS_density +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  scale_fill_manual(values=cbPalette)

pl2_FCS_density # display without background scheme





# Plot 3: Plot density Connectivity (ECA) considering duration

pl2_FCS_density_duration <-ggplot(funza_ufl_pc_cord_dur, aes(EC_PC_duration, color = REGION)) +
  geom_density() +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  theme(legend.position= c(0.2, 0.8)) +
  theme(legend.title = element_blank()) + 
  labs(x = "Connectivity (ECA)", y = "Density") +
  ggtitle("Duration connectivity") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(colour = "Mountain range")

pl2_FCS_density_duration +
  th +
  cp
  


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
############################## PLOT Violin graphs ##########################
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


# plot 1 - Violin shape trimmed to the range of the data
# define basics of plot
pl1_fcs_violin <- ggplot(funza_ufl_pc_cord, aes(factor(REGION), EC_PC)) +
  geom_violin(aes(fill = REGION)) + 
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x)))

pl1_fcs_violin2 <- pl1_fcs_violin + 
  th + 
  cp +
  theme(legend.position="top") +
  theme(legend.title = element_blank()) + 
  labs(x = "Mountain ranges", y = "Connectivity (ECA)") +
  ggtitle("Density plot trimmed to the range of the data") +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_blank()) +
  labs(colour = "Mountain range")

pl1_fcs_violin2



# Plot 2 All violin shape trimmed to maximum width to 1

#pl1_fcs_violin + aes(x = somethingelse) +
 # geom_violin(aes(fill = REGION),scale = "width") 

pl2_fcs_violin <- ggplot(funza_ufl_pc_cord, aes(factor(REGION), EC_PC)) +
  geom_violin(aes(fill = REGION),scale = "width") + 
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x)))

pl2_fcs_violin + 
  th + # predefined theme
  cp + # predefined colors
  theme(legend.position="top") +
  theme(legend.title = element_blank()) + 
  labs(x = "Mountain range", y = "Connectivity (ECA)") +
  ggtitle("Density plot trimmed to 1") +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_blank()) +
  labs(colour = "Mountain range")





# plot 3 - Violin shape trimmed to the range of the data with duration taken into account
pl3_fcs_violin <- ggplot(funza_ufl_pc_cord_dur, aes(factor(REGION), EC_PC_duration)) +
  geom_violin(aes(fill = REGION)) + 
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x)))

pl3_fcs_violin2 <- pl3_fcs_violin + 
  th + # predefined theme
  cp + # predefined colors
  theme(legend.position="top") +
  theme(legend.title = element_blank()) + 
  labs(x = "Mountain range", y = "Connectivity (ECA)") +
  ggtitle("Density plot trimmed to the range of the data weighted by duration") +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_blank()) +
  labs(colour = "Mountain range")

pl3_fcs_violin2



# Plot 4 All violin shape trimmed to maximum width to 1
pl4_fcs_violin <- ggplot(funza_ufl_pc_cord_dur, aes(factor(REGION), EC_PC_duration)) +
  geom_violin(aes(fill = REGION),scale = "width") + 
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x)))

pl4_fcs_violin + 
  th + # predefined theme
  cp + # predefined colors
  theme(legend.position="top") +
  theme(legend.title = element_blank()) + 
  labs(x = "Mountain range", y = "Connectivity (ECA)") +
  ggtitle("Density plot trimmed to 1 weighted by duration") +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_blank()) +
  labs(colour = "Mountain range")




# Plot 5 Violin plos in columns - Only based on frequency
# define basics of plot
pl5_fcs_violin <- ggplot(funza_ufl_pc_cord, aes(factor(REGION), EC_PC)) +
  geom_violin(aes(fill = REGION)) + 
  facet_wrap(~REGION,ncol=1) + # this is where the graph is organized in columns
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x)))

pl5_fcs_violin + 
  th + # predefined theme
  cp + # predefined colors
  theme(legend.position="none") + # no legend
  theme(axis.title.x=element_blank(), # nothing along x-axis
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())



# Plot 6 Columns - Considering also duration
pl6_fcs_violin <- ggplot(funza_ufl_pc_cord_dur, aes(factor(REGION), EC_PC_duration)) +
  geom_violin(aes(fill = REGION)) + 
  facet_wrap(~REGION,ncol=1) + # this is where the graph is organized in columns
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x)))

pl6_fcs_violin + 
  th + # predefined theme
  cp + # predefined colors
  theme(legend.position="none") + # no legend
  theme(axis.title.x=element_blank(), # nothing along x-axis
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


##############################################################################

# violin plot with mean points
pl3_fcs_violin2 +
  stat_summary(fun.y=mean, geom="point", shape=23, size=2)

# violin plot with median points
pl3_fcs_violin2 +
  stat_summary(fun.y=median, geom="point", size=2, color="red")

# violin plot with simple boxplot
pl3_fcs_violin2 + geom_boxplot(width=0.1)

# violin plot with crossbar
pl3_fcs_violin2 + 
  stat_summary(fun.data="mean_sdl", mult=1, geom="crossbar", width=0.1)

# violin plot with dot plot
pl3_fcs_violin2 + stat_summary(fun.data=mean_sdl,geom="pointrange")






# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##################################### COMBINED PLOTS #########################
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


pl1_fcs_violin3 <- pl1_fcs_violin +
  th + 
  cp2 +
  labs(x = "Mountain range", y = "Connectivity (ECA)") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

pl3_fcs_violin3 <- pl3_fcs_violin +
  th + 
  cp2 +
  labs(x = "Mountain range", y = "Connectivity (ECA)") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


comp_plot_1 <- grid.arrange(pl1_fcs_violin3, pl3_fcs_violin3, ncol = 2)






  
  
  
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  
############################## PLOT - Cumulative graph ###########
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


funza_ufl_pc_cord <- funza_ufl_pc_cord %>% 
  group_by(REGION) %>% 
  mutate(cumsum = cumsum(EC_PC * Duration_kyr))

pl1_FCS_cum <- ggplot(funza_ufl_pc_cord, aes(x = Time_ka, y = cumsum, group = REGION, color=REGION)) +
  geom_line(size=1.5) +
  scale_x_reverse() +
  theme(legend.position="top") +
  theme(legend.title = element_blank()) + 
  labs(x = "Years before present (ka)", y = "Cummulative connectivity") +
  ggtitle("Accumulated degree of connectivity")

pl1_FCS_cum  +
  scale_colour_manual(values=cbPalette)


#scale_x_reverse() +
  theme(legend.position="top") +
  theme(legend.title = element_blank()) + 
  labs(x = "Time (ka)", y = "Connectivity (ECA)") +
  ggtitle("Cumulative graph") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(colour = "Mountain range")







###################################################################


####### CALCULATE STATS ON ECA ############
  
  
# Calculate the mean of EC_PC per cordillera
EC_cord_dur_stats <- funza_ufl_pc_cord_dur %>%
  group_by(REGION) %>%
  summarise(
    mean_EC = mean(EC_PC_duration),
    min_EC = min(EC_PC_duration),
    max_EC = max(EC_PC_duration),
    var_EC = var(EC_PC_duration))
  
EC_cord_dur_stats %>% print(n = Inf)

## Add new column that calculates connectivity vs duration
EC_cord_dur_stats <- EC_cord_dur_stats %>%
  mutate(range_EC = max_EC*min_EC)

EC_cord_dur_stats %>% print(n = Inf)

write_csv(EC_cord_dur_stats, append = FALSE, col_names = !append)



## save data
save(funza_ufl_pc_cord, file.path(dir), funza_ufl_pc_cord_dur, EC_cord_dur_stats, file = "fcs_funza.Rdata")
  


