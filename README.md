# Directory Description
The "rawdata" folder contains the raw CSV data of platelet volumes for each thrombus. Meanwhile, the "figures" folder contains the figures generated from the R code file ```Figures.R```. Below, we document the package versions used in the R code, and briefly explain the written R code.

# R Code Documentation

The following packages were imported, as shown below. The package versions are also shown, although precise version matches are likely not necessary. 

```{r}
library(ggplot2)
library(mosaic)
library(tidyverse)
library(Rmisc)
library(tidyr)
library(plyr)
library(reshape2)
library(gridExtra)

SessionInfo()

other attached packages:
 [1] mosaic_1.5.0      Matrix_1.2-18     mosaicData_0.17.0 ggformula_0.9.2   ggstance_0.3.3    gridExtra_2.3     reshape2_1.4.4    Rmisc_1.5        
 [9] plyr_1.8.5        lattice_0.20-38   forcats_0.4.0     stringr_1.4.0     dplyr_0.8.3       purrr_0.3.3       readr_1.3.1       tidyr_1.0.0      
[17] tibble_2.1.3      tidyverse_1.3.0   ggplot2_3.3.2    

loaded via a namespace (and not attached):
 [1] ggrepel_0.8.1     Rcpp_1.0.3        lubridate_1.7.4   assertthat_0.2.1  zeallot_0.1.0     digest_0.6.23     mime_0.8          R6_2.4.1         
 [9] cellranger_1.1.0  backports_1.1.5   reprex_0.3.0      httr_1.4.1        pillar_1.4.3      rlang_0.4.2       lazyeval_0.2.2    readxl_1.3.1     
[17] rstudioapi_0.10   splines_3.6.2     htmlwidgets_1.5.3 munsell_0.5.0     shiny_1.4.0       broom_0.5.4       compiler_3.6.2    httpuv_1.5.2     
[25] modelr_0.1.5      pkgconfig_2.0.3   htmltools_0.4.0   tidyselect_0.2.5  mosaicCore_0.6.0  fansi_0.4.1       crayon_1.3.4      dbplyr_1.4.2     
[33] withr_2.1.2       later_1.0.0       MASS_7.3-51.4     grid_3.6.2        nlme_3.1-142      jsonlite_1.6      xtable_1.8-4      gtable_0.3.0     
[41] lifecycle_0.1.0   DBI_1.1.0         magrittr_1.5      scales_1.1.0      cli_2.0.1         stringi_1.4.4     fs_1.3.1          promises_1.1.0   
[49] leaflet_2.0.3     xml2_1.2.2        ggdendro_0.1-20   generics_0.0.2    vctrs_0.2.1       tools_3.6.2       glue_1.3.1        hms_0.5.3        
[57] crosstalk_1.0.0   fastmap_1.0.1     yaml_2.2.0        colorspace_1.4-1  rvest_0.3.5       haven_2.2.0      
```

Basic comments are included in the ```Figures.R``` code, but line-by-line comments are not included for conciseness. I heavily recommend referring to the ggplot2 documentation for understanding what each line of code means, as there is little actual data crunching and the code is mainly used to generate figures. 


### Loading the Data
We load the raw CSV files into R, and manually specify the time points and vault volumes with ```times``` and ```vaults```, respectively. 

```{r}
#################
### LOAD DATA ###
#################


files = c('6.21.17 WT1 1 min.csv',
          '4.18.19 WT5 1 min no-p.csv',
          '2.11.19 WT4 1 min p.csv',
          '9.18.18 WT2 5 min.csv',
          '2.11.19 WT6 5 min no-p.csv',
          '2.11.19 WT5 5 min no-p.csv',
          '12.13.18 WT4 5 min.csv', 
          '7.30.19 S2 20 min.csv',
          '8.27.19 S4 20 min.csv')

times = c(1,1,1,5,5,5,5,20,20)

Vaults <- c(0,0,0,699400,113300,1439760,476500,110425,42337)
```

### Specifying Figure Settings
Next we specify the formatting of the plot. For example, font sizes, point sizes, line thicknesses, etc. The names are fairly self-explanatory. 

```{r}
# Plot Format
Error.Width  <- 1
Error.Size   <-1
Mean.Width   <- 2.5
Mean.Size    <- 3
Point.Size   <- 8
Point.Alpha  <- 0.6
Point.Jitter <- 0.5
Line.Size    <- 1

# Text Format
Title.Size   <- 32
Title.hjust  <- 0.5
x.Element    <- rel(3)
y.Element    <- rel(3)
x.Text       <- 24
y.Text       <- 24
Legend.Location  <- c(0.7,0.7) 
Legend.Location2 <- c(0.7,0.5) 
Legend.Size  <- 24

# Image Dimension
Single.Width  <- 10.5
Single.Height <- 9
```

### Basic Data Analaysis Prior to Figure Creation
To create the figures, we must do basic data analysis (i.e. basic arithmetic). Most of it is just reformatting the raw CSV data.

```{r}
#############################
###    CREATE DATASET 1   ###
#############################
### FOR FIGURES 1A AND 1C ###
#############################

DataSet1 <- data.frame(matrix(nrow=length(files), ncol=3))
DataSet1 <- plyr::rename(DataSet1, c("X1"="Total", "X2"="Extravascular", "X3"="Intravascular"))
DataSet1$Time <- times
ct <- 1

for (file in files){
  TypeData1 <- read.csv(paste("~/",file,sep=""))
  Extra <- rowSums(TypeData1[c("Tight","Loose","Degran")])[1]+rowSums(TypeData1[c("Tight","Loose","Degran")])[4]
  Intra <- rowSums(TypeData1[c("Tight","Loose","Degran")])[3]+rowSums(TypeData1[c("Tight","Loose","Degran")])[6]
  DataSet1$Extravascular[ct] <- Extra
  DataSet1$Intravascular[ct] <- Intra
  DataSet1$Total[ct] <- Extra + Intra
  ct <- ct+1
}


###########################
###   CREATE DATASET 2  ###
###########################
###    FOR FIGURES 1B   ###
###########################

DataSet2 <- data.frame(matrix(nrow=length(files), ncol=4))
DataSet2 <- plyr::rename(DataSet2, c("X1"="Total", "X2"="Tight", "X3"="Loose", "X4"="Degranulated"))
DataSet2$Time <- times
ct <- 1

for (file in files){
  TypeData2 <- read.csv(paste("~/",file,sep=""))
  Tight  <- sum(TypeData2$Tight)
  Loose  <- sum(TypeData2$Loose)
  Degran <- sum(TypeData2$Degran)
  Total <- Tight+Loose+Degran
  DataSet2$Tight[ct] <- Tight
  DataSet2$Loose[ct] <- Loose
  DataSet2$Degranulated[ct] <- Degran
  DataSet2$Total[ct] <- Total
  ct <- ct+1
}


############################
###   CREATE DATASET 3   ###
############################
### FOR FIGURES 2A to 2C ###
############################

Dataset3 <- data.frame(matrix(nrow=length(files)*9, ncol=6))
Dataset3 <- plyr::rename(Dataset3, c("X1"="Type","X2"="Location","X3"="Volume","X4"="Fraction","X5"="Time","X6"="Clot"))
Dataset3$Time <- rep(times, each=9)
ct <- 1

for (file in files){
  # Import and preprocess data into long format for ggplot2
  TypeData3 <- read.csv(paste(paste("~/",file,sep=""),sep=""))
  Dataset3$Type[ct] <- "Tightly Adherent"
  Dataset3$Location[ct] <- "Extravascular"
  Dataset3$Volume[ct]   <- TypeData3$Tight[1]+TypeData3$Tight[4]
  Dataset3$Fraction[ct] <- Dataset3$Volume[ct]/sum(TypeData3$Tight)*100
  Dataset3$Clot[ct]     <- file
  ct <- ct + 1
  Dataset3$Type[ct] <- "Tightly Adherent"
  Dataset3$Location[ct] <- "Intravascular"
  Dataset3$Volume[ct]   <- TypeData3$Tight[3]+TypeData3$Tight[6]
  Dataset3$Fraction[ct] <- Dataset3$Volume[ct]/sum(TypeData3$Tight)*100
  Dataset3$Clot[ct]     <- file
  ct <- ct + 1
  Dataset3$Type[ct] <- "Tightly Adherent"
  Dataset3$Location[ct] <- "Cavity"
  Dataset3$Volume[ct]   <- TypeData3$Tight[2]+TypeData3$Tight[5]
  Dataset3$Fraction[ct] <- Dataset3$Volume[ct]/sum(TypeData3$Tight)*100
  Dataset3$Clot[ct]     <- file
  ct <- ct + 1
  Dataset3$Type[ct] <- "Loosely Adherent"
  Dataset3$Location[ct] <- "Extravascular"
  Dataset3$Volume[ct]   <- TypeData3$Loose[1]+TypeData3$Loose[4]
  Dataset3$Fraction[ct] <- Dataset3$Volume[ct]/sum(TypeData3$Loose)*100
  Dataset3$Clot[ct]     <- file
  ct <- ct + 1
  Dataset3$Type[ct] <- "Loosely Adherent"
  Dataset3$Location[ct] <- "Intravascular"
  Dataset3$Volume[ct]   <- TypeData3$Loose[3]+TypeData3$Loose[6]
  Dataset3$Fraction[ct] <- Dataset3$Volume[ct]/sum(TypeData3$Loose)*100
  Dataset3$Clot[ct]     <- file
  ct <- ct + 1
  Dataset3$Type[ct] <- "Loosely Adherent"
  Dataset3$Location[ct] <- "Cavity"
  Dataset3$Volume[ct]   <- TypeData3$Loose[2]+TypeData3$Loose[5]
  Dataset3$Fraction[ct] <- Dataset3$Volume[ct]/sum(TypeData3$Loose)*100
  Dataset3$Clot[ct]     <- file
  ct <- ct + 1 
  Dataset3$Type[ct] <- "Degranulated"
  Dataset3$Location[ct] <- "Extravascular"
  Dataset3$Volume[ct]   <- TypeData3$Degran[1]+TypeData3$Degran[4]
  Dataset3$Fraction[ct] <- Dataset3$Volume[ct]/sum(TypeData3$Degran)*100
  Dataset3$Clot[ct]     <- file
  ct <- ct + 1
  Dataset3$Type[ct] <- "Degranulated"
  Dataset3$Location[ct] <- "Intravascular"
  Dataset3$Volume[ct]   <- TypeData3$Degran[3]+TypeData3$Degran[6]
  Dataset3$Fraction[ct] <- Dataset3$Volume[ct]/sum(TypeData3$Degran)*100
  Dataset3$Clot[ct]     <- file
  ct <- ct + 1
  Dataset3$Type[ct] <- "Degranulated"
  Dataset3$Location[ct] <- "Cavity"
  Dataset3$Volume[ct]   <- TypeData3$Degran[2]+TypeData3$Degran[5]
  Dataset3$Fraction[ct] <- Dataset3$Volume[ct]/sum(TypeData3$Degran)*100
  Dataset3$Clot[ct]     <- file
  ct <- ct + 1
}
```

### Creating the Figures
In the next code chunk, we create the actual figures with ```ggplot2```. 

```{r}
########################################################
### FIGURE 1A: Thrombus, Platelet, and Vault Volumes ###
########################################################

# Preprocess Data
Data1A <- DataSet1[c("Total","Time")]
Data1A$Vaults <- Vaults/1e9
Data1A$Platelets <- Data1A$Total/1e9
Data1A$Total <- (Data1A$Total+Vaults)/1e9
names(Data1A)[names(Data1A) == 'Total'] <- 'Thrombus'
Data1A <- melt(Data1A, id=c("Time"))
Data1A$Time <- as.numeric(Data1A$Time)
Data1A <- Data1A %>% 
  dplyr::rename(
    Volume = value,
    Measurement = variable
  )
Data1A$Measurement <- factor(Data1A$Measurement,levels = c("Thrombus","Platelets","Vaults"))
Sum1A <- summarySE(Data1A, measurevar=("Volume"), groupvars=c("Time","Measurement"))

# Generate Plot
Figure_1A <- ggplot(Data1A, aes(x=Time, y=Volume, color=Measurement)) + theme_bw() + 
  
  # Create title and axes labels
  ggtitle('Thrombus Volume') + 
  xlab('Post-Puncture Time (Minutes)') + 
  ylab(expression(paste("Volume (", mm^3,")"))) + 
  theme(plot.title = element_text(size = Title.Size, face = "bold", hjust=Title.hjust)) +
  theme(axis.title.x = element_text(size = x.Element)) +
  theme(axis.title.y = element_text(size = y.Element)) +
  theme(axis.text.x = element_text(face="bold", size=x.Text)) +
  theme(axis.text.y = element_text(face="bold", size=y.Text)) +
  theme(legend.position = Legend.Location) +
  theme(legend.title = element_text(size=Legend.Size, face="bold")) +
  theme(legend.text = element_text(size=Legend.Size)) +
  theme(legend.background = element_rect(fill = alpha("white", 0))) +
  scale_y_continuous(breaks = seq(0, 0.008, len = 9)) +
  scale_color_manual(labels = c("Thrombus", "Platelets","Vaults"), values = c("#88498F","#3BF4FB","#4A4328")) +
  
  # Scatterplot of data points
  geom_point(
    size=Point.Size, 
    aes(shape=Measurement),
    alpha=Point.Alpha,
    position=position_jitter(w=Point.Jitter, h = 0)) +
  
  # Standard deviation error bars
  geom_errorbar(
    data=Sum1A, 
    aes(ymin=Volume-sd, ymax=Volume+sd),
    width = Error.Width,
    size = Error.Size) +
  
  # Mean bar
  stat_summary(
    fun = mean, 
    geom = "errorbar", 
    aes(ymax = ..y.., ymin = ..y..), 
    position = position_dodge(width = 0.0), 
    width = Mean.Width,
    size = Mean.Size) +
  
  # Lines from the mean
  geom_line(data=Sum1A, aes (x = as.numeric(Time),y=Volume),
            linetype = "solid",
            size = Line.Size)

ggsave(plot = Figure_1A, width = Single.Width, height = Single.Height, dpi = 300, filename = "Figure1A.png")



#############################################
### FIGURE 1B: Platelet Type Distribution ###
#############################################


# Preprocess Data
Data1B <- DataSet2[c(2,3,4,5)]
Data1B$Tight <- Data1B$Tight/1e9
Data1B$Loose <- Data1B$Loose/1e9
Data1B$Degranulated <- Data1B$Degranulated/1e9
Data1B <- gather(Data1B,Type,Volume,c("Tight","Loose","Degranulated"))
Sum1B <- summarySE(Data1B, measurevar="Volume", groupvars=c("Time","Type"))

# Generate Plot
Figure_1B <- ggplot(Data1B, aes(x=Time, y=Volume, color=Type)) + theme_bw() +
  
  # Create title and axes labels
  ggtitle('Proportion of Platelet Type') + 
  xlab('Post-Puncture Time (Minutes)') + 
  ylab(expression(paste("Volume (", mm^3,")"))) + 
  theme(plot.title = element_text(size = Title.Size, face = "bold", hjust=Title.hjust)) +
  theme(axis.title.x = element_text(size = x.Element)) +
  theme(axis.title.y = element_text(size = y.Element)) +
  theme(axis.text.x = element_text(face="bold", size=x.Text)) +
  theme(axis.text.y = element_text(face="bold", size=y.Text)) +
  theme(legend.position = Legend.Location) +
  theme(legend.title = element_text(size=Legend.Size, face="bold")) +
  theme(legend.text = element_text(size=Legend.Size)) +
  theme(legend.background = element_rect(fill = alpha("white", 0))) +
  scale_y_continuous(breaks = seq(0, 0.005, len = 6)) +
  scale_color_manual(labels = c("Degranulated", "Loose", "Tight"), values = c("#F37A27", "#E1DB12", "#2FC80A")) +
  
  # Scatterplot of data points
  geom_point(
    size=Point.Size, 
    aes(shape=Type),
    alpha=Point.Alpha,
    position=position_jitter(w=Point.Jitter, h = 0)) +
  
  # Standard deviation error bars
  geom_errorbar(
    data=Sum1B, 
    aes(ymin=Volume-sd, ymax=Volume+sd),
    width = Error.Width,
    size = Error.Size) +
  
  # Mean bar
  stat_summary(
    fun = mean, 
    geom = "errorbar", 
    aes(ymax = ..y.., ymin = ..y..), 
    position = position_dodge(width = 0.0), 
    width = Mean.Width,
    size = Mean.Size) +
  
  # Lines from the mean
  geom_line(data=Sum1B, aes (x = as.numeric(Time),y=Volume),
            linetype = "solid",
            size = Line.Size)

ggsave(plot = Figure_1B, width = Single.Width, height = Single.Height, dpi = 300, filename = "Figure1B.png")



#######################################################################
### FIGURE 1C: Extravascular vs Intravascular Platelet Distribution ###
#######################################################################

# Preprocess Data
Data1C <- DataSet1[c("Extravascular","Intravascular","Time")]
Data1C$Extravascular <- Data1C$Extravascular/1e9
Data1C$Intravascular <- Data1C$Intravascular/1e9
Data1C <- gather(Data1C,Type,Volume,c("Extravascular","Intravascular"))
Sum1C <- summarySE(Data1C, measurevar="Volume", groupvars=c("Time","Type"))

# Generate Plot
Figure_1C <- ggplot(Data1C, aes(x=Time, y=Volume, color=Type)) + theme_bw() +
  
  # Create title and axes labels
  ggtitle('Extravascular vs Intravascular Distribution') + 
  xlab('Post-Puncture Time (Minutes)') + 
  ylab(expression(paste("Volume (", mm^3,")"))) + 
  theme(plot.title = element_text(size = Title.Size, face = "bold", hjust=Title.hjust)) +
  theme(axis.title.x = element_text(size = x.Element)) +
  theme(axis.title.y = element_text(size = y.Element)) +
  theme(axis.text.x = element_text(face="bold", size=x.Text)) +
  theme(axis.text.y = element_text(face="bold", size=y.Text)) +
  theme(legend.position = Legend.Location) +
  theme(legend.title = element_text(size=Legend.Size, face="bold")) +
  theme(legend.text = element_text(size=Legend.Size)) +
  theme(legend.background = element_rect(fill = alpha("white", 0))) +
  scale_y_continuous(breaks = seq(0, 0.005, len = 6)) +
  scale_color_manual(labels = c("Extravascular", "Intravascular"), values = c("red","blue")) +
  
  # Scatterplot of data points
  geom_point(
    size=Point.Size, 
    aes(shape=Type),
    alpha=Point.Alpha,
    position=position_jitter(w=Point.Jitter, h = 0)) +
  
  # Standard deviation error bars
  geom_errorbar(
    data=Sum1C, 
    aes(ymin=Volume-sd, ymax=Volume+sd),
    width = Error.Width,
    size = Error.Size) +
  
  # Mean bars
  stat_summary(
    fun = mean, 
    geom = "errorbar", 
    aes(ymax = ..y.., ymin = ..y..), 
    position = position_dodge(width = 0.0), 
    width = Mean.Width,
    size = Mean.Size) +
  
  # Lines from the mean
  geom_line(data=Sum1C, aes (x = as.numeric(Time),y=Volume),
            linetype = "solid",
            size = Line.Size)

ggsave(plot = Figure_1C, width = Single.Width, height = Single.Height, dpi = 300, filename = "Figure1C.png")



#################################################################
### FIGURE 2: Platelet Distribution by Time and Area per Type ###
#################################################################

# Preprocess Data
Dataset3$Location <- factor(Dataset3$Location,levels = c("Extravascular","Cavity","Intravascular"))
Dataset3$Clot <- factor(Dataset3$Clot,levels = files)
Dataset3$Volume <- Dataset3$Volume/1e9
Degran.df <- Dataset3[ which(Dataset3$Type=='Degranulated'), ]
Loose.df <- Dataset3[ which(Dataset3$Type=='Loosely Adherent'), ]
Tight.df <- Dataset3[ which(Dataset3$Type=='Tightly Adherent'), ]
Degran.Summary <- summarySE(Degran.df, measurevar="Fraction", groupvars=c("Time","Location"))
Loose.Summary <- summarySE(Loose.df, measurevar="Fraction", groupvars=c("Time","Location"))
Tight.Summary <- summarySE(Tight.df, measurevar="Fraction", groupvars=c("Time","Location"))

# Figure 2A: Degranulated Platelets
Figure_2A <- ggplot(Degran.df, aes(x=Time, y=Fraction, color=Location)) + theme_bw() +
  
  # Create title and axes labels
  ggtitle('Degranulated Platelet Distribution') + 
  xlab('Post-Puncture Time (Minutes)') + 
  ylab('Relative Volume (%)') + 
  theme(plot.title = element_text(size = Title.Size, face = "bold", hjust=Title.hjust)) +
  theme(axis.title.x = element_text(size = x.Element)) +
  theme(axis.title.y = element_text(size = y.Element)) +
  theme(axis.text.x = element_text(face="bold", size=x.Text)) +
  theme(axis.text.y = element_text(face="bold", size=y.Text)) +
  theme(legend.position = Legend.Location2) +
  theme(legend.title = element_text(size=Legend.Size, face="bold")) +
  theme(legend.text = element_text(size=Legend.Size)) +
  theme(legend.background = element_rect(fill = alpha("white", 0))) +
  scale_y_continuous(breaks = seq(0, 100, len = 6)) +
  scale_color_manual(labels = c("Extravascular", "Cavity", "Intravascular"), values = c("#AD343E", "#000000", "#59C9A9")) +
  
  # Scatterplot of data points
  geom_point(
    size=Point.Size, 
    aes(shape=Location),
    alpha=Point.Alpha,
    position=position_jitter(w=Point.Jitter, h = 0)) +
  
  # Standard deviation error bars
  geom_errorbar(
    data=Degran.Summary, 
    aes(ymin=Fraction-sd, ymax=Fraction+sd),
    width = Error.Width,
    size = Error.Size) +
  
  # Mean bar
  stat_summary(
    fun = mean, 
    geom = "errorbar", 
    aes(ymax = ..y.., ymin = ..y..), 
    position = position_dodge(width = 0.0), 
    width = Mean.Width,
    size = Mean.Size) +
  
  # Lines from the mean
  geom_line(data=Degran.Summary, aes (x = as.numeric(Time),y=Fraction),
            linetype = "dashed",
            size = Line.Size)

ggsave(plot = Figure_2A, width = Single.Width, height = Single.Height, dpi = 300, filename = "Figure2A.png")


# Figure 2B: Loosely adherent platelets
Figure_2B <- ggplot(Loose.df, aes(x=Time, y=Fraction, color=Location)) + theme_bw() +
  
  # Create title and axes labels
  ggtitle('Loosely Adherent Platelet Distribution') + 
  xlab('Post-Puncture Time (Minutes)') + 
  ylab('Relative Volume (%)') + 
  theme(plot.title = element_text(size = Title.Size, face = "bold", hjust=Title.hjust)) +
  theme(axis.title.x = element_text(size = x.Element)) +
  theme(axis.title.y = element_text(size = y.Element)) +
  theme(axis.text.x = element_text(face="bold", size=x.Text)) +
  theme(axis.text.y = element_text(face="bold", size=y.Text)) +
  theme(legend.position = Legend.Location2) +
  theme(legend.title = element_text(size=Legend.Size, face="bold")) +
  theme(legend.text = element_text(size=Legend.Size)) +
  theme(legend.background = element_rect(fill = alpha("white", 0))) +
  scale_y_continuous(breaks = seq(0, 100, len = 6)) +
  scale_color_manual(labels = c("Extravascular", "Cavity", "Intravascular"), values = c("#AD343E", "#000000", "#59C9A9")) +
  
  # Scatterplot of data points
  geom_point(
    size=Point.Size, 
    aes(shape=Location),
    alpha=Point.Alpha,
    position=position_jitter(w=Point.Jitter, h = 0)) +
  
  # Standard deviation error bars
  geom_errorbar(
    data=Loose.Summary, 
    aes(ymin=Fraction-sd, ymax=Fraction+sd),
    width = Error.Width,
    size = Error.Size) +
  
  # Mean bar
  stat_summary(
    fun = mean, 
    geom = "errorbar", 
    aes(ymax = ..y.., ymin = ..y..), 
    position = position_dodge(width = 0.0), 
    width = Mean.Width,
    size = Mean.Size) +
  
  # Lines from the mean
  geom_line(data=Loose.Summary, aes (x = as.numeric(Time),y=Fraction),
            linetype = "dashed",
            size = Line.Size)

ggsave(plot = Figure_2B, width = Single.Width, height = Single.Height, dpi = 300, filename = "Figure2B.png")


# Figure 2C: Tightly adherent platelets
Figure_2C <- ggplot(Tight.df, aes(x=Time, y=Fraction, color=Location)) + theme_bw() +
  
  # Create title and axes labels
  ggtitle('Tightly Adherent Platelet Distribution') + 
  xlab('Post-Puncture Time (Minutes)') + 
  ylab('Relative Volume (%)') + 
  theme(plot.title = element_text(size = Title.Size, face = "bold", hjust=Title.hjust)) +
  theme(axis.title.x = element_text(size = x.Element)) +
  theme(axis.title.y = element_text(size = y.Element)) +
  theme(axis.text.x = element_text(face="bold", size=x.Text)) +
  theme(axis.text.y = element_text(face="bold", size=y.Text)) +
  theme(legend.position = Legend.Location2) +
  theme(legend.title = element_text(size=Legend.Size, face="bold")) +
  theme(legend.text = element_text(size=Legend.Size)) +
  theme(legend.background = element_rect(fill = alpha("white", 0))) +
  scale_y_continuous(breaks = seq(0, 100, len = 6)) +
  scale_color_manual(labels = c("Extravascular", "Cavity", "Intravascular"), values = c("#AD343E", "#000000", "#59C9A9")) +
  
  # Scatterplot of data points
  geom_point(
    size=Point.Size, 
    aes(shape=Location),
    alpha=Point.Alpha,
    position=position_jitter(w=Point.Jitter, h = 0)) +
  
  # Standard deviation error bars
  geom_errorbar(
    data=Tight.Summary, 
    aes(ymin=Fraction-sd, ymax=Fraction+sd),
    width = Error.Width,
    size = Error.Size) +
  
  # Mean bar
  stat_summary(
    fun = mean, 
    geom = "errorbar", 
    aes(ymax = ..y.., ymin = ..y..), 
    position = position_dodge(width = 0.0), 
    width = Mean.Width,
    size = Mean.Size) +
  
  # Lines from the mean
  geom_line(data=Tight.Summary, aes (x = as.numeric(Time),y=Fraction),
            linetype = "dashed",
            size = Line.Size)

ggsave(plot = Figure_2C, width = Single.Width, height = Single.Height, dpi = 300, filename = "Figure2C.png")


#################################################
### COMPILE SUBFIGURES INTO ONE SINGLE FIGURE ###
#################################################

g <- arrangeGrob(Figure_1A, Figure_2A, Figure_1B, Figure_2B, Figure_1C, Figure_2C, 
                 ncol = 2, nrow = 3, padding = 5) 

ggsave(file="All_Figures.png", g, width = Single.Width*2, height = Single.Height*3, dpi=300)
```
