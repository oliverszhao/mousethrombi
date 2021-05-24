#################
### LIBRARIES ###
#################
library(ggplot2)
library(mosaic)
library(tidyverse)
library(Rmisc)
library(tidyr)
library(plyr)
library(reshape2)
library(gridExtra)


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


#########################
### FIGURE FORMATTING ###
#########################

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

# Export dataset as CSV
write.csv(DataSet1,'Dataset1.csv')


###########################
###   CREATE DATASET 2  ###
###########################
###    FOR FIGURES 1B   ###
###########################
# Export dataset as CSV
write.csv(DataSet1,'Dataset1.csv')
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

# Export dataset as CSV
write.csv(DataSet2,'Dataset2.csv')

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

# Export dataset as CSV
write.csv(Dataset3,'Dataset3.csv')


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

# Export Data for Figure 1 as CSV
write.csv(Data1A,'Figure1A.csv')

# Statistical test
Thrombus.1Min = Data1A$Volume[Data1A$Time==1 & Data1A$Measurement=='Thrombus']
Thrombus.5Min = Data1A$Volume[Data1A$Time==5 & Data1A$Measurement=='Thrombus']
Platelets.1Min = Data1A$Volume[Data1A$Time==1 & Data1A$Measurement=='Platelets']
Platelets.5Min = Data1A$Volume[Data1A$Time==5 & Data1A$Measurement=='Platelets']

wilcox.test(Thrombus.1Min,Thrombus.5Min)
wilcox.test(Platelets.1Min,Platelets.5Min)

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

# Export Data for Figure 1 as CSV
write.csv(Data1B,'Figure1B.csv')

# Statistical test
Tight.1Min = Data1B$Volume[Data1B$Time==1 & Data1B$Type=='Tight']
Tight.5Min = Data1B$Volume[Data1B$Time==5 & Data1B$Type=='Tight']
Loose.1Min = Data1B$Volume[Data1B$Time==1 & Data1B$Type=='Loose']
Loose.5Min = Data1B$Volume[Data1B$Time==5 & Data1B$Type=='Loose']
Degran.1Min = Data1B$Volume[Data1B$Time==1 & Data1B$Type=='Degranulated']
Degran.5Min = Data1B$Volume[Data1B$Time==5 & Data1B$Type=='Degranulated']

wilcox.test(Tight.1Min,Tight.5Min)
wilcox.test(Loose.1Min,Loose.5Min)
wilcox.test(Degran.1Min,Degran.5Min)

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

# Export Data for Figure 1 as CSV
write.csv(Data1C,'Figure1C.csv')

# Statistical test
Extra.1Min = Data1C$Volume[Data1C$Time==1 & Data1C$Type=='Extravascular']
Extra.5Min = Data1C$Volume[Data1C$Time==5 & Data1C$Type=='Extravascular']
Intra.1Min = Data1C$Volume[Data1C$Time==1 & Data1C$Type=='Intravascular']
Intra.5Min = Data1C$Volume[Data1C$Time==5 & Data1C$Type=='Intravascular']

wilcox.test(Extra.1Min,Extra.5Min)
wilcox.test(Intra.1Min,Intra.5Min)

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

### Figure 2A: Degranulated Platelets
# Export Data for Figure 2A as CSV
write.csv(Degran.df,'Figure2A.csv')

# Statistical test for Degranulated Platelets
Degran.Extra.1Min = Dataset3$Volume[Dataset3$Time==1 & Dataset3$Location=='Extravascular' & Dataset3$Type=='Degranulated']
Degran.Extra.5Min = Dataset3$Volume[Dataset3$Time==5 & Dataset3$Location=='Extravascular' & Dataset3$Type=='Degranulated']
Degran.Intra.1Min = Dataset3$Volume[Dataset3$Time==1 & Dataset3$Location=='Intravascular' & Dataset3$Type=='Degranulated']
Degran.Intra.5Min = Dataset3$Volume[Dataset3$Time==5 & Dataset3$Location=='Intravascular' & Dataset3$Type=='Degranulated']
Degran.Cavity.1Min = Dataset3$Volume[Dataset3$Time==1 & Dataset3$Location=='Cavity' & Dataset3$Type=='Degranulated']
Degran.Cavity.5Min = Dataset3$Volume[Dataset3$Time==5 & Dataset3$Location=='Cavity' & Dataset3$Type=='Degranulated']

wilcox.test(Degran.Extra.1Min,Degran.Extra.5Min)
wilcox.test(Degran.Intra.1Min,Degran.Intra.5Min)
wilcox.test(Degran.Cavity.1Min,Degran.Cavity.5Min)

# Generate figure
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

### Figure 2B: Loosely adherent platelets
# Export Data for Figure 2B as CSV
write.csv(Loose.df,'Figure2B.csv')

# Statistical test for Loosely Adherent Platelets
Loose.Extra.1Min = Dataset3$Volume[Dataset3$Time==1 & Dataset3$Location=='Extravascular' & Dataset3$Type=='Loosely Adherent']
Loose.Extra.5Min = Dataset3$Volume[Dataset3$Time==5 & Dataset3$Location=='Extravascular' & Dataset3$Type=='Loosely Adherent']
Loose.Intra.1Min = Dataset3$Volume[Dataset3$Time==1 & Dataset3$Location=='Intravascular' & Dataset3$Type=='Loosely Adherent']
Loose.Intra.5Min = Dataset3$Volume[Dataset3$Time==5 & Dataset3$Location=='Intravascular' & Dataset3$Type=='Loosely Adherent']
Loose.Cavity.1Min = Dataset3$Volume[Dataset3$Time==1 & Dataset3$Location=='Cavity' & Dataset3$Type=='Loosely Adherent']
Loose.Cavity.5Min = Dataset3$Volume[Dataset3$Time==5 & Dataset3$Location=='Cavity' & Dataset3$Type=='Loosely Adherent']

wilcox.test(Loose.Extra.1Min,Loose.Extra.5Min)
wilcox.test(Loose.Intra.1Min,Loose.Intra.5Min)
wilcox.test(Loose.Cavity.1Min,Loose.Cavity.5Min)

# Generat figure
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

### Figure 2C: Tightly adherent platelets
# Export Data for Figure 2C as CSV
write.csv(Tight.df,'Figure2C.csv')

# Statistical test for Tightly Adherent Platelets
Tight.Extra.1Min = Dataset3$Volume[Dataset3$Time==1 & Dataset3$Location=='Extravascular' & Dataset3$Type=='Tightly Adherent']
Tight.Extra.5Min = Dataset3$Volume[Dataset3$Time==5 & Dataset3$Location=='Extravascular' & Dataset3$Type=='Tightly Adherent']
Tight.Intra.1Min = Dataset3$Volume[Dataset3$Time==1 & Dataset3$Location=='Intravascular' & Dataset3$Type=='Tightly Adherent']
Tight.Intra.5Min = Dataset3$Volume[Dataset3$Time==5 & Dataset3$Location=='Intravascular' & Dataset3$Type=='Tightly Adherent']
Tight.Cavity.1Min = Dataset3$Volume[Dataset3$Time==1 & Dataset3$Location=='Cavity' & Dataset3$Type=='Tightly Adherent']
Tight.Cavity.5Min = Dataset3$Volume[Dataset3$Time==5 & Dataset3$Location=='Cavity' & Dataset3$Type=='Tightly Adherent']

wilcox.test(Tight.Extra.1Min,Tight.Extra.5Min)
wilcox.test(Tight.Intra.1Min,Tight.Intra.5Min)
wilcox.test(Tight.Cavity.1Min,Tight.Cavity.5Min)

# Generate figure
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