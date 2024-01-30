setwd("/Users/admin/github/otolith")

#################
library(tidyverse)

#### Dateset 1 ####
age <- read.csv("Age.csv")

### Summary of data ###
age0 <- subset(age,!is.na(age$SrCa)) # Omit NA row
age0 %>%
  group_by(Point, Month, Sex) %>%
  tally()

#### Dateset 2 ####
### 20230625 Update ###

## References of Sr/Ca ##
sea <- NA
brack <- 4.4
fresh <- 3.2

## References of Sr/Ca ##
ref_d34S <- 17.0

## Color settings ##
col_sea <- "#350c0c"
col_brack <- "#a22526"
col_fresh <- "#fa6c6d"

#### Dateset 3 ####
#srca <- read.csv("yr159.csv")


#########################
#### Make Data Table ####
#########################

# Omit NA row #
age1 <- subset(age,!is.na(age$SrCa)) 

# Select time periods >>> 
# 2m or 3mm or 4m or 6m or 1y #

setting_periods <- "6m"

if (setting_periods == "2m"){
  col_year <- age1 %>% select(X2months_um)
} else if (setting_periods == "3mm") {
  col_year <- age1 %>% select(X3months_um)
} else if (setting_periods == "4m") {
  col_year <- age1 %>% select(X4months_um)
} else if (setting_periods == "6m") {
  col_year <- age1 %>% select(X6months_um)
} else {
  col_year <- age1 %>% select(X1year_um)
}

col_year


# Calculation 1 #
SrCa_prof <- c()
ID <- c()

for (i in 1:nrow(age1)) {
  
  file.name <- age1$File.name[i]  # yr159.csv~yr529.csv
  
  #### Dateset 3 ####
  srca <- read.csv(file.name)
  
  ####
  id_fish <- srca %>%
    dplyr::select(1,2,15) # 個体番号, 測定No, Sr/Ca (wt ratio x1000)
  
  colnames(id_fish) <- c("ID","Distance","Sr.Ca")
  outer <- max(id_fish$Distance)
  
  ####
  #col_year <- age1 %>% select(periods)
  point_per <- round(as.numeric(col_year[i,]), digits = 0)
  
  dist <- id_fish %>%
    filter(id_fish$Distance >= outer - point_per)
  
  SrCa_prof <- rbind(SrCa_prof, dist)
  #ID <- c(ID, id_fish$ID[2])
  
}

# Fin. Calculation 1 #
########

################################################
#### Judge the results by delta-34S & Sr/Ca ####
################################################

# Fin. Calculation 2 #

judge_Sr.Ca <- c()
judge_d34S <- c()
counts_Sr.Ca <- c()
sum_counts <- c()

THLD <- 3  # Ca.Sr < 4.4 x 10^-3 がX回のとき

for (i in 1:nrow(age1)) {
  
  id <- unique(SrCa_prof$ID)[i]
  
  fish <- SrCa_prof %>%
    dplyr::filter(ID == id)
  fish <- subset(fish,!is.na(fish$Sr.Ca))
  
  for (j in 1:nrow(fish)) {
    
    if (fish$Sr.Ca[j] < brack){
      counts_temp <- 1
    } else {
      counts_temp <- 0
    }
    
    counts_Sr.Ca <- c(counts_Sr.Ca, counts_temp)
    counts_Sr.Ca <- sum(counts_Sr.Ca)
    
  }
  
  sum_counts <- c(sum_counts, counts_Sr.Ca)
  
  if (counts_Sr.Ca < THLD){
    judge_Sr.Ca_temp <- "Sea"
  } else {
    judge_Sr.Ca_temp <- "River"
  }
  
  judge_Sr.Ca <- c(judge_Sr.Ca, judge_Sr.Ca_temp)
  
  if (age1$d34S.d32S[i] > ref_d34S){
    judge_d34S_temp <- "Sea"
  } else {
    judge_d34S_temp <- "River"
  }
  
  judge_d34S <- c(judge_d34S,judge_d34S_temp)
  
  counts_Sr.Ca <- c()
  
}

judge <- cbind(age1$ID, judge_Sr.Ca, judge_d34S)
#print(judge)

# Fin. Calculation 2 #

###################################################
#### Compare the estimates of delta-34S & Sr/Ca ####
###################################################

# Calculation 3 #

match <- c()
precision <- c()

for (k in 1:nrow(judge)) {
  
  match_temp <- charmatch(judge[k,2], judge[k,3])
  match <- c(match, match_temp)
  
}
  

precision1 <- sum(na.omit(match))/nrow(judge) * 100
print(precision1)

precision <- c(precision,precision1)

judge

# as.numeric(as.factor(judge[,2:3]))  

# Fin. Calculation 3 #

####################################################
#### Compare the estimates of delta-34S & Sr/Ca ####
####################################################

# Calculation 4 #

group <- c()
judge_mer <- c()

for (i in 1:nrow(judge)) {
  
  group_temp <- paste(judge[i,2], judge[i,3], sep = "-")
  group <- c(group, group_temp)
  
  id <- unique(SrCa_prof$ID)[i]
  
  SrCa_prof_1 <- SrCa_prof %>%
    dplyr::filter(ID == id)
  
  judge_mer_temp <- rep(group_temp, nrow(SrCa_prof_1))
  judge_mer <- c(judge_mer, judge_mer_temp)
  
}

SrCa_prof_fig <- cbind(SrCa_prof, judge_mer)

# Fin. Calculation 4 #

############################################################
#### Plots of Sr/Ca profiles with judge in individuals  ####
############################################################

## Plots 1 ##
library("gridExtra")
library(ggeffects)
library(ggrepel)

cols1 <- c(
  "#F13B47", # River-River 
  "#FABBBD", # River-Sea
  "#BDC9FA", # Sea-River
  "#497CF6"  # Sea-Sea
)

# labs <- as.character(as.factor(age1$ID_Fig))

a <- ggplot(SrCa_prof_fig, aes(x = Distance, y = Sr.Ca, color = judge_mer)) + 
  geom_point(size = 1) +
  geom_hline(yintercept = brack) +
  scale_color_manual(values = cols1) 

a + facet_wrap(~ID, scales = "free", ncol = 4) +
  ylim(0, 20) +
  theme_classic() +
  theme(text = element_text(size = 10),
        panel.grid=element_blank(),
        strip.background = element_blank())

#ggsave("6months_20240129.png",width=8,height=12,dpi=300)

# Fin. Plots 1 #

############################################################
#### Title  ####
############################################################

## Plots 2 ##
age_x <- cbind(age1, sum_counts)
age_m <- age_x[1:32,]
age_r <- age_x[33:41,]

## Marine ##
#png("Fig_Marine_plot.png", width = 1200, height = 1200, res=200)
plot(age_m$sum_counts, age_m$d34S.d32S,
     xlim = c(0,100),
     ylim = c(5,21),
     col = "#350c0c",
     pch = 1,
     xlab = "",
     ylab = "",
     las = 1
)

abline(h = 17, col = "#fa6c6d", lty = 2)
abline(v = 3, col = "#fa6c6d", lty = 2)

dev.off()

## River  ##
#png("Fig_River_plot.png", width = 1200, height = 1200, res=200)
plot(age_r$sum_counts, age_r$d34S.d32S,
     xlim = c(0,100),
     ylim = c(5,21),
     col = "#a22526",
     pch = 16,
     xlab = "Count of Sr/Ca < 4.4 x 10^-3",
     ylab = "delta34S(‰)",
     las = 1
)

abline(h = 17, col = "#fa6c6d", lty = 2)
abline(v = 3, col = "#fa6c6d", lty = 2)

dev.off()

## All ##
#png("Fig_All_Marine+River_plot.png", width = 1200, height = 1200, res=200)

plot(age_m$sum_counts, age_m$d34S.d32S,
     xlim = c(0,100),
     ylim = c(5,21),
     col = "#350c0c",
     pch = 1,
     xlab = "",
     ylab = "",
     las = 1
)

par(new=T)

plot(age_r$sum_counts, age_r$d34S.d32S,
     xlim = c(0,100),
     ylim = c(5,21),
     col = "#a22526",
     pch = 16,
     xlab = "Count of Sr/Ca < 4.4 x 10^-3",
     ylab = "delta34S(‰)",
     las = 1
)

abline(h = 17, col = "#fa6c6d", lty = 2)
abline(v = 3, col = "#fa6c6d", lty = 2)

dev.off()

# Fin. Plots 2 #

############################################################
#### Precision  ####
############################################################

## Data sets settings ##
# Select data sets >>> All- age1, Sea- age1[1:32,], River- age1[33:41,] #

setting_dataset <- "All"

if (setting_dataset == "All"){
  data <- age1
} else if (setting_dataset == "Sea") {
  data <- age1[1:32,]
} else {
  data <- age1[33:41,]
}

data

# Select time periods >>> 
# 2m or 3mm or 4m or 6m or 1y #

setting_periods <- "6m"

if (setting_periods == "2m"){
  col_year <- data %>% select(X2months_um)
} else if (setting_periods == "3m") {
  col_year <- data %>% select(X3months_um)
} else if (setting_periods == "4m") {
  col_year <- data %>% select(X4months_um)
} else if (setting_periods == "6m") {
  col_year <- data %>% select(X6months_um)
} else {
  col_year <- data %>% select(X1year_um)
}

col_year

# Calculation 5 #
counts_Sr.Ca <- c()
precision <- c()

for (h in 1:100) {
  
  judge_Sr.Ca <- c()
  judge_d34S <- c()
  counts_Sr.Ca <- c()
  
  THLD <- h # Ca.Sr < 1 がX回のとき
  
  for (i in 1:nrow(data)) {
    
    # Calculation 1 #
    file.name <- data$File.name[i]  # yr159.csv~yr529.csv
    
    srca <- read.csv(file.name)
    
    ####
    id_fish <- srca %>%
      dplyr::select(1,2,15) # 個体番号, 測定No, Sr/Ca (wt ratio x1000)
    
    colnames(id_fish) <- c("ID","Distance","Sr.Ca")
    outer <- max(id_fish$Distance)
    
    ####
    point_per <- round(as.numeric(col_year[i,]), digits = 0)
    
    dist <- id_fish %>%
      filter(id_fish$Distance >= outer - point_per)
 
    dist <- subset(dist,!is.na(dist$Sr.Ca))
    
    # Calculation 2 #
    
    #Sr/Ca
      for (j in 1:nrow(dist)) {
        
        if (dist$Sr.Ca[j] < brack){
          counts_temp <- 1
        } else {
          counts_temp <- 0
        }
        counts_Sr.Ca <- c(counts_Sr.Ca, counts_temp)
        counts_Sr.Ca <- sum(counts_Sr.Ca)
      }
      
      if (counts_Sr.Ca < THLD){
        judge_Sr.Ca_temp <- "Sea"
      } else {
        judge_Sr.Ca_temp <- "River"
      }
      
      judge_Sr.Ca <- c(judge_Sr.Ca, judge_Sr.Ca_temp)
      
      #delta34S
      if (data$d34S.d32S[i] > ref_d34S){
        judge_d34S_temp <- "Sea"
      } else {
        judge_d34S_temp <- "River"
      }
      
      judge_d34S <- c(judge_d34S,judge_d34S_temp)
      
      counts_Sr.Ca <- c()
    }
    
    judge <- cbind(data$ID, judge_Sr.Ca, judge_d34S)
    
    # Calculation 3 #
    
    match <- c()

    for (k in 1:nrow(judge)) {
      
      match_temp <- charmatch(judge[k,2], judge[k,3])
      match <- c(match, match_temp)
      
    }
    
    precision1 <- (sum(na.omit(match))/nrow(judge)) * 100
    print(precision1)
    
    precision <- c(precision,precision1)
  
}

#jud1 <- as.data.frame(judge)

# Save on object #
#pre_all <- precision
#pre_sea <- precision
pre_riv <- precision

# range of X-axis #
nu <- 20 

#png("Fig_precision_20240129.png", width = 1200, height = 450, res = 150)
par(mfrow = c(1,3))

plot(1:nu,pre_all[1:nu], ylim = c(50,100), las = 1, xlab = "", ylab = "")
abline(v=3, col="#c5344d")

plot(1:nu,pre_sea[1:nu], ylim = c(50,100), las = 1, las = 1, xlab = "", ylab = "")
abline(v=3, col="#c5344d")

plot(1:nu,pre_riv[1:nu], ylim = c(50,100), las = 1, las = 1, xlab = "", ylab = "")
abline(v=3, col="#c5344d")

dev.off()
par(mfrow = c(1,1))

# Fin. Calculation 5 #
