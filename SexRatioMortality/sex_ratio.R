# June 12, 2024 Tara amends Katrina's script to get sex ratio out of NMFS surveys. 
# done separately for spring and fall .

#### Start ####
{
  library(here)
  library(tidyverse)
  library(ggplot2)
  library(viridis)
  library(reshape2)
}


#### Data ####

# NEFSC (F)
# https://www.fisheries.noaa.gov/inport/item/22557 
# last downloaded 2/3/24
# attributes
# https://www.fisheries.noaa.gov/inport/item/33601
# https://www.fisheries.noaa.gov/inport/item/33631

# MADMF (MA)
# Mark Szymanski <mark.szymanski@mass.gov>
# Steve Wilcox <steve.wilcox@mass.gov>
# expanded counts
# last downloaded 2/9/24

# ME/NH (MN)
# https://mainedmr.shinyapps.io/MaineDMR_Trawl_Survey_Portal/
# standardized frequency of each length caught for each species in each tow of the surveys
# last downloaded 5/5/24

#### F FALL ####

# upload and clean ff = federal fall 1963-[2020]-
{
  # Tow data
  ff_tow <- read.csv(here("C:/Users/Tara.Dolan/OneDrive - Commonwealth of Massachusetts/Documents/R-Github/flounder_EDM/data_NMFS_bts", "22560_UNION_FSCS_SVSTA.csv"))
  # remove bad tows = codes that correspond to a representative tow (<2009: SHG <= 136, >=2009: TOGA <= 1324) 
  # Add year
  ff_tow$EST_YEAR <- substr(ff_tow$CRUISE6, 1, 4)
  # Split by years
  ff_tow1 <- ff_tow[ff_tow$EST_YEAR < 2009,]
  ff_tow1 <- ff_tow1[ff_tow1$SHG <= 136,] # select representative tows
  ff_tow2 <- ff_tow[ff_tow$EST_YEAR >= 2009,]
  ff_tow2 <- ff_tow2[ff_tow2$TOGA <=1324,] # select representative tows
  ff_tow <- rbind(ff_tow1, ff_tow2)
  #NAs in stat area, need to be removed
  ff_tow <- ff_tow[!is.na(ff_tow$AREA), ]
  # subset to GOM
  ff_tow <- ff_tow[ff_tow$AREA >= 511 & ff_tow$AREA <= 515, ]
  # columns of interest
  ff_tow <- ff_tow[, c("CRUISE6", "STRATUM", "TOW", "STATION", "AREA", "EST_YEAR")]
  # clean strata numbers
  ff_tow$STRATUM <- sub("^0+", "", ff_tow$STRATUM)
  
  # Length data
  ff_len <- read.csv(here("C:/Users/Tara.Dolan/OneDrive - Commonwealth of Massachusetts/Documents/R-Github/flounder_EDM/NMFS_bts", "22560_UNION_FSCS_SVLEN.csv"))
  # subset to winter flounder only
  ff_len <- ff_len[(ff_len$SVSPP == 106),]
  # clean strata numbers
  ff_len$STRATUM <- sub("^0+", "", ff_len$STRATUM)
  # subset to GOM
  ff_len <- merge(ff_tow, ff_len, by = c("CRUISE6", "STRATUM", "TOW", "STATION"), all = T)
  ff_len <- ff_len[!is.na(ff_len$AREA),]
  # Add year
  ff_len$EST_YEAR <- substr(ff_len$CRUISE6, 1, 4)
  # Replace NA in numbers at length with 0 for tows with no winter flounder
  ff_len$EXPNUMLEN[is.na(ff_len$EXPNUMLEN)] <- 0
  # Split by years and add calibration factor
  ff_len1 <- ff_len[ff_len$EST_YEAR < 2009,]
  ff_len2 <- ff_len[ff_len$EST_YEAR >= 2009,]
  ff_len2$EXPNUMLEN <-  ff_len2$EXPNUMLEN * 2.086
  ff_len <- rbind(ff_len1, ff_len2)
  # columns of interest
  ff_len <- ff_len[, c("CRUISE6", "STRATUM", "TOW", "STATION", "AREA", "EST_YEAR", "LENGTH", "EXPNUMLEN")]
  
  # Biological data
  ff_bio <- read.csv(here("C:/Users/Tara.Dolan/OneDrive - Commonwealth of Massachusetts/Documents/R-Github/flounder_EDM/NMFS_bts", "22560_UNION_FSCS_SVBIO.csv"))
  # subset to winter flounder only
  ff_bio <- ff_bio[(ff_bio$SVSPP == 106),] #mature: & ff_bio$MATURITY != "I"
  # clean strata numbers
  ff_bio$STRATUM <- sub("^0+", "", ff_bio$STRATUM)
  # subset to GOM
  ff_bio <- merge(ff_tow, ff_bio, by = c("CRUISE6", "STRATUM", "TOW", "STATION"), all = T)
  ff_bio <- ff_bio[!is.na(ff_bio$AREA),]
  # Add year
  ff_bio$EST_YEAR <- substr(ff_bio$CRUISE6, 1, 4)
  # Replace NA in sex with 999 for tows with no winter flounder
  ff_bio$SEX[is.na(ff_bio$SEX)] <- 999
}


# Create length bins
{
  # get range for bins
  range(ff_len$LENGTH, na.rm = T)
  # Create 5 cm bins
  ff_len <- ff_len %>%
    mutate(
      length_bin = cut(
        LENGTH,
        breaks = c(seq(0, 74, by = 5), Inf),
        labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74"),
        include.lowest = TRUE,
        right = FALSE  # Ensure right-closed intervals
      )
    )
  
  # get range for bins
  range(ff_bio$LENGTH, na.rm = T)
  # Create 5 cm bins
  ff_bio <- ff_bio %>%
    mutate(
      length_bin = cut(
        LENGTH,
        breaks = c(seq(0, 64, by = 5), Inf),
        labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64"),
        include.lowest = TRUE,
        right = FALSE  # Ensure right-closed intervals
      )
    )
}


{
  # Adjust for uneven sampling by weighting core strata
  # some strata have more tows/km2 than others, some strata were sampled in some years and not others
  
  # [TOTAL_AREA_OF_STRATUM]/[TOTAL_TOWS_BY_STRATA_AND_YEAR]
  ff_tow_bin_strata <- ff_tow %>%
    group_by(EST_YEAR, STRATUM) %>%
    summarise(total = n())
  # clean strata numbers
  ff_tow_bin_strata$STRATUM <- sub("^0+", "", ff_tow_bin_strata$STRATUM)
  
  # Stratum data
  ff_strat <- read.csv(here("C:/Users/Tara.Dolan/OneDrive - Commonwealth of Massachusetts/Documents/R-Github/flounder_EDM/NMFS_bts", "SVDBS_SVMSTRATA.csv"))
  # columns of interest
  ff_strat <- ff_strat[ , c("stratum", "stratum_area")]
  names(ff_strat)[1] <- "STRATUM"
  # merge area of stratum with number of tows (removing stratum outside of GOM)
  ff_tow_bin_strata <- merge(ff_tow_bin_strata, ff_strat, by = "STRATUM", all.x = T)
  # generate expansion factor for length bin counts based on tow coverage for each stratum
  ff_tow_bin_strata$exp_fact <- (ff_tow_bin_strata$stratum_area/ff_tow_bin_strata$total)/100
  ff_tow_bin_strata <- ff_tow_bin_strata[ , c("STRATUM", "EST_YEAR", "exp_fact")]
  
  
  # number per length bin by strata
  ff_length_bin_strata <- ff_len %>%
    group_by(EST_YEAR, STRATUM, length_bin) %>%
    mutate(sum_EXPNUMLEN = sum(EXPNUMLEN, na.rm = TRUE)) %>%
    distinct(EST_YEAR, STRATUM, length_bin, .keep_all = TRUE)
  # remove individual data fields that were aggregated to create bins so not confusing
  ff_length_bin_strata <- ff_length_bin_strata %>%
    select(-LENGTH, -EXPNUMLEN, -STATION, -TOW)
  
  # expanded length bin counts
  # merge
  ff_length_bin_strata <- merge(ff_length_bin_strata, ff_tow_bin_strata, by = c("STRATUM", "EST_YEAR"), all = T)
  # apply expansion factor
  ff_length_bin_strata$EXP_sum_EXPNUMLEN <- ff_length_bin_strata$sum_EXPNUMLEN*ff_length_bin_strata$exp_fact
}  


{
  # Calculate the sex ratio in 5 cm length bins for each year and stratum
  ff_ratio_by_bin <- aggregate(SEX ~ length_bin + EST_YEAR + STRATUM, data = ff_bio, 
                               function(x) sum(x == 2) / (sum(x == 1) + sum(x == 2)), 
                               drop = FALSE)
  
  # Expanded numbers at length by year
  ff_length_bin_strata$EXP_sum_EXPNUMLEN
  
  # Merge sex ratio with numbers at length
  ff_length_bin_count <- left_join(ff_length_bin_strata, ff_ratio_by_bin, by = c("length_bin", "EST_YEAR", "STRATUM"))
  
  # Sex ratio conditioned on expanded numbers at length by year
  # female count
  ff_length_bin_count$f_count <- ff_length_bin_count$EXP_sum_EXPNUMLEN * ff_length_bin_count$SEX
  # male count
  ff_length_bin_count$m_count <- ff_length_bin_count$EXP_sum_EXPNUMLEN - ff_length_bin_count$f_count
  
  #tara adds
  write.csv(ff_length_bin_count,"numbersbysexlengthNMFSFBTS.csv")
  
  # corrected sex ratio by stratum
  ff_ratio_corrected <- ff_length_bin_count %>%
    group_by(EST_YEAR, STRATUM, length_bin) %>%
    mutate(ratio = sum(f_count, na.rm = TRUE) / 
             (sum(f_count, na.rm = TRUE) + sum(m_count, na.rm = TRUE)))
  
  # Sex ratio conditioned on numbers at length by year and length bin
  ff_ratio_corrected2 <- ff_length_bin_count %>%
    group_by(EST_YEAR, length_bin) %>%
    summarise(ratio = sum(f_count, na.rm = TRUE) / 
                (sum(f_count, na.rm = TRUE) + sum(m_count, na.rm = TRUE)))
  
  # Add grouping variable for plotting NA ratios
  # size bins as sampled for all years
  ff_ratio_corrected2$ratio_available <- ifelse(ff_ratio_corrected2$ratio == "NaN", 0, 1)
  
  # Sex ratio conditioned on numbers at length by year and length bin, 30+ cm fish
  ff_ratio_corrected2m30 <- ff_ratio_corrected2[!(ff_ratio_corrected2$length_bin == "0-4" |
                                                    ff_ratio_corrected2$length_bin == "5-9" |
                                                    ff_ratio_corrected2$length_bin == "10-14" |
                                                    ff_ratio_corrected2$length_bin == "15-19" |
                                                    ff_ratio_corrected2$length_bin == "20-24" |
                                                    ff_ratio_corrected2$length_bin == "25-29"), ]
  #Tara adds:
  write.csv(ff_ratio_corrected2m30,"NMFSBTS_CorrectedSexRatioAtLength.csv")
  
  
  # Sex ratio conditioned on numbers at length by year and length bin, <30 cm fish
  ff_ratio_corrected2s30 <- ff_ratio_corrected2[(ff_ratio_corrected2$length_bin == "0-4" |
                                                   ff_ratio_corrected2$length_bin == "5-9" |
                                                   ff_ratio_corrected2$length_bin == "10-14" |
                                                   ff_ratio_corrected2$length_bin == "15-19" |
                                                   ff_ratio_corrected2$length_bin == "20-24" |
                                                   ff_ratio_corrected2$length_bin == "25-29"), ]
  
  # What uncorrected ratio was
  ff_ratio_uncorrected <- ff_bio %>%
    group_by(EST_YEAR) %>%
    summarise(
      f_count = sum(SEX == 2, na.rm = TRUE),
      m_count = sum(SEX == 1, na.rm = TRUE),
      ratio_uncorrected = f_count / (f_count + m_count))

  
  # Reformat for plotting
  ff_ratio <- left_join(ff_ratio_corrected2, ff_ratio_uncorrected, by = "EST_YEAR")
  ff_ratio <- ff_ratio %>% select(EST_YEAR, ratio, ratio_uncorrected)
  # reshape the data frame from wide to long format, keeping the "EST_YEAR" column unchanged
  ff_ratio2 <- melt(ff_ratio, id.vars = "EST_YEAR")
  
  # annual average
  ff_ratio_corrected3 <- ff_ratio_corrected2 %>%
    group_by(EST_YEAR) %>%
    summarise(ratio = mean(ratio, na.rm = T))
  
  ff_ratio_corrected3m30 <- ff_ratio_corrected2m30 %>%
    group_by(EST_YEAR) %>%
    summarise(ratio = mean(ratio, na.rm = T))
  
  ff_ratio_corrected3s30 <- ff_ratio_corrected2s30 %>%
    group_by(EST_YEAR) %>%
    summarise(ratio = mean(ratio, na.rm = T))
  
  ff_ratio_corrected2 <- ff_ratio_corrected2[!is.na(ff_ratio_corrected2$length_bin),]
  
}

# Visualize
# Sampling and ratio across length bins

(plot <- ggplot(ff_ratio_corrected2, aes(as.numeric(EST_YEAR), length_bin, fill = ratio)) +
    geom_tile() +
    labs(title = "Fall BTS NEFSC, Sex ratio by length bin",
         x = "Year",
         y = "Length bin (cm)") +
    scale_x_continuous(breaks = seq(1963, max(ff_ratio_corrected2$EST_YEAR), by = 5),
                       guide = guide_axis(angle = 45)) +
    scale_fill_continuous(name = "Ratio") +
    theme_classic())
ggsave("ff_sampledvnot.png", plot, width = 6, height = 4, dpi = 300)

# Female ratio corrected vs not corrected
(plot <- ggplot(ff_ratio2, aes(x = EST_YEAR, y = value, color = variable)) +
    #geom_point() +
    geom_line(aes(group = variable), stat = "summary") +
    labs(title = "Fall BTS NEFSC",
         x = "Year",
         y = "Sex ratio (F/M)") +
    scale_x_discrete(labels = function(x) ifelse(seq_along(x) %% 5 == 0, x, ""),
                     guide = guide_axis(angle = 45)) +
    scale_color_discrete(name = "") +
    theme_classic())
ggsave("ff_ratio_correctvnot.png", plot, width = 8, height = 4, dpi = 300)

#Corrected female ratio
(plot <- ggplot(ff_ratio_corrected2, aes(x = as.factor(EST_YEAR), y = ratio)) +
    geom_boxplot(alpha = 0.4) +
    #geom_point(data = ff_ratio_corrected3, aes(x = as.factor(EST_YEAR), y = ratio), color = "blue") +
    geom_line(data = ff_ratio_corrected3, aes(x = as.factor(EST_YEAR), y = ratio, group = 1), color = "blue", lwd = 1.5) +
    labs(title = "Fall BTS NEFSC",
         x = "Year",
         y = "Sex ratio (F/M)") +
    scale_x_discrete(labels = function(x) ifelse(seq_along(x) %% 5 == 0, x, ""),
                     guide = guide_axis(angle = 45)) +
    scale_color_discrete(name = "") +
    theme_classic() +
    geom_hline(yintercept = 0.5, color = "red"))
ggsave("ff_combined.png", plot, width = 8, height = 4, dpi = 300)

(plot <- ggplot(ff_ratio_corrected2m30, aes(x = EST_YEAR, y = ratio)) + 
    geom_boxplot() +
    geom_line(data = ff_ratio_corrected3m30, aes(x = as.factor(EST_YEAR), y = ratio, group = 1), color = "blue", lwd = 1.5) +
    labs(title = "Fall BTS NEFSC, 30+ cm",
         x = "Year",
         y = "Sex ratio (F/M)") +
    scale_x_discrete(labels = function(x) ifelse(seq_along(x) %% 5 == 0, x, ""),
                     guide = guide_axis(angle = 45)) +
    scale_color_discrete(name = "") +
    theme_classic() +
    geom_hline(yintercept = 0.5, color = "red"))
ggsave("ff_m30.png", plot, width = 8, height = 4, dpi = 300)

(plot <- ggplot(ff_ratio_corrected2s30, aes(x = EST_YEAR, y = ratio)) + 
    geom_boxplot() +
    geom_line(data = ff_ratio_corrected3s30, aes(x = as.factor(EST_YEAR), y = ratio, group = 1), color = "blue", lwd = 1.5) +
    labs(title = "Fall BTS NEFSC, <30 cm",
         x = "Year",
         y = "Sex ratio (F/M)") +
    scale_x_discrete(labels = function(x) ifelse(seq_along(x) %% 5 == 0, x, ""),
                     guide = guide_axis(angle = 45)) +
    scale_color_discrete(name = "") +
    theme_classic() +
    geom_hline(yintercept = 0.5, color = "red"))
ggsave("ff_s30.png", plot, width = 8, height = 4, dpi = 300)


# Define the number of bins
num_bins <- 11

# Calculate bin width
bin_width <- 1 / num_bins

# Create bins
bin_intervals <- seq(0, 1, by = bin_width)
bin_intervals <- cbind(bin_intervals[-length(bin_intervals)], bin_intervals[-1])

# Convert bins to labels for visualization
bin_labels <- paste0(round(bin_intervals[, 1], 4), "-", round(bin_intervals[, 2], 4))

# Cut the data into bins
ff_ratio_corrected2m30_counts <- ff_ratio_corrected2m30 %>%
  mutate(bin = cut(ratio, breaks = c(0, bin_intervals[, 2]), labels = bin_labels, include.lowest = TRUE)) %>%
  group_by(bin) %>%
  summarize(count = n())

(plot <- ggplot(data = ff_ratio_corrected2m30, aes(x = ratio)) +
    geom_histogram(binwidth = 0.1, fill = "#3C7DB1", color = "black") +
    labs(title = "Fall BTS NEFSC, 30+ cm",
         x = "Sex ratio (F/M)",
         y = "Frequency") +
    theme_classic())
ggsave("ff_m30_hist.png", plot, width = 4, height = 4, dpi = 300)

# Cut the data into bins
ff_ratio_corrected2s30_counts <- ff_ratio_corrected2s30 %>%
  mutate(bin = cut(ratio, breaks = c(0, bin_intervals[, 2]), labels = bin_labels, include.lowest = TRUE)) %>%
  group_by(bin) %>%
  summarize(count = n())

(plot <- ggplot(data = ff_ratio_corrected2s30, aes(x = ratio)) +
    geom_histogram(binwidth = 0.1, fill = "#3C7DB1", color = "black") +
    labs(title = "Fall BTS NEFSC, <30 cm",
         x = "Sex ratio (F/M)",
         y = "Frequency") +
    theme_classic())
ggsave("ff_s30_hist.png", plot, width = 4, height = 4, dpi = 300)


#### F SPRING ####

# upload and clean fs = federal spring 1968-
{
  # Tow data
  fs_tow <- read.csv(here("C:/Users/kzarrellasmi/Documents/GitHub/WF_Federal_Trawl/Spring/data", "22561_UNION_FSCS_SVSTA.csv"))
  # remove bad tows = codes that correspond to a representative tow (<2009: SHG <= 136, >=2009: TOGA <= 1324) 
  # Add year
  fs_tow$EST_YEAR <- substr(fs_tow$CRUISE6, 1, 4)
  # Split by years
  fs_tow1 <- fs_tow[fs_tow$EST_YEAR < 2009,]
  fs_tow1 <- fs_tow1[fs_tow1$SHG <= 136,] # select representative tows
  fs_tow2 <- fs_tow[fs_tow$EST_YEAR >= 2009,]
  fs_tow2 <- fs_tow2[fs_tow2$TOGA <=1324,]
  fs_tow <- rbind(fs_tow1, fs_tow2)
  #NAs in stat area, need to be removed
  fs_tow <- fs_tow[!is.na(fs_tow$AREA), ]
  # subset to GOM
  fs_tow <- fs_tow[fs_tow$AREA >= 511 & fs_tow$AREA <= 515, ]
  # columns of interest
  fs_tow <- fs_tow[, c("CRUISE6", "STRATUM", "TOW", "STATION", "AREA", "EST_YEAR")]
  
  # Length data
  fs_len <- read.csv(here("C:/Users/kzarrellasmi/Documents/GitHub/WF_Federal_Trawl/Spring/data", "22561_UNION_FSCS_SVLEN.csv"))
  # subset to winter flounder only
  fs_len <- fs_len[(fs_len$SVSPP == 106),]
  # subset to GOM
  fs_len <- merge(fs_tow, fs_len, by = c("CRUISE6", "STRATUM", "TOW", "STATION"), all = T)
  fs_len <- fs_len[!is.na(fs_len$AREA),]
  # Add year
  fs_len$EST_YEAR <- substr(fs_len$CRUISE6, 1, 4)
  # Replace NA in numbers at length with 0 for tows with no winter flounder
  fs_len$EXPNUMLEN[is.na(fs_len$EXPNUMLEN)] <- 0
  # Split by years and add calibration factor
  fs_len1 <- fs_len[fs_len$EST_YEAR < 2009,]
  fs_len2 <- fs_len[fs_len$EST_YEAR >= 2009,]
  fs_len2$EXPNUMLEN <-  fs_len2$EXPNUMLEN * 2.086
  fs_len <- rbind(fs_len1, fs_len2)
  # columns of interest
  fs_len <- fs_len[, c("CRUISE6", "STRATUM", "TOW", "STATION", "AREA", "EST_YEAR", "LENGTH", "EXPNUMLEN")]
  
  # Biological data
  fs_bio <- read.csv(here("C:/Users/kzarrellasmi/Documents/GitHub/WF_Federal_Trawl/Spring/data", "22561_UNION_FSCS_SVBIO.csv"))
  # subset to winter flounder only
  fs_bio <- fs_bio[(fs_bio$SVSPP == 106),] #mature: & fs_bio$MATURITY != "I"
  # subset to GOM
  fs_bio <- merge(fs_tow, fs_bio, by = c("CRUISE6", "STRATUM", "TOW", "STATION"), all = T)
  fs_bio <- fs_bio[!is.na(fs_bio$AREA),]
  # Add year
  fs_bio$EST_YEAR <- substr(fs_bio$CRUISE6, 1, 4)
  unique(fs_bio$EST_YEAR)
  # Replace NA in sex with 999 for tows with no winter flounder
  fs_bio$SEX[is.na(fs_bio$SEX)] <- 999
}


# Create length bins
{
  # get range for bins
  range(fs_len$LENGTH, na.rm = T)
  # Create 5 cm bins
  fs_len <- fs_len %>%
    mutate(
      length_bin = cut(
        LENGTH,
        breaks = c(seq(0, 59, by = 5), Inf),
        labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59"),
        include.lowest = TRUE,
        right = FALSE  # Ensure right-closed intervals
      )
    )
  
  # get range for bins
  range(fs_bio$LENGTH, na.rm = T)
  # Create 5 cm bins
  fs_bio <- fs_bio %>%
    mutate(
      length_bin = cut(
        LENGTH,
        breaks = c(seq(0, 59, by = 5), Inf),
        labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59"),
        include.lowest = TRUE,
        right = FALSE  # Ensure right-closed intervals
      )
    )
}


{
# Adjust for uneven sampling by weighting core strata
# some strata have more tows/km2 than others, some strata were sampled in some years and not others

  # [TOTAL_AREA_OF_STRATUM]/[TOTAL_TOWS_BY_STRATA_AND_YEAR]
  fs_tow_bin_strata <- fs_tow %>%
    group_by(EST_YEAR, STRATUM) %>%
    summarise(total = n())
  
  # Stratum data
  fs_strat <- read.csv(here("C:/Users/kzarrellasmi/Documents/GitHub/WF_Federal_Trawl/Annual", "SVDBS_SVMSTRATA.csv"))
  # columns of interest
  fs_strat <- fs_strat[ , c("stratum", "stratum_area")]
  names(fs_strat)[1] <- "STRATUM"
  # merge area of stratum with number of tows (removing stratum outside of GOM)
  fs_tow_bin_strata <- merge(fs_tow_bin_strata, fs_strat, by = "STRATUM", all.x = T)
  # generate expansion factor for length bin counts based on tow coverage for each stratum
  fs_tow_bin_strata$exp_fact <- (fs_tow_bin_strata$stratum_area/fs_tow_bin_strata$total)/100
  fs_tow_bin_strata <- fs_tow_bin_strata[ , c("STRATUM", "EST_YEAR", "exp_fact")]
  
  
  # number per length bin by strata
  fs_length_bin_strata <- fs_len %>%
    group_by(EST_YEAR, STRATUM, length_bin) %>%
    mutate(sum_EXPNUMLEN = sum(EXPNUMLEN, na.rm = TRUE)) %>%
    distinct(EST_YEAR, STRATUM, length_bin, .keep_all = TRUE)
  # remove individual data fields that were aggregated to create bins so not confusing
  fs_length_bin_strata <- fs_length_bin_strata %>%
    select(-LENGTH, -EXPNUMLEN, -STATION, -TOW)
  
  # expanded length bin counts
  # merge
  fs_length_bin_strata <- merge(fs_length_bin_strata, fs_tow_bin_strata, by = c("STRATUM", "EST_YEAR"), all = T)
  # apply expansion factor
  fs_length_bin_strata$EXP_sum_EXPNUMLEN <- fs_length_bin_strata$sum_EXPNUMLEN*fs_length_bin_strata$exp_fact
}  
 
 
{
  # Calculate the sex ratio in 5 cm length bins for each year and stratum
  fs_ratio_by_bin <- aggregate(SEX ~ length_bin + EST_YEAR + STRATUM, data = fs_bio, 
                               function(x) sum(x == 2) / (sum(x == 1) + sum(x == 2)), 
                               drop = FALSE)
  
  # Expanded numbers at length by year
  fs_length_bin_strata$EXP_sum_EXPNUMLEN
  
  # Merge sex ratio with numbers at length
  fs_length_bin_count <- left_join(fs_length_bin_strata, fs_ratio_by_bin, by = c("length_bin", "EST_YEAR", "STRATUM"))
  
  # Sex ratio conditioned on expanded numbers at length by year
  # female count
  fs_length_bin_count$f_count <- fs_length_bin_count$EXP_sum_EXPNUMLEN * fs_length_bin_count$SEX
  # male count
  fs_length_bin_count$m_count <- fs_length_bin_count$EXP_sum_EXPNUMLEN - fs_length_bin_count$f_count
  
  # corrected sex ratio by stratum
  fs_ratio_corrected <- fs_length_bin_count %>%
    group_by(EST_YEAR, STRATUM, length_bin) %>%
    mutate(ratio = sum(f_count, na.rm = TRUE) / 
                (sum(f_count, na.rm = TRUE) + sum(m_count, na.rm = TRUE)))
  
  # Sex ratio conditioned on numbers at length by year and length bin
  fs_ratio_corrected2 <- fs_length_bin_count %>%
    group_by(EST_YEAR, length_bin) %>%
    summarise(ratio = sum(f_count, na.rm = TRUE) / 
                (sum(f_count, na.rm = TRUE) + sum(m_count, na.rm = TRUE)))
  
  # Add grouping variable for plotting NA ratios
  # size bins as sampled for all years
  fs_ratio_corrected2$ratio_available <- ifelse(fs_ratio_corrected2$ratio == "NaN", 0, 1)
  
  # Sex ratio conditioned on numbers at length by year and length bin, 30+ cm fish
  fs_ratio_corrected2m30 <- fs_ratio_corrected2[!(fs_ratio_corrected2$length_bin == "0-4" |
                                                    fs_ratio_corrected2$length_bin == "5-9" |
                                                    fs_ratio_corrected2$length_bin == "10-14" |
                                                    fs_ratio_corrected2$length_bin == "15-19" |
                                                    fs_ratio_corrected2$length_bin == "20-24" |
                                                    fs_ratio_corrected2$length_bin == "25-29"), ]
  
  # Sex ratio conditioned on numbers at length by year and length bin, <30 cm fish
  fs_ratio_corrected2s30 <- fs_ratio_corrected2[(fs_ratio_corrected2$length_bin == "0-4" |
                                                   fs_ratio_corrected2$length_bin == "5-9" |
                                                   fs_ratio_corrected2$length_bin == "10-14" |
                                                   fs_ratio_corrected2$length_bin == "15-19" |
                                                   fs_ratio_corrected2$length_bin == "20-24" |
                                                   fs_ratio_corrected2$length_bin == "25-29"), ]
  
  # What uncorrected ratio was
  fs_ratio_uncorrected <- fs_bio %>%
    group_by(EST_YEAR) %>%
    summarise(
      f_count = sum(SEX == 2, na.rm = TRUE),
      m_count = sum(SEX == 1, na.rm = TRUE),
      ratio_uncorrected = f_count / (f_count + m_count))
  
  # Reformat for plotting
  fs_ratio <- left_join(fs_ratio_corrected2, fs_ratio_uncorrected, by = "EST_YEAR")
  fs_ratio <- fs_ratio %>% select(EST_YEAR, ratio, ratio_uncorrected)
  # reshape the data frame from wide to long format, keeping the "EST_YEAR" column unchanged
  fs_ratio2 <- melt(fs_ratio, id.vars = "EST_YEAR")
  
  # annual average
  fs_ratio_corrected3 <- fs_ratio_corrected2 %>%
    group_by(EST_YEAR) %>%
    summarise(ratio = mean(ratio, na.rm = T))
  
  fs_ratio_corrected3m30 <- fs_ratio_corrected2m30 %>%
    group_by(EST_YEAR) %>%
    summarise(ratio = mean(ratio, na.rm = T))
  
  fs_ratio_corrected3s30 <- fs_ratio_corrected2s30 %>%
    group_by(EST_YEAR) %>%
    summarise(ratio = mean(ratio, na.rm = T))
  
  fs_ratio_corrected2 <- fs_ratio_corrected2[!is.na(fs_ratio_corrected2$length_bin),]
  
}

# Visualize
# Sampling and ratio across length bins
(plot <- ggplot(fs_ratio_corrected2, aes(as.numeric(EST_YEAR), length_bin, fill = ratio)) +
  geom_tile() +
  labs(title = "Spring BTS NEFSC, Sex ratio by length bin",
       x = "Year",
       y = "Length bin (cm)") +
  scale_x_continuous(breaks = seq(1963, max(fs_ratio_corrected2$EST_YEAR), by = 5),
                     guide = guide_axis(angle = 45)) +
  scale_fill_continuous(name = "Ratio") +
  theme_classic())
ggsave("fs_sampledvnot.png", plot, width = 6, height = 4, dpi = 300)

# Female ratio corrected vs not corrected
(plot <- ggplot(fs_ratio2, aes(x = EST_YEAR, y = value, color = variable)) +
    #geom_point() +
    geom_line(aes(group = variable), stat = "summary") +
    labs(title = "Spring BTS NEFSC",
         x = "Year",
         y = "Sex ratio (F/M)") +
    scale_x_discrete(labels = function(x) ifelse(seq_along(x) %% 5 == 0, x, ""),
                     guide = guide_axis(angle = 45)) +
    scale_color_discrete(name = "") +
    theme_classic())
ggsave("fs_ratio_correctvnot.png", plot, width = 8, height = 4, dpi = 300)

#Corrected female ratio
(plot <- ggplot(fs_ratio_corrected2, aes(x = as.factor(EST_YEAR), y = ratio)) +
  geom_boxplot(alpha = 0.4) +
  #geom_point(data = fs_ratio_corrected3, aes(x = as.factor(EST_YEAR), y = ratio), color = "blue") +
  geom_line(data = fs_ratio_corrected3, aes(x = as.factor(EST_YEAR), y = ratio, group = 1), color = "blue", lwd = 1.5) +
  labs(title = "Spring BTS NEFSC",
       x = "Year",
       y = "Sex ratio (F/M)") +
  scale_x_discrete(labels = function(x) ifelse(seq_along(x) %% 5 == 0, x, ""),
                   guide = guide_axis(angle = 45)) +
  scale_color_discrete(name = "") +
  theme_classic() +
  geom_hline(yintercept = 0.5, color = "red"))
ggsave("fs_combined.png", plot, width = 8, height = 4, dpi = 300)

(plot <- ggplot(fs_ratio_corrected2m30, aes(x = EST_YEAR, y = ratio)) + 
  geom_boxplot() +
  geom_line(data = fs_ratio_corrected3m30, aes(x = as.factor(EST_YEAR), y = ratio, group = 1), color = "blue", lwd = 1.5) +
  labs(title = "Spring BTS NEFSC, 30+ cm",
       x = "Year",
       y = "Sex ratio (F/M)") +
  scale_x_discrete(labels = function(x) ifelse(seq_along(x) %% 5 == 0, x, ""),
                   guide = guide_axis(angle = 45)) +
  scale_color_discrete(name = "") +
  theme_classic() +
  geom_hline(yintercept = 0.5, color = "red"))
ggsave("fs_m30.png", plot, width = 8, height = 4, dpi = 300)

(plot <- ggplot(fs_ratio_corrected2s30, aes(x = EST_YEAR, y = ratio)) + 
  geom_boxplot() +
  geom_line(data = fs_ratio_corrected3s30, aes(x = as.factor(EST_YEAR), y = ratio, group = 1), color = "blue", lwd = 1.5) +
  labs(title = "Spring BTS NEFSC, <30 cm",
       x = "Year",
       y = "Sex ratio (F/M)") +
  scale_x_discrete(labels = function(x) ifelse(seq_along(x) %% 5 == 0, x, ""),
                   guide = guide_axis(angle = 45)) +
  scale_color_discrete(name = "") +
  theme_classic() +
  geom_hline(yintercept = 0.5, color = "red"))
ggsave("fs_s30.png", plot, width = 8, height = 4, dpi = 300)


# Define the number of bins
num_bins <- 11

# Calculate bin width
bin_width <- 1 / num_bins

# Create bins
bin_intervals <- seq(0, 1, by = bin_width)
bin_intervals <- cbind(bin_intervals[-length(bin_intervals)], bin_intervals[-1])

# Convert bins to labels for visualization
bin_labels <- paste0(round(bin_intervals[, 1], 4), "-", round(bin_intervals[, 2], 4))

# Cut the data into bins
fs_ratio_corrected2m30_counts <- fs_ratio_corrected2m30 %>%
  mutate(bin = cut(ratio, breaks = c(0, bin_intervals[, 2]), labels = bin_labels, include.lowest = TRUE)) %>%
  group_by(bin) %>%
  summarize(count = n())

(plot <- ggplot(data = fs_ratio_corrected2m30, aes(x = ratio)) +
    geom_histogram(binwidth = 0.1, fill = "#3C7DB1", color = "black") +
    labs(title = "Spring BTS NEFSC, 30+ cm",
         x = "Sex ratio (F/M)",
         y = "Frequency") +
    theme_classic())
ggsave("fs_m30_hist.png", plot, width = 4, height = 4, dpi = 300)

# Cut the data into bins
fs_ratio_corrected2s30_counts <- fs_ratio_corrected2s30 %>%
  mutate(bin = cut(ratio, breaks = c(0, bin_intervals[, 2]), labels = bin_labels, include.lowest = TRUE)) %>%
  group_by(bin) %>%
  summarize(count = n())

(plot <- ggplot(data = fs_ratio_corrected2s30, aes(x = ratio)) +
    geom_histogram(binwidth = 0.1, fill = "#3C7DB1", color = "black") +
    labs(title = "Spring BTS NEFSC, <30 cm",
         x = "Sex ratio (F/M)",
         y = "Frequency") +
    theme_classic())
ggsave("fs_s30_hist.png", plot, width = 4, height = 4, dpi = 300)

#### F SUMMER ####

# upload and clean fsu = federal Summer 1991-[1992]-1995
{
  # Tow data
  fsu_tow <- read.csv(here("C:/Users/kzarrellasmi/Documents/GitHub/WF_Federal_Trawl/Summer/data", "22562_UNION_FSCS_SVSTA.csv"))
  # Add year
  fsu_tow$EST_YEAR <- substr(fsu_tow$CRUISE6, 1, 4)
  # remove bad tows: codes that correspond to a representative tow (SHG <= 136) 
  fsu_tow <- fsu_tow[fsu_tow$SHG <= 136,]
  # subset to GOM
  fsu_tow <- fsu_tow[fsu_tow$AREA >= 511 & fsu_tow$AREA <= 515, ]
  # columns of interest
  fsu_tow <- fsu_tow[, c("CRUISE6", "STRATUM", "TOW", "STATION", "AREA", "EST_YEAR")]
  
  # Length data
  fsu_len <- read.csv(here("C:/Users/kzarrellasmi/Documents/GitHub/WF_Federal_Trawl/Summer/data", "22562_UNION_FSCS_SVLEN.csv"))
  # subset to winter flounder only
  fsu_len <- fsu_len[(fsu_len$SVSPP == 106),]
  # subset to GOM
  fsu_len <- merge(fsu_tow, fsu_len, by = c("CRUISE6", "STRATUM", "TOW", "STATION"), all = T)
  fsu_len <- fsu_len[!is.na(fsu_len$AREA),]
  # Add year
  fsu_len$EST_YEAR <- substr(fsu_len$CRUISE6, 1, 4)
  # Replace NA in numbers at length with 0 for tows with no winter flounder
  fsu_len$EXPNUMLEN[is.na(fsu_len$EXPNUMLEN)] <- 0
  # columns of interest
  fsu_len <- fsu_len[, c("CRUISE6", "STRATUM", "TOW", "STATION", "AREA", "EST_YEAR", "LENGTH", "EXPNUMLEN")]
  
  # Biological data
  fsu_bio <- read.csv(here("C:/Users/kzarrellasmi/Documents/GitHub/WF_Federal_Trawl/Summer/data", "22562_UNION_FSCS_SVBIO.csv"))
  # subset to winter flounder only
  fsu_bio <- fsu_bio[(fsu_bio$SVSPP == 106),] #mature: & fsu_bio$MATURITY != "I"
  # subset to GOM
  fsu_bio <- merge(fsu_tow, fsu_bio, by = c("CRUISE6", "STRATUM", "TOW", "STATION"), all = T)
  fsu_bio <- fsu_bio[!is.na(fsu_bio$AREA),]
  # Add year
  fsu_bio$EST_YEAR <- substr(fsu_bio$CRUISE6, 1, 4)
  # Replace NA in sex with 999 for tows with no winter flounder
  fsu_bio$SEX[is.na(fsu_bio$SEX)] <- 999
}


# Create length bins
{
  # get range for bins
  range(fsu_len$LENGTH, na.rm = T)
  # Create 5 cm bins
  fsu_len <- fsu_len %>%
    mutate(
      length_bin = cut(
        LENGTH,
        breaks = c(seq(0, 59, by = 5), Inf),
        labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59"),
        include.lowest = TRUE,
        right = FALSE  # Ensure right-closed intervals
      )
    )
  
  # get range for bins
  range(fsu_bio$LENGTH, na.rm = T)
  # Create 5 cm bins
  fsu_bio <- fsu_bio %>%
    mutate(
      length_bin = cut(
        LENGTH,
        breaks = c(seq(0, 59, by = 5), Inf),
        labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59"),
        include.lowest = TRUE,
        right = FALSE  # Ensure right-closed intervals
      )
    )
}


{
  # Adjust for uneven sampling by weighting core strata
  # some strata have more tows/km2 than others, some strata were sampled in some years and not others
  
  # [TOTAL_AREA_OF_STRATUM]/[TOTAL_TOWS_BY_STRATA_AND_YEAR]
  fsu_tow_bin_strata <- fsu_tow %>%
    group_by(EST_YEAR, STRATUM) %>%
    summarise(total = n())
  
  # Stratum data
  fsu_strat <- read.csv(here("C:/Users/kzarrellasmi/Documents/GitHub/WF_Federal_Trawl/Annual", "SVDBS_SVMSTRATA.csv"))
  # columns of interest
  fsu_strat <- fsu_strat[ , c("stratum", "stratum_area")]
  names(fsu_strat)[1] <- "STRATUM"
  # merge area of stratum with number of tows (removing stratum outside of GOM)
  fsu_tow_bin_strata <- merge(fsu_tow_bin_strata, fsu_strat, by = "STRATUM", all.x = T)
  # generate expansion factor for length bin counts based on tow coverage for each stratum
  fsu_tow_bin_strata$exp_fact <- (fsu_tow_bin_strata$stratum_area/fsu_tow_bin_strata$total)/100
  fsu_tow_bin_strata <- fsu_tow_bin_strata[ , c("STRATUM", "EST_YEAR", "exp_fact")]
  
  
  # number per length bin by strata
  fsu_length_bin_strata <- fsu_len %>%
    group_by(EST_YEAR, STRATUM, length_bin) %>%
    mutate(sum_EXPNUMLEN = sum(EXPNUMLEN, na.rm = TRUE)) %>%
    distinct(EST_YEAR, STRATUM, length_bin, .keep_all = TRUE)
  # remove individual data fields that were aggregated to create bins so not confusing
  fsu_length_bin_strata <- fsu_length_bin_strata %>%
    select(-LENGTH, -EXPNUMLEN, -STATION, -TOW)
  
  # expanded length bin counts
  # merge
  fsu_length_bin_strata <- merge(fsu_length_bin_strata, fsu_tow_bin_strata, by = c("STRATUM", "EST_YEAR"), all = T)
  # apply expansion factor
  fsu_length_bin_strata$EXP_sum_EXPNUMLEN <- fsu_length_bin_strata$sum_EXPNUMLEN*fsu_length_bin_strata$exp_fact
}  


{
  # Calculate the sex ratio in 5 cm length bins for each year and stratum
  fsu_ratio_by_bin <- aggregate(SEX ~ length_bin + EST_YEAR + STRATUM, data = fsu_bio, 
                               function(x) sum(x == 2) / (sum(x == 1) + sum(x == 2)), 
                               drop = FALSE)
  
  # Expanded numbers at length by year
  fsu_length_bin_strata$EXP_sum_EXPNUMLEN
  
  # Merge sex ratio with numbers at length
  fsu_length_bin_count <- left_join(fsu_length_bin_strata, fsu_ratio_by_bin, by = c("length_bin", "EST_YEAR", "STRATUM"))
  
  # Sex ratio conditioned on expanded numbers at length by year
  # female count
  fsu_length_bin_count$f_count <- fsu_length_bin_count$EXP_sum_EXPNUMLEN * fsu_length_bin_count$SEX
  # male count
  fsu_length_bin_count$m_count <- fsu_length_bin_count$EXP_sum_EXPNUMLEN - fsu_length_bin_count$f_count
  
  # corrected sex ratio by stratum
  fsu_ratio_corrected <- fsu_length_bin_count %>%
    group_by(EST_YEAR, STRATUM, length_bin) %>%
    mutate(ratio = sum(f_count, na.rm = TRUE) / 
             (sum(f_count, na.rm = TRUE) + sum(m_count, na.rm = TRUE)))
  
  # Sex ratio conditioned on numbers at length by year and length bin
  fsu_ratio_corrected2 <- fsu_length_bin_count %>%
    group_by(EST_YEAR, length_bin) %>%
    summarise(ratio = sum(f_count, na.rm = TRUE) / 
                (sum(f_count, na.rm = TRUE) + sum(m_count, na.rm = TRUE)))
  
  # Add grouping variable for plotting NA ratios
  # size bins as sampled for all years
  fsu_ratio_corrected2$ratio_available <- ifelse(fsu_ratio_corrected2$ratio == "NaN", 0, 1)
  
  # Sex ratio conditioned on numbers at length by year and length bin, 30+ cm fish
  fsu_ratio_corrected2m30 <- fsu_ratio_corrected2[!(fsu_ratio_corrected2$length_bin == "0-4" |
                                                    fsu_ratio_corrected2$length_bin == "5-9" |
                                                    fsu_ratio_corrected2$length_bin == "10-14" |
                                                    fsu_ratio_corrected2$length_bin == "15-19" |
                                                    fsu_ratio_corrected2$length_bin == "20-24" |
                                                    fsu_ratio_corrected2$length_bin == "25-29"), ]
  
  # Sex ratio conditioned on numbers at length by year and length bin, <30 cm fish
  fsu_ratio_corrected2s30 <- fsu_ratio_corrected2[(fsu_ratio_corrected2$length_bin == "0-4" |
                                                   fsu_ratio_corrected2$length_bin == "5-9" |
                                                   fsu_ratio_corrected2$length_bin == "10-14" |
                                                   fsu_ratio_corrected2$length_bin == "15-19" |
                                                   fsu_ratio_corrected2$length_bin == "20-24" |
                                                   fsu_ratio_corrected2$length_bin == "25-29"), ]
  
  # What uncorrected ratio was
  fsu_ratio_uncorrected <- fsu_bio %>%
    group_by(EST_YEAR) %>%
    summarise(
      f_count = sum(SEX == 2, na.rm = TRUE),
      m_count = sum(SEX == 1, na.rm = TRUE),
      ratio_uncorrected = f_count / (f_count + m_count))
  
  # Reformat for plotting
  fsu_ratio <- left_join(fsu_ratio_corrected2, fsu_ratio_uncorrected, by = "EST_YEAR")
  fsu_ratio <- fsu_ratio %>% select(EST_YEAR, ratio, ratio_uncorrected)
  # reshape the data frame from wide to long format, keeping the "EST_YEAR" column unchanged
  fsu_ratio2 <- melt(fsu_ratio, id.vars = "EST_YEAR")
  
  # annual average
  fsu_ratio_corrected3 <- fsu_ratio_corrected2 %>%
    group_by(EST_YEAR) %>%
    summarise(ratio = mean(ratio, na.rm = T))
  
  fsu_ratio_corrected3m30 <- fsu_ratio_corrected2m30 %>%
    group_by(EST_YEAR) %>%
    summarise(ratio = mean(ratio, na.rm = T))
  
  fsu_ratio_corrected3s30 <- fsu_ratio_corrected2s30 %>%
    group_by(EST_YEAR) %>%
    summarise(ratio = mean(ratio, na.rm = T))
  
  fsu_ratio_corrected2 <- fsu_ratio_corrected2[!is.na(fsu_ratio_corrected2$length_bin),]
  
}

# Visualize
# Sampling and ratio across length bins
(plot <- ggplot(fsu_ratio_corrected2, aes(as.numeric(EST_YEAR), length_bin, fill = ratio)) +
    geom_tile() +
    labs(title = "Summer BTS NEFSC, Sex ratio by length bin",
         x = "Year",
         y = "Length bin (cm)") +
    scale_x_continuous(breaks = seq(1963, max(fsu_ratio_corrected2$EST_YEAR), by = 5),
                       guide = guide_axis(angle = 45)) +
    scale_fill_continuous(name = "Ratio") +
    theme_classic())
ggsave("fsu_sampledvnot.png", plot, width = 6, height = 4, dpi = 300)

# Female ratio corrected vs not corrected
(plot <- ggplot(fsu_ratio2, aes(x = EST_YEAR, y = value, color = variable)) +
    #geom_point() +
    geom_line(aes(group = variable), stat = "summary") +
    labs(title = "Summer BTS NEFSC",
         x = "Year",
         y = "Sex ratio (F/M)") +
    scale_x_discrete(labels = function(x) ifelse(seq_along(x) %% 5 == 0, x, ""),
                     guide = guide_axis(angle = 45)) +
    scale_color_discrete(name = "") +
    theme_classic())
ggsave("fsu_ratio_correctvnot.png", plot, width = 8, height = 4, dpi = 300)

#Corrected female ratio
(plot <- ggplot(fsu_ratio_corrected2, aes(x = as.factor(EST_YEAR), y = ratio)) +
    geom_boxplot(alpha = 0.4) +
    #geom_point(data = fsu_ratio_corrected3, aes(x = as.factor(EST_YEAR), y = ratio), color = "blue") +
    geom_line(data = fsu_ratio_corrected3, aes(x = as.factor(EST_YEAR), y = ratio, group = 1), color = "blue", lwd = 1.5) +
    labs(title = "Summer BTS NEFSC",
         x = "Year",
         y = "Sex ratio (F/M)") +
    scale_x_discrete(labels = function(x) ifelse(seq_along(x) %% 5 == 0, x, ""),
                     guide = guide_axis(angle = 45)) +
    scale_color_discrete(name = "") +
    theme_classic() +
    geom_hline(yintercept = 0.5, color = "red"))
ggsave("fsu_combined.png", plot, width = 8, height = 4, dpi = 300)

(plot <- ggplot(fsu_ratio_corrected2m30, aes(x = EST_YEAR, y = ratio)) + 
    geom_boxplot() +
    geom_line(data = fsu_ratio_corrected3m30, aes(x = as.factor(EST_YEAR), y = ratio, group = 1), color = "blue", lwd = 1.5) +
    labs(title = "Summer BTS NEFSC, 30+ cm",
         x = "Year",
         y = "Sex ratio (F/M)") +
    scale_x_discrete(labels = function(x) ifelse(seq_along(x) %% 5 == 0, x, ""),
                     guide = guide_axis(angle = 45)) +
    scale_color_discrete(name = "") +
    theme_classic() +
    geom_hline(yintercept = 0.5, color = "red"))
ggsave("fsu_m30.png", plot, width = 8, height = 4, dpi = 300)

(plot <- ggplot(fsu_ratio_corrected2s30, aes(x = EST_YEAR, y = ratio)) + 
    geom_boxplot() +
    geom_line(data = fsu_ratio_corrected3s30, aes(x = as.factor(EST_YEAR), y = ratio, group = 1), color = "blue", lwd = 1.5) +
    labs(title = "Summer BTS NEFSC, <30 cm",
         x = "Year",
         y = "Sex ratio (F/M)") +
    scale_x_discrete(labels = function(x) ifelse(seq_along(x) %% 5 == 0, x, ""),
                     guide = guide_axis(angle = 45)) +
    scale_color_discrete(name = "") +
    theme_classic() +
    geom_hline(yintercept = 0.5, color = "red"))
ggsave("fsu_s30.png", plot, width = 8, height = 4, dpi = 300)


# Define the number of bins
num_bins <- 11

# Calculate bin width
bin_width <- 1 / num_bins

# Create bins
bin_intervals <- seq(0, 1, by = bin_width)
bin_intervals <- cbind(bin_intervals[-length(bin_intervals)], bin_intervals[-1])

# Convert bins to labels for visualization
bin_labels <- paste0(round(bin_intervals[, 1], 4), "-", round(bin_intervals[, 2], 4))

# Cut the data into bins
fsu_ratio_corrected2m30_counts <- fsu_ratio_corrected2m30 %>%
  mutate(bin = cut(ratio, breaks = c(0, bin_intervals[, 2]), labels = bin_labels, include.lowest = TRUE)) %>%
  group_by(bin) %>%
  summarize(count = n())

(plot <- ggplot(data = fsu_ratio_corrected2m30, aes(x = ratio)) +
    geom_histogram(binwidth = 0.1, fill = "#3C7DB1", color = "black") +
    labs(title = "Summer BTS NEFSC, 30+ cm",
         x = "Sex ratio (F/M)",
         y = "Frequency") +
    theme_classic())
ggsave("fsu_m30_hist.png", plot, width = 4, height = 4, dpi = 300)

# Cut the data into bins
fsu_ratio_corrected2s30_counts <- fsu_ratio_corrected2s30 %>%
  mutate(bin = cut(ratio, breaks = c(0, bin_intervals[, 2]), labels = bin_labels, include.lowest = TRUE)) %>%
  group_by(bin) %>%
  summarize(count = n())

(plot <- ggplot(data = fsu_ratio_corrected2s30, aes(x = ratio)) +
    geom_histogram(binwidth = 0.1, fill = "#3C7DB1", color = "black") +
    labs(title = "Summer BTS NEFSC, <30 cm",
         x = "Sex ratio (F/M)",
         y = "Frequency") +
    theme_classic())
ggsave("fsu_s30_hist.png", plot, width = 4, height = 4, dpi = 300)


#### MA FALL ####

# upload and clean maf = MADMF fall 1978-[2020]-
{
  # Tow data
  maf_tow <- read.csv(here("C:/Users/kzarrellasmi/Documents/GitHub/WF_MA_State_Trawl/Annual/data", "106_stacat_78-23.csv"))
  # Subset to fall
  maf_tow <- maf_tow[maf_tow$SEASON == "FALL",]
  # Subset to GOM
  maf_tow <- maf_tow[!(maf_tow$LATITUDE < 42 & maf_tow$LONGITUDE < 70), ]
  
  ####
  #bad tows?
  #####
  
  # columns of interest
  maf_tow <- maf_tow[, c("CRUISE", "STRATUM", "STATION", "YEAR")]
  maf_tow$AREA <- "GOM"

  # Length data
  maf_len <- read.csv(here("C:/Users/kzarrellasmi/Documents/GitHub/WF_MA_State_Trawl/Annual/data", "106_stalen_78-23.csv"))
  # Subset to fall
  maf_len <- maf_len[maf_len$SEASON == "FALL",]
  # subset to GOM
  maf_len <- merge(maf_tow, maf_len, by = c("CRUISE", "STRATUM", "STATION"), all = T)
  maf_len <- maf_len[!is.na(maf_len$AREA),]
  names(maf_len)[names(maf_len) == "YEAR.x"] <- "YEAR"
  # Replace NA in numbers at length with 0 for tows with no winter flounder
  maf_len$NUMBER.AT.LENGTH[is.na(maf_len$NUMBER.AT.LENGTH)] <- 0
  # columns of interest
  maf_len <- maf_len[, c("CRUISE", "STRATUM", "STATION", "YEAR", "LENGTH..cm.", "NUMBER.AT.LENGTH")]
  
  # Biological data
  maf_bio <- read.csv(here("C:/Users/kzarrellasmi/Documents/GitHub/WF_MA_State_Trawl/Annual/data", "106_stabio_78-23.csv"))
  # Subset to fall
  maf_bio <- maf_bio[maf_bio$SEASON == "FALL",]
  # subset to GOM
  maf_bio <- merge(maf_tow, maf_bio, by = c("CRUISE", "STRATUM", "STATION"), all = T)
  maf_bio <- maf_bio[!is.na(maf_bio$AREA),]
  names(maf_bio)[names(maf_bio) == "YEAR.x"] <- "YEAR"
  # Replace NA in sex with 999 for tows with no winter flounder
  maf_bio$SEX.CODE[is.na(maf_bio$SEX.CODE)] <- 999
}


# Create length bins
{
  # get range for bins
  range(maf_len$LENGTH..cm., na.rm = T)
  # Create 5 cm bins
  maf_len <- maf_len %>%
    mutate(
      length_bin = cut(
        LENGTH..cm.,
        breaks = c(seq(0, 54, by = 5), Inf),
        labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54"),
        include.lowest = TRUE,
        right = FALSE  # Ensure right-closed intervals
      )
    )
  
  # get range for bins
  range(maf_bio$LENGTH..cm., na.rm = T)
  # Create 5 cm bins
  maf_bio <- maf_bio %>%
    mutate(
      length_bin = cut(
        LENGTH..cm.,
        breaks = c(seq(0, 54, by = 5), Inf),
        labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54"),
        include.lowest = TRUE,
        right = FALSE  # Ensure right-closed intervals
      )
    )
}


{
  # Adjust for uneven sampling by weighting core strata
  # some strata have more tows/km2 than others, some strata were sampled in some years and not others
  
  # [TOTAL_AREA_OF_STRATUM]/[TOTAL_TOWS_BY_STRATA_AND_YEAR]
  maf_tow_bin_strata <- maf_tow %>%
    group_by(YEAR, STRATUM) %>%
    summarise(total = n())
  
  #### -------------------------------------------
  #stratum area?
  # Stratum data
  maf_strat <- read.csv(here("C:/Users/kzarrellasmi/Documents/GitHub/WF_Federal_Trawl/Annual", "SVDBS_SVMSTRATA.csv"))
  # columns of interest
  maf_strat <- maf_strat[ , c("stratum", "stratum_area")]
  names(maf_strat)[1] <- "STRATUM"
  # merge area of stratum with number of tows (removing stratum outside of GOM)
  maf_tow_bin_strata <- merge(maf_tow_bin_strata, maf_strat, by = "STRATUM", all.x = T)
  # generate expansion factor for length bin counts based on tow coverage for each stratum
  maf_tow_bin_strata$exp_fact <- (maf_tow_bin_strata$stratum_area/maf_tow_bin_strata$total)/100
  maf_tow_bin_strata <- maf_tow_bin_strata[ , c("STRATUM", "EST_YEAR", "exp_fact")]
  ####
  
  # number per length bin by strata
  maf_length_bin_strata <- maf_len %>%
    group_by(YEAR, STRATUM, length_bin) %>%
    mutate(sum_EXPNUMLEN = sum(NUMBER.AT.LENGTH, na.rm = TRUE)) %>%
    distinct(YEAR, STRATUM, length_bin, .keep_all = TRUE)
  # remove individual data fields that were aggregated to create bins so not confusing
  maf_length_bin_strata <- maf_length_bin_strata %>%
    select(-LENGTH..cm., -NUMBER.AT.LENGTH, -STATION)
  
  # expanded length bin counts
  # merge
  maf_length_bin_strata <- merge(maf_length_bin_strata, maf_tow_bin_strata, by = c("STRATUM", "YEAR"), all = T)
  
  #### -----------------------------------------------------
  # apply expansion factor
  maf_length_bin_strata$EXP_sum_EXPNUMLEN <- maf_length_bin_strata$sum_EXPNUMLEN*maf_length_bin_strata$exp_fact
  ####
}  


{
  # Calculate the sex ratio in 5 cm length bins for each year and stratum
  maf_ratio_by_bin <- aggregate(SEX.CODE ~ length_bin + YEAR + STRATUM, data = maf_bio, 
                               function(x) sum(x == 2) / (sum(x == 1) + sum(x == 2)), 
                               drop = FALSE)
  
  # Expanded numbers at length by year
  maf_length_bin_strata$EXP_sum_EXPNUMLEN
  
  # Merge sex ratio with numbers at length
  maf_length_bin_count <- left_join(maf_length_bin_strata, maf_ratio_by_bin, by = c("length_bin", "YEAR", "STRATUM"))
  
  # Sex ratio conditioned on expanded numbers at length by year
  # female count
  maf_length_bin_count$f_count <- maf_length_bin_count$EXP_sum_EXPNUMLEN * maf_length_bin_count$SEX
  # male count
  maf_length_bin_count$m_count <- maf_length_bin_count$EXP_sum_EXPNUMLEN - maf_length_bin_count$f_count
  
  # corrected sex ratio by stratum
  maf_ratio_corrected <- maf_length_bin_count %>%
    group_by(YEAR, STRATUM, length_bin) %>%
    mutate(ratio = sum(f_count, na.rm = TRUE) / 
             (sum(f_count, na.rm = TRUE) + sum(m_count, na.rm = TRUE)))
  
  # Sex ratio conditioned on numbers at length by year and length bin
  maf_ratio_corrected2 <- maf_length_bin_count %>%
    group_by(YEAR, length_bin) %>%
    summarise(ratio = sum(f_count, na.rm = TRUE) / 
                (sum(f_count, na.rm = TRUE) + sum(m_count, na.rm = TRUE)))
  
  # Add grouping variable for plotting NA ratios
  # size bins as sampled for all years
  maf_ratio_corrected2$ratio_available <- ifelse(maf_ratio_corrected2$ratio == "NaN", 0, 1)
  
  # Sex ratio conditioned on numbers at length by year and length bin, 30+ cm fish
  maf_ratio_corrected2m30 <- maf_ratio_corrected2[!(maf_ratio_corrected2$length_bin == "0-4" |
                                                    maf_ratio_corrected2$length_bin == "5-9" |
                                                    maf_ratio_corrected2$length_bin == "10-14" |
                                                    maf_ratio_corrected2$length_bin == "15-19" |
                                                    maf_ratio_corrected2$length_bin == "20-24" |
                                                    maf_ratio_corrected2$length_bin == "25-29"), ]
  
  # Sex ratio conditioned on numbers at length by year and length bin, <30 cm fish
  maf_ratio_corrected2s30 <- maf_ratio_corrected2[(maf_ratio_corrected2$length_bin == "0-4" |
                                                   maf_ratio_corrected2$length_bin == "5-9" |
                                                   maf_ratio_corrected2$length_bin == "10-14" |
                                                   maf_ratio_corrected2$length_bin == "15-19" |
                                                   maf_ratio_corrected2$length_bin == "20-24" |
                                                   maf_ratio_corrected2$length_bin == "25-29"), ]
  
  # What uncorrected ratio was
  maf_ratio_uncorrected <- maf_bio %>%
    group_by(YEAR) %>%
    summarise(
      f_count = sum(SEX.CODE == 2, na.rm = TRUE),
      m_count = sum(SEX.CODE == 1, na.rm = TRUE),
      ratio_uncorrected = f_count / (f_count + m_count))
  
  
  # Reformat for plotting
  maf_ratio <- left_join(maf_ratio_corrected2, maf_ratio_uncorrected, by = "YEAR")
  maf_ratio <- maf_ratio %>% select(YEAR, ratio, ratio_uncorrected)
  # reshape the data frame from wide to long format, keeping the "EST_YEAR" column unchanged
  maf_ratio2 <- melt(maf_ratio, id.vars = "YEAR")
  
  # annual average
  maf_ratio_corrected3 <- maf_ratio_corrected2 %>%
    group_by(YEAR) %>%
    summarise(ratio = mean(ratio, na.rm = T))
  
  maf_ratio_corrected3m30 <- maf_ratio_corrected2m30 %>%
    group_by(YEAR) %>%
    summarise(ratio = mean(ratio, na.rm = T))
  
  maf_ratio_corrected3s30 <- maf_ratio_corrected2s30 %>%
    group_by(YEAR) %>%
    summarise(ratio = mean(ratio, na.rm = T))
  
  maf_ratio_corrected2 <- maf_ratio_corrected2[!is.na(maf_ratio_corrected2$length_bin),]
  
}

# Visualize
# Sampling and ratio across length bins

(plot <- ggplot(maf_ratio_corrected2, aes(as.numeric(YEAR), length_bin, fill = ratio)) +
    geom_tile() +
    labs(title = "Fall BTS MADMF, Sex ratio by length bin",
         x = "Year",
         y = "Length bin (cm)") +
    scale_x_continuous(breaks = seq(1963, max(maf_ratio_corrected2$YEAR), by = 5),
                       guide = guide_axis(angle = 45)) +
    scale_fill_continuous(name = "Ratio") +
    theme_classic())
ggsave("maf_sampledvnot.png", plot, width = 6, height = 4, dpi = 300)

# Female ratio corrected vs not corrected
(plot <- ggplot(maf_ratio2, aes(x = as.factor(YEAR), y = value, color = variable)) +
    #geom_point() +
    geom_line(aes(group = variable), stat = "summary") +
    labs(title = "Fall BTS MADMF",
         x = "Year",
         y = "Sex ratio (F/M)") +
    scale_x_discrete(labels = function(x) ifelse(seq_along(x) %% 5 == 0, x, ""),
                     guide = guide_axis(angle = 45)) +
    scale_color_discrete(name = "") +
    theme_classic())
ggsave("maf_ratio_correctvnot.png", plot, width = 8, height = 4, dpi = 300)

#Corrected female ratio
(plot <- ggplot(maf_ratio_corrected2, aes(x = as.factor(YEAR), y = ratio)) +
    geom_boxplot(alpha = 0.4) +
    #geom_point(data = maf_ratio_corrected3, aes(x = as.factor(EST_YEAR), y = ratio), color = "blue") +
    geom_line(data = maf_ratio_corrected3, aes(x = as.factor(YEAR), y = ratio, group = 1), color = "blue", lwd = 1.5) +
    labs(title = "Fall BTS MADMF",
         x = "Year",
         y = "Sex ratio (F/M)") +
    scale_x_discrete(labels = function(x) ifelse(seq_along(x) %% 5 == 0, x, ""),
                     guide = guide_axis(angle = 45)) +
    scale_color_discrete(name = "") +
    theme_classic() +
    geom_hline(yintercept = 0.5, color = "red"))
ggsave("maf_combined.png", plot, width = 8, height = 4, dpi = 300)

(plot <- ggplot(maf_ratio_corrected2m30, aes(x = as.factor(YEAR), y = ratio)) + 
    geom_boxplot() +
    geom_line(data = maf_ratio_corrected3m30, aes(x = as.factor(YEAR), y = ratio, group = 1), color = "blue", lwd = 1.5) +
    labs(title = "Fall BTS MADMF, 30+ cm",
         x = "Year",
         y = "Sex ratio (F/M)") +
    scale_x_discrete(labels = function(x) ifelse(seq_along(x) %% 5 == 0, x, ""),
                     guide = guide_axis(angle = 45)) +
    scale_color_discrete(name = "") +
    theme_classic() +
    geom_hline(yintercept = 0.5, color = "red"))
ggsave("maf_m30.png", plot, width = 8, height = 4, dpi = 300)

(plot <- ggplot(maf_ratio_corrected2s30, aes(x = as.factor(YEAR), y = ratio)) + 
    geom_boxplot() +
    geom_line(data = maf_ratio_corrected3s30, aes(x = as.factor(YEAR), y = ratio, group = 1), color = "blue", lwd = 1.5) +
    labs(title = "Fall BTS MADMF, <30 cm",
         x = "Year",
         y = "Sex ratio (F/M)") +
    scale_x_discrete(labels = function(x) ifelse(seq_along(x) %% 5 == 0, x, ""),
                     guide = guide_axis(angle = 45)) +
    scale_color_discrete(name = "") +
    theme_classic() +
    geom_hline(yintercept = 0.5, color = "red"))
ggsave("maf_s30.png", plot, width = 8, height = 4, dpi = 300)


# Define the number of bins
num_bins <- 11

# Calculate bin width
bin_width <- 1 / num_bins

# Create bins
bin_intervals <- seq(0, 1, by = bin_width)
bin_intervals <- cbind(bin_intervals[-length(bin_intervals)], bin_intervals[-1])

# Convert bins to labels for visualization
bin_labels <- paste0(round(bin_intervals[, 1], 4), "-", round(bin_intervals[, 2], 4))

# Cut the data into bins
maf_ratio_corrected2m30_counts <- maf_ratio_corrected2m30 %>%
  mutate(bin = cut(ratio, breaks = c(0, bin_intervals[, 2]), labels = bin_labels, include.lowest = TRUE)) %>%
  group_by(bin) %>%
  summarize(count = n())

(plot <- ggplot(data = maf_ratio_corrected2m30, aes(x = ratio)) +
    geom_histogram(binwidth = 0.1, fill = "#3C7DB1", color = "black") +
    labs(title = "Fall BTS MADMF, 30+ cm",
         x = "Sex ratio (F/M)",
         y = "Frequency") +
    theme_classic())
ggsave("maf_m30_hist.png", plot, width = 4, height = 4, dpi = 300)

# Cut the data into bins
maf_ratio_corrected2s30_counts <- maf_ratio_corrected2s30 %>%
  mutate(bin = cut(ratio, breaks = c(0, bin_intervals[, 2]), labels = bin_labels, include.lowest = TRUE)) %>%
  group_by(bin) %>%
  summarize(count = n())

(plot <- ggplot(data = maf_ratio_corrected2s30, aes(x = ratio)) +
    geom_histogram(binwidth = 0.1, fill = "#3C7DB1", color = "black") +
    labs(title = "Fall BTS MADMF, <30 cm",
         x = "Sex ratio (F/M)",
         y = "Frequency") +
    theme_classic())
ggsave("maf_s30_hist.png", plot, width = 4, height = 4, dpi = 300)


#### MA SPRING ####

# upload and clean mas = MADMF Spring 1978-[2020]-
{
  # Tow data
  mas_tow <- read.csv(here("C:/Users/kzarrellasmi/Documents/GitHub/WF_MA_State_Trawl/Annual/data", "106_stacat_78-23.csv"))
  # Subset to Spring
  mas_tow <- mas_tow[mas_tow$SEASON == "SPRING",]
  # Subset to GOM
  mas_tow <- mas_tow[!(mas_tow$LATITUDE < 42 & mas_tow$LONGITUDE < 70), ]
  
  #### --------------------------------------------------------
  #bad tows?
  ####
  
  # columns of interest
  mas_tow <- mas_tow[, c("CRUISE", "STRATUM", "STATION", "YEAR")]
  mas_tow$AREA <- "GOM"
  
  # Length data
  mas_len <- read.csv(here("C:/Users/kzarrellasmi/Documents/GitHub/WF_MA_State_Trawl/Annual/data", "106_stalen_78-23.csv"))
  # Subset to Spring
  mas_len <- mas_len[mas_len$SEASON == "SPRING",]
  # subset to GOM
  mas_len <- merge(mas_tow, mas_len, by = c("CRUISE", "STRATUM", "STATION"), all = T)
  mas_len <- mas_len[!is.na(mas_len$AREA),]
  names(mas_len)[names(mas_len) == "YEAR.x"] <- "YEAR"
  # Replace NA in numbers at length with 0 for tows with no winter flounder
  mas_len$NUMBER.AT.LENGTH[is.na(mas_len$NUMBER.AT.LENGTH)] <- 0
  # columns of interest
  mas_len <- mas_len[, c("CRUISE", "STRATUM", "STATION", "YEAR", "LENGTH..cm.", "NUMBER.AT.LENGTH")]
  
  # Biological data
  mas_bio <- read.csv(here("C:/Users/kzarrellasmi/Documents/GitHub/WF_MA_State_Trawl/Annual/data", "106_stabio_78-23.csv"))
  # Subset to Spring
  mas_bio <- mas_bio[mas_bio$SEASON == "SPRING",]
  # subset to GOM
  mas_bio <- merge(mas_tow, mas_bio, by = c("CRUISE", "STRATUM", "STATION"), all = T)
  mas_bio <- mas_bio[!is.na(mas_bio$AREA),]
  names(mas_bio)[names(mas_bio) == "YEAR.x"] <- "YEAR"
  # Replace NA in sex with 999 for tows with no winter flounder
  mas_bio$SEX.CODE[is.na(mas_bio$SEX.CODE)] <- 999
}


# Create length bins
{
  # get range for bins
  range(mas_len$LENGTH..cm., na.rm = T)
  # Create 5 cm bins
  mas_len <- mas_len %>%
    mutate(
      length_bin = cut(
        LENGTH..cm.,
        breaks = c(seq(0, 59, by = 5), Inf),
        labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59"),
        include.lowest = TRUE,
        right = FALSE  # Ensure right-closed intervals
      )
    )
  
  # get range for bins
  range(mas_bio$LENGTH..cm., na.rm = T)
  # Create 5 cm bins
  mas_bio <- mas_bio %>%
    mutate(
      length_bin = cut(
        LENGTH..cm.,
        breaks = c(seq(0, 59, by = 5), Inf),
        labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59"),
        include.lowest = TRUE,
        right = FALSE  # Ensure right-closed intervals
      )
    )
}


{
  # Adjust for uneven sampling by weighting core strata
  # some strata have more tows/km2 than others, some strata were sampled in some years and not others
  
  # [TOTAL_AREA_OF_STRATUM]/[TOTAL_TOWS_BY_STRATA_AND_YEAR]
  mas_tow_bin_strata <- mas_tow %>%
    group_by(YEAR, STRATUM) %>%
    summarise(total = n())
  
  #### -------------------------------------------------------------------------
  # Stratum data
  mas_strat <- read.csv(here("C:/Users/kzarrellasmi/Documents/GitHub/WF_Federal_Trawl/Annual", "SVDBS_SVMSTRATA.csv"))
  # columns of interest
  mas_strat <- mas_strat[ , c("stratum", "stratum_area")]
  names(mas_strat)[1] <- "STRATUM"
  # merge area of stratum with number of tows (removing stratum outside of GOM)
  mas_tow_bin_strata <- merge(mas_tow_bin_strata, mas_strat, by = "STRATUM", all.x = T)
  # generate expansion factor for length bin counts based on tow coverage for each stratum
  mas_tow_bin_strata$exp_fact <- (mas_tow_bin_strata$stratum_area/mas_tow_bin_strata$total)/100
  mas_tow_bin_strata <- mas_tow_bin_strata[ , c("STRATUM", "EST_YEAR", "exp_fact")]
  ####
  
  # number per length bin by strata
  mas_length_bin_strata <- mas_len %>%
    group_by(YEAR, STRATUM, length_bin) %>%
    mutate(sum_EXPNUMLEN = sum(NUMBER.AT.LENGTH, na.rm = TRUE)) %>%
    distinct(YEAR, STRATUM, length_bin, .keep_all = TRUE)
  # remove individual data fields that were aggregated to create bins so not confusing
  mas_length_bin_strata <- mas_length_bin_strata %>%
    select(-LENGTH..cm., -NUMBER.AT.LENGTH, -STATION)
  
  # expanded length bin counts
  # merge
  mas_length_bin_strata <- merge(mas_length_bin_strata, mas_tow_bin_strata, by = c("STRATUM", "YEAR"), all = T)
  
  #### -----------------------------------------------------------------
  # apply expansion factor
  mas_length_bin_strata$EXP_sum_EXPNUMLEN <- mas_length_bin_strata$sum_EXPNUMLEN*mas_length_bin_strata$exp_fact
  ####
}  


{
  # Calculate the sex ratio in 5 cm length bins for each year and stratum
  mas_ratio_by_bin <- aggregate(SEX.CODE ~ length_bin + YEAR + STRATUM, data = mas_bio, 
                                function(x) sum(x == 2) / (sum(x == 1) + sum(x == 2)), 
                                drop = FALSE)
  
  # Expanded numbers at length by year
  mas_length_bin_strata$EXP_sum_EXPNUMLEN
  
  # Merge sex ratio with numbers at length
  mas_length_bin_count <- left_join(mas_length_bin_strata, mas_ratio_by_bin, by = c("length_bin", "YEAR", "STRATUM"))
  
  # Sex ratio conditioned on expanded numbers at length by year
  # female count
  mas_length_bin_count$f_count <- mas_length_bin_count$EXP_sum_EXPNUMLEN * mas_length_bin_count$SEX
  # male count
  mas_length_bin_count$m_count <- mas_length_bin_count$EXP_sum_EXPNUMLEN - mas_length_bin_count$f_count
  
  # corrected sex ratio by stratum
  mas_ratio_corrected <- mas_length_bin_count %>%
    group_by(YEAR, STRATUM, length_bin) %>%
    mutate(ratio = sum(f_count, na.rm = TRUE) / 
             (sum(f_count, na.rm = TRUE) + sum(m_count, na.rm = TRUE)))
  
  # Sex ratio conditioned on numbers at length by year and length bin
  mas_ratio_corrected2 <- mas_length_bin_count %>%
    group_by(YEAR, length_bin) %>%
    summarise(ratio = sum(f_count, na.rm = TRUE) / 
                (sum(f_count, na.rm = TRUE) + sum(m_count, na.rm = TRUE)))
  
  # Add grouping variable for plotting NA ratios
  # size bins as sampled for all years
  mas_ratio_corrected2$ratio_available <- ifelse(mas_ratio_corrected2$ratio == "NaN", 0, 1)
  
  # Sex ratio conditioned on numbers at length by year and length bin, 30+ cm fish
  mas_ratio_corrected2m30 <- mas_ratio_corrected2[!(mas_ratio_corrected2$length_bin == "0-4" |
                                                      mas_ratio_corrected2$length_bin == "5-9" |
                                                      mas_ratio_corrected2$length_bin == "10-14" |
                                                      mas_ratio_corrected2$length_bin == "15-19" |
                                                      mas_ratio_corrected2$length_bin == "20-24" |
                                                      mas_ratio_corrected2$length_bin == "25-29"), ]
  
  # Sex ratio conditioned on numbers at length by year and length bin, <30 cm fish
  mas_ratio_corrected2s30 <- mas_ratio_corrected2[(mas_ratio_corrected2$length_bin == "0-4" |
                                                     mas_ratio_corrected2$length_bin == "5-9" |
                                                     mas_ratio_corrected2$length_bin == "10-14" |
                                                     mas_ratio_corrected2$length_bin == "15-19" |
                                                     mas_ratio_corrected2$length_bin == "20-24" |
                                                     mas_ratio_corrected2$length_bin == "25-29"), ]
  
  # What uncorrected ratio was
  mas_ratio_uncorrected <- mas_bio %>%
    group_by(YEAR) %>%
    summarise(
      f_count = sum(SEX.CODE == 2, na.rm = TRUE),
      m_count = sum(SEX.CODE == 1, na.rm = TRUE),
      ratio_uncorrected = f_count / (f_count + m_count))
  
  
  # Reformat for plotting
  mas_ratio <- left_join(mas_ratio_corrected2, mas_ratio_uncorrected, by = "YEAR")
  mas_ratio <- mas_ratio %>% select(YEAR, ratio, ratio_uncorrected)
  # reshape the data frame from wide to long format, keeping the "EST_YEAR" column unchanged
  mas_ratio2 <- melt(mas_ratio, id.vars = "YEAR")
  
  # annual average
  mas_ratio_corrected3 <- mas_ratio_corrected2 %>%
    group_by(YEAR) %>%
    summarise(ratio = mean(ratio, na.rm = T))
  
  mas_ratio_corrected3m30 <- mas_ratio_corrected2m30 %>%
    group_by(YEAR) %>%
    summarise(ratio = mean(ratio, na.rm = T))
  
  mas_ratio_corrected3s30 <- mas_ratio_corrected2s30 %>%
    group_by(YEAR) %>%
    summarise(ratio = mean(ratio, na.rm = T))
  
  mas_ratio_corrected2 <- mas_ratio_corrected2[!is.na(mas_ratio_corrected2$length_bin),]
  
}

# Visualize
# Sampling and ratio across length bins

(plot <- ggplot(mas_ratio_corrected2, aes(as.numeric(YEAR), length_bin, fill = ratio)) +
    geom_tile() +
    labs(title = "Spring BTS MADMF, Sex ratio by length bin",
         x = "Year",
         y = "Length bin (cm)") +
    scale_x_continuous(breaks = seq(1963, max(mas_ratio_corrected2$YEAR), by = 5),
                       guide = guide_axis(angle = 45)) +
    scale_fill_continuous(name = "Ratio") +
    theme_classic())
ggsave("mas_sampledvnot.png", plot, width = 6, height = 4, dpi = 300)

# Female ratio corrected vs not corrected
(plot <- ggplot(mas_ratio2, aes(x = as.factor(YEAR), y = value, color = variable)) +
    #geom_point() +
    geom_line(aes(group = variable), stat = "summary") +
    labs(title = "Spring BTS MADMF",
         x = "Year",
         y = "Sex ratio (F/M)") +
    scale_x_discrete(labels = function(x) ifelse(seq_along(x) %% 5 == 0, x, ""),
                     guide = guide_axis(angle = 45)) +
    scale_color_discrete(name = "") +
    theme_classic())
ggsave("mas_ratio_correctvnot.png", plot, width = 8, height = 4, dpi = 300)

#Corrected female ratio
(plot <- ggplot(mas_ratio_corrected2, aes(x = as.factor(YEAR), y = ratio)) +
    geom_boxplot(alpha = 0.4) +
    #geom_point(data = mas_ratio_corrected3, aes(x = as.factor(EST_YEAR), y = ratio), color = "blue") +
    geom_line(data = mas_ratio_corrected3, aes(x = as.factor(YEAR), y = ratio, group = 1), color = "blue", lwd = 1.5) +
    labs(title = "Spring BTS MADMF",
         x = "Year",
         y = "Sex ratio (F/M)") +
    scale_x_discrete(labels = function(x) ifelse(seq_along(x) %% 5 == 0, x, ""),
                     guide = guide_axis(angle = 45)) +
    scale_color_discrete(name = "") +
    theme_classic() +
    geom_hline(yintercept = 0.5, color = "red"))
ggsave("mas_combined.png", plot, width = 8, height = 4, dpi = 300)

(plot <- ggplot(mas_ratio_corrected2m30, aes(x = as.factor(YEAR), y = ratio)) + 
    geom_boxplot() +
    geom_line(data = mas_ratio_corrected3m30, aes(x = as.factor(YEAR), y = ratio, group = 1), color = "blue", lwd = 1.5) +
    labs(title = "Spring BTS MADMF, 30+ cm",
         x = "Year",
         y = "Sex ratio (F/M)") +
    scale_x_discrete(labels = function(x) ifelse(seq_along(x) %% 5 == 0, x, ""),
                     guide = guide_axis(angle = 45)) +
    scale_color_discrete(name = "") +
    theme_classic() +
    geom_hline(yintercept = 0.5, color = "red"))
ggsave("mas_m30.png", plot, width = 8, height = 4, dpi = 300)

(plot <- ggplot(mas_ratio_corrected2s30, aes(x = as.factor(YEAR), y = ratio)) + 
    geom_boxplot() +
    geom_line(data = mas_ratio_corrected3s30, aes(x = as.factor(YEAR), y = ratio, group = 1), color = "blue", lwd = 1.5) +
    labs(title = "Spring BTS MADMF, <30 cm",
         x = "Year",
         y = "Sex ratio (F/M)") +
    scale_x_discrete(labels = function(x) ifelse(seq_along(x) %% 5 == 0, x, ""),
                     guide = guide_axis(angle = 45)) +
    scale_color_discrete(name = "") +
    theme_classic() +
    geom_hline(yintercept = 0.5, color = "red"))
ggsave("mas_s30.png", plot, width = 8, height = 4, dpi = 300)


# Define the number of bins
num_bins <- 11

# Calculate bin width
bin_width <- 1 / num_bins

# Create bins
bin_intervals <- seq(0, 1, by = bin_width)
bin_intervals <- cbind(bin_intervals[-length(bin_intervals)], bin_intervals[-1])

# Convert bins to labels for visualization
bin_labels <- paste0(round(bin_intervals[, 1], 4), "-", round(bin_intervals[, 2], 4))

# Cut the data into bins
mas_ratio_corrected2m30_counts <- mas_ratio_corrected2m30 %>%
  mutate(bin = cut(ratio, breaks = c(0, bin_intervals[, 2]), labels = bin_labels, include.lowest = TRUE)) %>%
  group_by(bin) %>%
  summarize(count = n())

(plot <- ggplot(data = mas_ratio_corrected2m30, aes(x = ratio)) +
    geom_histogram(binwidth = 0.1, fill = "#3C7DB1", color = "black") +
    labs(title = "Spring BTS MADMF, 30+ cm",
         x = "Sex ratio (F/M)",
         y = "Frequency") +
    theme_classic())
ggsave("mas_m30_hist.png", plot, width = 4, height = 4, dpi = 300)

# Cut the data into bins
mas_ratio_corrected2s30_counts <- mas_ratio_corrected2s30 %>%
  mutate(bin = cut(ratio, breaks = c(0, bin_intervals[, 2]), labels = bin_labels, include.lowest = TRUE)) %>%
  group_by(bin) %>%
  summarize(count = n())

(plot <- ggplot(data = mas_ratio_corrected2s30, aes(x = ratio)) +
    geom_histogram(binwidth = 0.1, fill = "#3C7DB1", color = "black") +
    labs(title = "Spring BTS MADMF, <30 cm",
         x = "Sex ratio (F/M)",
         y = "Frequency") +
    theme_classic())
ggsave("mas_s30_hist.png", plot, width = 4, height = 4, dpi = 300)


#### ME/NH FALL ####

# upload and clean mnf = ME/NH fall 2011, 2005-2006
{
  # Tow data
  mnf_tow <- read.csv(here("C:/Users/kzarrellasmi/Documents/GitHub/WF_MENH_State_Trawl/data", "MaineDMR_Trawl_Survey_Tow_Data_2024-05-06.csv"))
  # Subset to fall
  mnf_tow <- mnf_tow[mnf_tow$Season == "Fall",]
  
  #### -----------------------------
  #bad tows?
  
  # columns of interest
  mnf_tow <- mnf_tow[, c("Depth_Stratum", "Tow_Number", "Year")]
  names(mnf_tow)[names(mnf_tow) == "Depth_Stratum"] <- "Stratum"
  
  # Length data
  mnf_len <- read.csv(here("C:/Users/kzarrellasmi/Documents/GitHub/WF_MENH_State_Trawl/data", "MaineDMR_Trawl_Survey_Catch_at_Length_Data_2024-05-06.csv"))
  # Subset to fall
  mnf_len <- mnf_len[mnf_len$Season == "Fall",]
  # Subset to  winter flounder
  mnf_len <- mnf_len[mnf_len$Common_Name == "Flounder Winter",]
  # add in tows with no winter flounder
  mnf_len <- merge(mnf_tow, mnf_len, by = c("Year", "Stratum", "Tow_Number"), all = T)
  # Replace NA in numbers at length with 0 for tows with no winter flounder
  mnf_len$Frequency[is.na(mnf_len$Frequency)] <- 0
  # columns of interest
  mnf_len <- mnf_len[, c("Stratum", "Tow_Number", "Year", "Length", "Frequency")]
  
  # Biological data
  mnf_bio <- read.csv(here("C:/Users/kzarrellasmi/Documents/GitHub/WF_MENH_State_Trawl/data", "MaineDMR_Trawl_Survey_Maturity_Data_2024-05-06.csv"))
  # Subset to fall
  mnf_bio <- mnf_bio[mnf_bio$Season == "Fall",]
  # Subset to  winter flounder
  mnf_bio <- mnf_bio[mnf_bio$Common_name == "Flounder Winter",]
  # Replace NA in sex with 999 for tows with no winter flounder
  mnf_bio$Sex[is.na(mnf_bio$Sex)] <- 999
}


# Create length bins
{
  # get range for bins
  range(mnf_len$Length, na.rm = T)
  # Create 5 cm bins
  mnf_len <- mnf_len %>%
    mutate(
      length_bin = cut(
        Length,
        breaks = c(seq(0, 49, by = 5), Inf),
        labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"),
        include.lowest = TRUE,
        right = FALSE  # Ensure right-closed intervals
      )
    )
  
  # get range for bins
  range(mnf_bio$Length, na.rm = T)
  # Create 5 cm bins
  mnf_bio <- mnf_bio %>%
    mutate(
      length_bin = cut(
        Length,
        breaks = c(seq(0, 44, by = 5), Inf),
        labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44"),
        include.lowest = TRUE,
        right = FALSE  # Ensure right-closed intervals
      )
    )
}


{
  # Adjust for uneven sampling by weighting core strata
  # some strata have more tows/km2 than others, some strata were sampled in some years and not others
  
  # [TOTAL_AREA_OF_STRATUM]/[TOTAL_TOWS_BY_STRATA_AND_YEAR]
  mnf_tow_bin_strata <- mnf_tow %>%
    group_by(Year, Stratum) %>%
    summarise(total = n())
  
  #### -------------------------
  # Stratum data
  mnf_strat <- read.csv(here("C:/Users/kzarrellasmi/Documents/GitHub/WF_Federal_Trawl/Annual", "SVDBS_SVMSTRATA.csv"))
  # columns of interest
  mnf_strat <- mnf_strat[ , c("stratum", "stratum_area")]
  names(mnf_strat)[1] <- "Stratum"
  # merge area of stratum with number of tows (removing stratum outside of GOM)
  mnf_tow_bin_strata <- merge(mnf_tow_bin_strata, mnf_strat, by = "Stratum", all.x = T)
  # generate expansion factor for length bin counts based on tow coverage for each stratum
  mnf_tow_bin_strata$exp_fact <- (mnf_tow_bin_strata$stratum_area/mnf_tow_bin_strata$total)/100
  mnf_tow_bin_strata <- mnf_tow_bin_strata[ , c("Stratum", "EST_YEAR", "exp_fact")]
  ####
  
  # number per length bin by strata
  mnf_length_bin_strata <- mnf_len %>%
    group_by(Year, Stratum, length_bin) %>%
    mutate(sum_Frequency = sum(Frequency, na.rm = TRUE)) %>%
    distinct(Year, Stratum, length_bin, .keep_all = TRUE)
  # remove individual data fields that were aggregated to create bins so not confusing
  mnf_length_bin_strata <- mnf_length_bin_strata %>%
    select(-Length, -Frequency, -Tow_Number)
  
  # expanded length bin counts
  # merge
  mnf_length_bin_strata <- merge(mnf_length_bin_strata, mnf_tow_bin_strata, by = c("Stratum", "Year"), all = T)
  
  #### -----------------------------------------
  # apply expansion factor
  mnf_length_bin_strata$EXP_sum_Frequency <- mnf_length_bin_strata$sum_Frequency*mnf_length_bin_strata$exp_fact
  ####
}  


{
  # Calculate the sex ratio in 5 cm length bins for each year and stratum
  mnf_ratio_by_bin <- aggregate(Sex ~ length_bin + Year + Stratum, data = mnf_bio, 
                                function(x) sum(x == "Female") / (sum(x == "Male") + sum(x == "Female")), 
                                drop = FALSE)
  
  # Expanded numbers at length by year
  mnf_length_bin_strata$EXP_sum_Frequency
  
  # Merge sex ratio with numbers at length
  mnf_length_bin_count <- left_join(mnf_length_bin_strata, mnf_ratio_by_bin, by = c("length_bin", "Year", "Stratum"))
  
  # Sex ratio conditioned on expanded numbers at length by year
  # female count
  mnf_length_bin_count$f_count <- mnf_length_bin_count$EXP_sum_Frequency * mnf_length_bin_count$Sex
  # male count
  mnf_length_bin_count$m_count <- mnf_length_bin_count$EXP_sum_Frequency - mnf_length_bin_count$f_count
  
  # corrected sex ratio by stratum
  mnf_ratio_corrected <- mnf_length_bin_count %>%
    group_by(Year, Stratum, length_bin) %>%
    mutate(ratio = sum(f_count, na.rm = TRUE) / 
             (sum(f_count, na.rm = TRUE) + sum(m_count, na.rm = TRUE)))
  
  # Sex ratio conditioned on numbers at length by year and length bin
  mnf_ratio_corrected2 <- mnf_length_bin_count %>%
    group_by(Year, length_bin) %>%
    summarise(ratio = sum(f_count, na.rm = TRUE) / 
                (sum(f_count, na.rm = TRUE) + sum(m_count, na.rm = TRUE)))
  
  # Add grouping variable for plotting NA ratios
  # size bins as sampled for all years
  mnf_ratio_corrected2$ratio_available <- ifelse(mnf_ratio_corrected2$ratio == "NaN", 0, 1)
  
  # Sex ratio conditioned on numbers at length by year and length bin, 30+ cm fish
  mnf_ratio_corrected2m30 <- mnf_ratio_corrected2[!(mnf_ratio_corrected2$length_bin == "0-4" |
                                                      mnf_ratio_corrected2$length_bin == "5-9" |
                                                      mnf_ratio_corrected2$length_bin == "10-14" |
                                                      mnf_ratio_corrected2$length_bin == "15-19" |
                                                      mnf_ratio_corrected2$length_bin == "20-24" |
                                                      mnf_ratio_corrected2$length_bin == "25-29"), ]
  
  # Sex ratio conditioned on numbers at length by year and length bin, <30 cm fish
  mnf_ratio_corrected2s30 <- mnf_ratio_corrected2[(mnf_ratio_corrected2$length_bin == "0-4" |
                                                     mnf_ratio_corrected2$length_bin == "5-9" |
                                                     mnf_ratio_corrected2$length_bin == "10-14" |
                                                     mnf_ratio_corrected2$length_bin == "15-19" |
                                                     mnf_ratio_corrected2$length_bin == "20-24" |
                                                     mnf_ratio_corrected2$length_bin == "25-29"), ]
  
  # What uncorrected ratio was
  mnf_ratio_uncorrected <- mnf_bio %>%
    group_by(Year) %>%
    summarise(
      f_count = sum(Sex == "Female", na.rm = TRUE),
      m_count = sum(Sex == "Male", na.rm = TRUE),
      ratio_uncorrected = f_count / (f_count + m_count))
  
  
  # Reformat for plotting
  mnf_ratio <- left_join(mnf_ratio_corrected2, mnf_ratio_uncorrected, by = "Year")
  mnf_ratio <- mnf_ratio %>% select(Year, ratio, ratio_uncorrected)
  # reshape the data frame from wide to long format, keeping the "Year" column unchanged
  mnf_ratio2 <- melt(mnf_ratio, id.vars = "Year")
  
  # annual average
  mnf_ratio_corrected3 <- mnf_ratio_corrected2 %>%
    group_by(Year) %>%
    summarise(ratio = mean(ratio, na.rm = T))
  
  mnf_ratio_corrected3m30 <- mnf_ratio_corrected2m30 %>%
    group_by(Year) %>%
    summarise(ratio = mean(ratio, na.rm = T))
  
  mnf_ratio_corrected3s30 <- mnf_ratio_corrected2s30 %>%
    group_by(Year) %>%
    summarise(ratio = mean(ratio, na.rm = T))
  
  mnf_ratio_corrected2 <- mnf_ratio_corrected2[!is.na(mnf_ratio_corrected2$length_bin),]
  
}

# Visualize
# Sampling and ratio across length bins

(plot <- ggplot(mnf_ratio_corrected2, aes(as.numeric(Year), length_bin, fill = ratio)) +
    geom_tile() +
    labs(title = "Fall BTS ME/NH, Sex ratio by length bin",
         x = "Year",
         y = "Length bin (cm)") +
    scale_x_continuous(breaks = seq(1963, max(mnf_ratio_corrected2$Year), by = 5),
                       guide = guide_axis(angle = 45)) +
    scale_fill_continuous(name = "Ratio") +
    theme_classic())
ggsave("mnf_sampledvnot.png", plot, width = 6, height = 4, dpi = 300)

# Female ratio corrected vs not corrected
(plot <- ggplot(mnf_ratio2, aes(x = as.factor(Year), y = value, color = variable)) +
    #geom_point() +
    geom_line(aes(group = variable), stat = "summary") +
    labs(title = "Fall BTS ME/NH",
         x = "Year",
         y = "Sex ratio (F/M)") +
    scale_x_discrete(labels = function(x) ifelse(seq_along(x) %% 5 == 0, x, ""),
                     guide = guide_axis(angle = 45)) +
    scale_color_discrete(name = "") +
    theme_classic())
ggsave("mnf_ratio_correctvnot.png", plot, width = 8, height = 4, dpi = 300)

#Corrected female ratio
(plot <- ggplot(mnf_ratio_corrected2, aes(x = as.factor(Year), y = ratio)) +
    geom_boxplot(alpha = 0.4) +
    #geom_point(data = mnf_ratio_corrected3, aes(x = as.factor(Year), y = ratio), color = "blue") +
    geom_line(data = mnf_ratio_corrected3, aes(x = as.factor(Year), y = ratio, group = 1), color = "blue", lwd = 1.5) +
    labs(title = "Fall BTS ME/NH",
         x = "Year",
         y = "Sex ratio (F/M)") +
    scale_x_discrete(labels = function(x) ifelse(seq_along(x) %% 5 == 0, x, ""),
                     guide = guide_axis(angle = 45)) +
    scale_color_discrete(name = "") +
    theme_classic() +
    geom_hline(yintercept = 0.5, color = "red"))
ggsave("mnf_combined.png", plot, width = 8, height = 4, dpi = 300)

(plot <- ggplot(mnf_ratio_corrected2m30, aes(x = as.factor(Year), y = ratio)) + 
    geom_boxplot() +
    geom_line(data = mnf_ratio_corrected3m30, aes(x = as.factor(Year), y = ratio, group = 1), color = "blue", lwd = 1.5) +
    labs(title = "Fall BTS ME/NH, 30+ cm",
         x = "Year",
         y = "Sex ratio (F/M)") +
    scale_x_discrete(labels = function(x) ifelse(seq_along(x) %% 5 == 0, x, ""),
                     guide = guide_axis(angle = 45)) +
    scale_color_discrete(name = "") +
    theme_classic() +
    geom_hline(yintercept = 0.5, color = "red"))
ggsave("mnf_m30.png", plot, width = 8, height = 4, dpi = 300)

(plot <- ggplot(mnf_ratio_corrected2s30, aes(x = as.factor(Year), y = ratio)) + 
    geom_boxplot() +
    geom_line(data = mnf_ratio_corrected3s30, aes(x = as.factor(Year), y = ratio, group = 1), color = "blue", lwd = 1.5) +
    labs(title = "Fall BTS ME/NH, <30 cm",
         x = "Year",
         y = "Sex ratio (F/M)") +
    scale_x_discrete(labels = function(x) ifelse(seq_along(x) %% 5 == 0, x, ""),
                     guide = guide_axis(angle = 45)) +
    scale_color_discrete(name = "") +
    theme_classic() +
    geom_hline(yintercept = 0.5, color = "red"))
ggsave("mnf_s30.png", plot, width = 8, height = 4, dpi = 300)


# Define the number of bins
num_bins <- 11

# Calculate bin width
bin_width <- 1 / num_bins

# Create bins
bin_intervals <- seq(0, 1, by = bin_width)
bin_intervals <- cbind(bin_intervals[-length(bin_intervals)], bin_intervals[-1])

# Convert bins to labels for visualization
bin_labels <- paste0(round(bin_intervals[, 1], 4), "-", round(bin_intervals[, 2], 4))

# Cut the data into bins
mnf_ratio_corrected2m30_counts <- mnf_ratio_corrected2m30 %>%
  mutate(bin = cut(ratio, breaks = c(0, bin_intervals[, 2]), labels = bin_labels, include.lowest = TRUE)) %>%
  group_by(bin) %>%
  summarize(count = n())

(plot <- ggplot(data = mnf_ratio_corrected2m30, aes(x = ratio)) +
    geom_histogram(binwidth = 0.1, fill = "#3C7DB1", color = "black") +
    labs(title = "Fall BTS ME/NH, 30+ cm",
         x = "Sex ratio (F/M)",
         y = "Frequency") +
    theme_classic())
ggsave("mnf_m30_hist.png", plot, width = 4, height = 4, dpi = 300)

# Cut the data into bins
mnf_ratio_corrected2s30_counts <- mnf_ratio_corrected2s30 %>%
  mutate(bin = cut(ratio, breaks = c(0, bin_intervals[, 2]), labels = bin_labels, include.lowest = TRUE)) %>%
  group_by(bin) %>%
  summarize(count = n())

(plot <- ggplot(data = mnf_ratio_corrected2s30, aes(x = ratio)) +
    geom_histogram(binwidth = 0.1, fill = "#3C7DB1", color = "black") +
    labs(title = "Fall BTS ME/NH, <30 cm",
         x = "Sex ratio (F/M)",
         y = "Frequency") +
    theme_classic())
ggsave("mnf_s30_hist.png", plot, width = 4, height = 4, dpi = 300)


#### ME/NH SPRING ####

# upload and clean mnf = ME/NH Spring 1978-[2020]-
{
  # Tow data
  mns_tow <- read.csv(here("C:/Users/kzarrellasmi/Documents/GitHub/WF_MENH_State_Trawl/data", "MaineDMR_Trawl_Survey_Tow_Data_2024-05-06.csv"))
  # Subset to Spring
  mns_tow <- mns_tow[mns_tow$Season == "Spring",]
  
  #### -----------------------------
  #bad tows?
  
  # columns of interest
  mns_tow <- mns_tow[, c("Depth_Stratum", "Tow_Number", "Year")]
  names(mns_tow)[names(mns_tow) == "Depth_Stratum"] <- "Stratum"
  
  # Length data
  mns_len <- read.csv(here("C:/Users/kzarrellasmi/Documents/GitHub/WF_MENH_State_Trawl/data", "MaineDMR_Trawl_Survey_Catch_at_Length_Data_2024-05-06.csv"))
  # Subset to Spring
  mns_len <- mns_len[mns_len$Season == "Spring",]
  # Subset to  winter flounder
  mns_len <- mns_len[mns_len$Common_Name == "Flounder Winter",]
  # add in tows with no winter flounder
  mns_len <- merge(mns_tow, mns_len, by = c("Year", "Stratum", "Tow_Number"), all = T)
  # Replace NA in numbers at length with 0 for tows with no winter flounder
  mns_len$Frequency[is.na(mns_len$Frequency)] <- 0
  # columns of interest
  mns_len <- mns_len[, c("Stratum", "Tow_Number", "Year", "Length", "Frequency")]
  
  # Biological data
  mns_bio <- read.csv(here("C:/Users/kzarrellasmi/Documents/GitHub/WF_MENH_State_Trawl/data", "MaineDMR_Trawl_Survey_Maturity_Data_2024-05-06.csv"))
  # Subset to Spring
  mns_bio <- mns_bio[mns_bio$Season == "Spring",]
  # Subset to  winter flounder
  mns_bio <- mns_bio[mns_bio$Common_name == "Flounder Winter",]
  # Replace NA in sex with 999 for tows with no winter flounder
  mns_bio$Sex[is.na(mns_bio$Sex)] <- 999
}


# Create length bins
{
  # get range for bins
  range(mns_len$Length, na.rm = T)
  # Create 5 cm bins
  mns_len <- mns_len %>%
    mutate(
      length_bin = cut(
        Length,
        breaks = c(seq(0, 49, by = 5), Inf),
        labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"),
        include.lowest = TRUE,
        right = FALSE  # Ensure right-closed intervals
      )
    )
  
  # get range for bins
  range(mns_bio$Length, na.rm = T)
  # Create 5 cm bins
  mns_bio <- mns_bio %>%
    mutate(
      length_bin = cut(
        Length,
        breaks = c(seq(0, 49, by = 5), Inf),
        labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"),
        include.lowest = TRUE,
        right = FALSE  # Ensure right-closed intervals
      )
    )
}


{
  # Adjust for uneven sampling by weighting core strata
  # some strata have more tows/km2 than others, some strata were sampled in some years and not others
  
  # [TOTAL_AREA_OF_STRATUM]/[TOTAL_TOWS_BY_STRATA_AND_YEAR]
  mns_tow_bin_strata <- mns_tow %>%
    group_by(Year, Stratum) %>%
    summarise(total = n())
  
  #### ----------------------------------
  # Stratum data
  mns_strat <- read.csv(here("C:/Users/kzarrellasmi/Documents/GitHub/WF_Federal_Trawl/Annual", "SVDBS_SVMSTRATA.csv"))
  # columns of interest
  mns_strat <- mns_strat[ , c("stratum", "stratum_area")]
  names(mns_strat)[1] <- "Stratum"
  # merge area of stratum with number of tows (removing stratum outside of GOM)
  mns_tow_bin_strata <- merge(mns_tow_bin_strata, mns_strat, by = "Stratum", all.x = T)
  # generate expansion factor for length bin counts based on tow coverage for each stratum
  mns_tow_bin_strata$exp_fact <- (mns_tow_bin_strata$stratum_area/mns_tow_bin_strata$total)/100
  mns_tow_bin_strata <- mns_tow_bin_strata[ , c("Stratum", "EST_YEAR", "exp_fact")]
  ####
  
  # number per length bin by strata
  mns_length_bin_strata <- mns_len %>%
    group_by(Year, Stratum, length_bin) %>%
    mutate(sum_Frequency = sum(Frequency, na.rm = TRUE)) %>%
    distinct(Year, Stratum, length_bin, .keep_all = TRUE)
  # remove individual data fields that were aggregated to create bins so not confusing
  mns_length_bin_strata <- mns_length_bin_strata %>%
    select(-Length, -Frequency, -Tow_Number)
  
  # expanded length bin counts
  # merge
  mns_length_bin_strata <- merge(mns_length_bin_strata, mns_tow_bin_strata, by = c("Stratum", "Year"), all = T)
  
  #### ---------------------
  # apply expansion factor
  mns_length_bin_strata$EXP_sum_Frequency <- mns_length_bin_strata$sum_Frequency*mns_length_bin_strata$exp_fact
  ####
}  


{
  # Calculate the sex ratio in 5 cm length bins for each year and stratum
  mns_ratio_by_bin <- aggregate(Sex ~ length_bin + Year + Stratum, data = mns_bio, 
                                function(x) sum(x == "Female") / (sum(x == "Male") + sum(x == "Female")), 
                                drop = FALSE)
  
  # Expanded numbers at length by year
  mns_length_bin_strata$EXP_sum_Frequency
  
  # Merge sex ratio with numbers at length
  mns_length_bin_count <- left_join(mns_length_bin_strata, mns_ratio_by_bin, by = c("length_bin", "Year", "Stratum"))
  
  # Sex ratio conditioned on expanded numbers at length by year
  # female count
  mns_length_bin_count$f_count <- mns_length_bin_count$EXP_sum_Frequency * mns_length_bin_count$Sex
  # male count
  mns_length_bin_count$m_count <- mns_length_bin_count$EXP_sum_Frequency - mns_length_bin_count$f_count
  
  # corrected sex ratio by stratum
  mns_ratio_corrected <- mns_length_bin_count %>%
    group_by(Year, Stratum, length_bin) %>%
    mutate(ratio = sum(f_count, na.rm = TRUE) / 
             (sum(f_count, na.rm = TRUE) + sum(m_count, na.rm = TRUE)))
  
  # Sex ratio conditioned on numbers at length by year and length bin
  mns_ratio_corrected2 <- mns_length_bin_count %>%
    group_by(Year, length_bin) %>%
    summarise(ratio = sum(f_count, na.rm = TRUE) / 
                (sum(f_count, na.rm = TRUE) + sum(m_count, na.rm = TRUE)))
  
  # Add grouping variable for plotting NA ratios
  # size bins as sampled for all years
  mns_ratio_corrected2$ratio_available <- ifelse(mns_ratio_corrected2$ratio == "NaN", 0, 1)
  
  # Sex ratio conditioned on numbers at length by year and length bin, 30+ cm fish
  mns_ratio_corrected2m30 <- mns_ratio_corrected2[!(mns_ratio_corrected2$length_bin == "0-4" |
                                                      mns_ratio_corrected2$length_bin == "5-9" |
                                                      mns_ratio_corrected2$length_bin == "10-14" |
                                                      mns_ratio_corrected2$length_bin == "15-19" |
                                                      mns_ratio_corrected2$length_bin == "20-24" |
                                                      mns_ratio_corrected2$length_bin == "25-29"), ]
  
  # Sex ratio conditioned on numbers at length by year and length bin, <30 cm fish
  mns_ratio_corrected2s30 <- mns_ratio_corrected2[(mns_ratio_corrected2$length_bin == "0-4" |
                                                     mns_ratio_corrected2$length_bin == "5-9" |
                                                     mns_ratio_corrected2$length_bin == "10-14" |
                                                     mns_ratio_corrected2$length_bin == "15-19" |
                                                     mns_ratio_corrected2$length_bin == "20-24" |
                                                     mns_ratio_corrected2$length_bin == "25-29"), ]
  
  # What uncorrected ratio was
  mns_ratio_uncorrected <- mns_bio %>%
    group_by(Year) %>%
    summarise(
      f_count = sum(Sex == "Female", na.rm = TRUE),
      m_count = sum(Sex == "Male", na.rm = TRUE),
      ratio_uncorrected = f_count / (f_count + m_count))
  
  
  # Reformat for plotting
  mns_ratio <- left_join(mns_ratio_corrected2, mns_ratio_uncorrected, by = "Year")
  mns_ratio <- mns_ratio %>% select(Year, ratio, ratio_uncorrected)
  # reshape the data frame from wide to long format, keeping the "Year" column unchanged
  mns_ratio2 <- melt(mns_ratio, id.vars = "Year")
  
  # annual average
  mns_ratio_corrected3 <- mns_ratio_corrected2 %>%
    group_by(Year) %>%
    summarise(ratio = mean(ratio, na.rm = T))
  
  mns_ratio_corrected3m30 <- mns_ratio_corrected2m30 %>%
    group_by(Year) %>%
    summarise(ratio = mean(ratio, na.rm = T))
  
  mns_ratio_corrected3s30 <- mns_ratio_corrected2s30 %>%
    group_by(Year) %>%
    summarise(ratio = mean(ratio, na.rm = T))
  
  mns_ratio_corrected2 <- mns_ratio_corrected2[!is.na(mns_ratio_corrected2$length_bin),]
  
}

# Visualize
# Sampling and ratio across length bins

(plot <- ggplot(mns_ratio_corrected2, aes(as.numeric(Year), length_bin, fill = ratio)) +
    geom_tile() +
    labs(title = "Spring BTS ME/NH, Sex ratio by length bin",
         x = "Year",
         y = "Length bin (cm)") +
    scale_x_continuous(breaks = seq(1963, max(mns_ratio_corrected2$Year), by = 5),
                       guide = guide_axis(angle = 45)) +
    scale_fill_continuous(name = "Ratio") +
    theme_classic())
ggsave("mns_sampledvnot.png", plot, width = 6, height = 4, dpi = 300)

# Female ratio corrected vs not corrected
(plot <- ggplot(mns_ratio2, aes(x = as.factor(Year), y = value, color = variable)) +
    #geom_point() +
    geom_line(aes(group = variable), stat = "summary") +
    labs(title = "Spring BTS ME/NH",
         x = "Year",
         y = "Sex ratio (F/M)") +
    scale_x_discrete(labels = function(x) ifelse(seq_along(x) %% 5 == 0, x, ""),
                     guide = guide_axis(angle = 45)) +
    scale_color_discrete(name = "") +
    theme_classic())
ggsave("mns_ratio_correctvnot.png", plot, width = 8, height = 4, dpi = 300)

#Corrected female ratio
(plot <- ggplot(mns_ratio_corrected2, aes(x = as.factor(Year), y = ratio)) +
    geom_boxplot(alpha = 0.4) +
    #geom_point(data = mns_ratio_corrected3, aes(x = as.factor(Year), y = ratio), color = "blue") +
    geom_line(data = mns_ratio_corrected3, aes(x = as.factor(Year), y = ratio, group = 1), color = "blue", lwd = 1.5) +
    labs(title = "Spring BTS ME/NH",
         x = "Year",
         y = "Sex ratio (F/M)") +
    scale_x_discrete(labels = function(x) ifelse(seq_along(x) %% 5 == 0, x, ""),
                     guide = guide_axis(angle = 45)) +
    scale_color_discrete(name = "") +
    theme_classic() +
    geom_hline(yintercept = 0.5, color = "red"))
ggsave("mns_combined.png", plot, width = 8, height = 4, dpi = 300)

(plot <- ggplot(mns_ratio_corrected2m30, aes(x = as.factor(Year), y = ratio)) + 
    geom_boxplot() +
    geom_line(data = mns_ratio_corrected3m30, aes(x = as.factor(Year), y = ratio, group = 1), color = "blue", lwd = 1.5) +
    labs(title = "Spring BTS ME/NH, 30+ cm",
         x = "Year",
         y = "Sex ratio (F/M)") +
    scale_x_discrete(labels = function(x) ifelse(seq_along(x) %% 5 == 0, x, ""),
                     guide = guide_axis(angle = 45)) +
    scale_color_discrete(name = "") +
    theme_classic() +
    geom_hline(yintercept = 0.5, color = "red"))
ggsave("mns_m30.png", plot, width = 8, height = 4, dpi = 300)

(plot <- ggplot(mns_ratio_corrected2s30, aes(x = as.factor(Year), y = ratio)) + 
    geom_boxplot() +
    geom_line(data = mns_ratio_corrected3s30, aes(x = as.factor(Year), y = ratio, group = 1), color = "blue", lwd = 1.5) +
    labs(title = "Spring BTS ME/NH, <30 cm",
         x = "Year",
         y = "Sex ratio (F/M)") +
    scale_x_discrete(labels = function(x) ifelse(seq_along(x) %% 5 == 0, x, ""),
                     guide = guide_axis(angle = 45)) +
    scale_color_discrete(name = "") +
    theme_classic() +
    geom_hline(yintercept = 0.5, color = "red"))
ggsave("mns_s30.png", plot, width = 8, height = 4, dpi = 300)


# Define the number of bins
num_bins <- 11

# Calculate bin width
bin_width <- 1 / num_bins

# Create bins
bin_intervals <- seq(0, 1, by = bin_width)
bin_intervals <- cbind(bin_intervals[-length(bin_intervals)], bin_intervals[-1])

# Convert bins to labels for visualization
bin_labels <- paste0(round(bin_intervals[, 1], 4), "-", round(bin_intervals[, 2], 4))

# Cut the data into bins
mns_ratio_corrected2m30_counts <- mns_ratio_corrected2m30 %>%
  mutate(bin = cut(ratio, breaks = c(0, bin_intervals[, 2]), labels = bin_labels, include.lowest = TRUE)) %>%
  group_by(bin) %>%
  summarize(count = n())

(plot <- ggplot(data = mns_ratio_corrected2m30, aes(x = ratio)) +
    geom_histogram(binwidth = 0.1, fill = "#3C7DB1", color = "black") +
    labs(title = "Spring BTS ME/NH, 30+ cm",
         x = "Sex ratio (F/M)",
         y = "Frequency") +
    theme_classic())
ggsave("mns_m30_hist.png", plot, width = 4, height = 4, dpi = 300)

# Cut the data into bins
mns_ratio_corrected2s30_counts <- mns_ratio_corrected2s30 %>%
  mutate(bin = cut(ratio, breaks = c(0, bin_intervals[, 2]), labels = bin_labels, include.lowest = TRUE)) %>%
  group_by(bin) %>%
  summarize(count = n())

(plot <- ggplot(data = mns_ratio_corrected2s30, aes(x = ratio)) +
    geom_histogram(binwidth = 0.1, fill = "#3C7DB1", color = "black") +
    labs(title = "Spring BTS ME/NH, <30 cm",
         x = "Sex ratio (F/M)",
         y = "Frequency") +
    theme_classic())
ggsave("mns_s30_hist.png", plot, width = 4, height = 4, dpi = 300)
