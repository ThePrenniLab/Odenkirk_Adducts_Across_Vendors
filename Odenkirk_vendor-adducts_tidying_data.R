# File Description: 
### This document serves to clean and map manifest file data to Skyline outputs for IRTS signals. 
### This will also include:
# - filtering out adducts with mass error >15 ppm (likely other signals) for each IRTS species
# - correcting for dilution factors

# Install if needed:

# install.packages("tidyverse")
# install.packages("plotly)



library(tidyverse)


# Read in files
sample_info <- read.csv("adducts_cross_lab_manifest.csv")
sample_info <- sample_info %>%
  mutate(
    lab_filename_clean = lab_filename %>%
      basename() %>%                     
      str_remove("\\.[^.]*$")            
  )


negative_skyline <- read.csv("adducts_negative_skyline_output.csv")
positive_skyline <- read.csv("adducts_positive_skyline_output.csv")


# Clean and merge Skyline data analysis outputs
clean_numeric <- function(x) {
  x <- str_replace_all(x, "#N/A|NA|na", "")
  as.numeric(x)
}

numeric_cols <- c("Retention.Time", "Total.Area.MS1", "Height", "Mass.Error.PPM")

negative_skyline <- negative_skyline %>%
  mutate(across(all_of(numeric_cols), clean_numeric))

positive_skyline <- positive_skyline %>%
  mutate(across(all_of(numeric_cols), clean_numeric))


clean_character <- function(x) {
  x %>%
    str_to_lower() %>%               
    str_replace_all("_|-", " ") %>%  
    str_trim()                        
}

char_cols <- c("Molecule", "Molecule.List.Name") 

negative_skyline <- negative_skyline %>%
  mutate(across(all_of(char_cols), clean_character))

positive_skyline <- positive_skyline %>%
  mutate(across(all_of(char_cols), clean_character))


negative_skyline <- negative_skyline %>%
  mutate(mode = "negative")

positive_skyline <- positive_skyline %>%
  mutate(mode = "positive")



df <- bind_rows(negative_skyline, positive_skyline)


# Clean outputs to intensity and mass error cutoff values

df_int_filt <- df %>%
  filter(
    (mode == "positive" & !(Height < 1000 | Total.Area.MS1 < 2000)) |
      (mode == "negative" & !(Height < 5000 | Total.Area.MS1 < 11000))
  )

df_filtered <- df_int_filt  %>%
  filter(!is.na(Mass.Error.PPM)) %>%           
  filter(Mass.Error.PPM <= 15 & Mass.Error.PPM >= -15)


# Add sample info data

df_final <- df_filtered %>%
  inner_join(
    sample_info %>% select(lab_filename_clean, everything()), 
    by = c("Replicate.Name" = "lab_filename_clean")
  )


# Correct intensities for optimal resuspension and injection volumes in a lab

df_final <- df_final %>%
  mutate(
    dilution_factor = (resuspension_volume / injection_volume) / sample_conc_factor
  )

df_final <- df_final %>%
  mutate(
    Height_corrected = Height / dilution_factor,
    Area_corrected = Total.Area.MS1 / dilution_factor
  )

df_final <- df_final %>%
  group_by(access_folder, polarity) %>%
  mutate(
    Height_norm = Height_corrected / sum(Height_corrected, na.rm = TRUE)* 1e6,
    Area_norm = Area_corrected / sum(Area_corrected, na.rm = TRUE) * 1e6
  ) %>%
  ungroup()

write.csv(df_final, "adduct_vendors_tidied_data_output.csv")

