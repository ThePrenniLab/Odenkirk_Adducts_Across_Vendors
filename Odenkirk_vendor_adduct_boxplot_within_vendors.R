library(tidyverse)


df <- read_csv("adduct_vendors_tidied_data_output.csv")

#negative 326_MADN 
df_clean <- df %>%
  filter(Molecule == "326 madn") %>%   
  filter(mode == "negative") %>%          
  filter(str_detect(group_id, regex("^B", ignore_case = TRUE))) %>%  
  filter(!str_detect(group_id, regex("^C", ignore_case = TRUE)))  %>% 
  filter(!str_detect(instrument, regex("Shimadzu|Waters", ignore_case = TRUE)))


df_norm <- df_clean %>%
  group_by(lab_folder) %>%
  mutate(
    total_area = sum(Area_norm, na.rm = TRUE),
    norm_area_percent = 100 * Area_norm / total_area
  ) %>%
  ungroup()

df_norm <- df_norm %>%
  mutate(
    Adduct_Lab = paste0(Precursor.Adduct, " | ", lab_folder),
    Adduct_Lab = factor(Adduct_Lab, levels = unique(Adduct_Lab))  # preserves order
  )

lab_order <- c("cuanschutz", "imperial", "mahidol",  "vanderbilt", 
               "dtu-biosustain", "westcoastmetabolomicscenter", "wur")  # example order

df_norm <- df_norm %>%
  mutate(lab_folder = factor(lab_folder, levels = lab_order))

adduct_colors <- c(
  "[M-H]"    = "#B56576",
  "[M+Cl]"   = "#E0B7B7",
  "[M+FA-H]" = "#FAA275"
)

lab_plot <- ggplot(df_norm, aes(x = lab_folder, y = norm_area_percent, color = Precursor.Adduct)) +
  geom_boxplot(outlier.shape = NA, linewidth = 0.8, position = position_dodge(width = 0.8)) +
  geom_jitter(aes(group = Precursor.Adduct),  # group by adduct for dodging
              size = 1.2, 
              position = position_dodge(width = 0.8)) +  # remove width argument
  geom_point(aes(fill = Precursor.Adduct),  # fill for the interior
             shape = 21,                     # circle with fill + outline
             size = 2,                        # slightly larger
             stroke = .6,
             color = "black",
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8)) +                    # thickness of outline
  scale_color_manual(values = adduct_colors) +
  scale_fill_manual(values = adduct_colors) +  # make fill match outline
  theme_classic() +
  theme(
    axis.line= element_line(color = "black", linewidth = .8),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8, face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 11, face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 9)
  ) +
  
  labs(
    x = "Lab",
    y = "Normalized Area (%)",
    color = "Adduct"
  )


ggsave(
  filename = "normalized_adducts_boxplot.png",   # name of the file
  plot = lab_plot,                              # the plot object
  width = 7, height = 3.5,                        # size in inches
  dpi = 900                                     # resolution
)


#positive 306_OOPX (thermo and shimadzu)
df_clean <- df %>%
  filter(Molecule == "306 oopx") %>%   
  filter(mode == "positive") %>%          
  filter(str_detect(group_id, regex("^B", ignore_case = TRUE))) %>%  
  filter(!str_detect(group_id, regex("^C", ignore_case = TRUE)))  %>% 
  filter(!str_detect(instrument, regex("Agilent|Waters", ignore_case = TRUE)))


df_norm <- df_clean %>%
  group_by(lab_folder) %>%
  mutate(
    total_area = sum(Area_norm, na.rm = TRUE),
    norm_area_percent = 100 * Area_norm / total_area
  ) %>%
  ungroup()

df_norm <- df_norm %>%
  mutate(
    Adduct_Lab = paste0(Precursor.Adduct, " | ", lab_folder),
    Adduct_Lab = factor(Adduct_Lab, levels = unique(Adduct_Lab))  # preserves order
  )

lab_order <- c("javeriana", "naro", 
               "dtu-biosustain", "westcoastmetabolomicscenter", "wur")  # example order

df_norm <- df_norm %>%
  mutate(lab_folder = factor(lab_folder, levels = lab_order))

adduct_colors <- c(
  "[M+H]"    = "#355070",
  "[M+NH4]"   = "#087E8B",
  "[M+K]"   = "#C5D86D",
  "[M+Na]" = "#8CBA80"
)

lab_plot <- ggplot(df_norm, aes(x = lab_folder, y = norm_area_percent, color = Precursor.Adduct)) +
  geom_boxplot(outlier.shape = NA, linewidth = 0.8, position = position_dodge(width = 0.8)) +
  geom_jitter(aes(group = Precursor.Adduct),  # group by adduct for dodging
              size = 1.2, 
              position = position_dodge(width = 0.8)) +  # remove width argument
  geom_point(aes(fill = Precursor.Adduct),  # fill for the interior
             shape = 21,                     # circle with fill + outline
             size = 2,                        # slightly larger
             stroke = .6,
             color = "black",
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8)) +                    # thickness of outline
  scale_color_manual(values = adduct_colors) +
  scale_fill_manual(values = adduct_colors) +  # make fill match outline
  theme_classic() +
  theme(
    axis.line= element_line(color = "black", linewidth = .8),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8, face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 11, face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 9)
  ) +
  
  labs(
    x = "Lab",
    y = "Normalized Area (%)",
    color = "Adduct"
  )


ggsave(
  filename = "306_oopx_normalized_adducts_boxplot_long.png",   # name of the file
  plot = lab_plot,                              # the plot object
  width = 6.5, height = 3.5,                        # size in inches
  dpi = 900                                     # resolution
)




#positive 484_FKUB (thermo and agilent)
df_clean <- df %>%
  filter(Molecule == "484 fkub") %>%   
  filter(mode == "positive") %>%          
  filter(str_detect(group_id, regex("^B", ignore_case = TRUE))) %>%  
  filter(!str_detect(group_id, regex("^C", ignore_case = TRUE)))  %>% 
  filter(!str_detect(instrument, regex("Shimadzu|Waters", ignore_case = TRUE)))


df_norm <- df_clean %>%
  group_by(lab_folder) %>%
  mutate(
    total_area = sum(Area_norm, na.rm = TRUE),
    norm_area_percent = 100 * Area_norm / total_area
  ) %>%
  ungroup()

df_norm <- df_norm %>%
  mutate(
    Adduct_Lab = paste0(Precursor.Adduct, " | ", lab_folder),
    Adduct_Lab = factor(Adduct_Lab, levels = unique(Adduct_Lab))  # preserves order
  )

lab_order <- c("cuanschutz", "imperial", "mahidol",  "vanderbilt",  
               "dtu-biosustain", "westcoastmetabolomicscenter", "wur")  # example order

df_norm <- df_norm %>%
  mutate(lab_folder = factor(lab_folder, levels = lab_order))

adduct_colors <- c(
  "[M+H]"    = "#355070",
  "[M+NH4]"   = "#087E8B",
  "[M+K]"   = "#C5D86D",
  "[M+Na]" = "#8CBA80"
)

lab_plot <- ggplot(df_norm, aes(x = lab_folder, y = norm_area_percent, color = Precursor.Adduct)) +
  geom_boxplot(outlier.shape = NA, linewidth = 0.8, position = position_dodge(width = 0.8)) +
  geom_jitter(aes(group = Precursor.Adduct),  # group by adduct for dodging
              size = 1.2, 
              position = position_dodge(width = 0.8)) +  # remove width argument
  geom_point(aes(fill = Precursor.Adduct),  # fill for the interior
             shape = 21,                     # circle with fill + outline
             size = 2,                        # slightly larger
             stroke = .6,
             color = "black",
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8)) +                    # thickness of outline
  scale_color_manual(values = adduct_colors) +
  scale_fill_manual(values = adduct_colors) +  # make fill match outline
  theme_classic() +
  theme(
    axis.line= element_line(color = "black", linewidth = .8),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8, face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 11, face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 9)
  ) +
  
  labs(
    x = "Lab",
    y = "Normalized Area (%)",
    color = "Adduct"
  )


ggsave(
  filename = "484fkub_normalized_adducts_boxplot_long.png",   # name of the file
  plot = lab_plot,                              # the plot object
  width = 7.7, height = 3.5,                        # size in inches
  dpi = 900                                     # resolution
)

