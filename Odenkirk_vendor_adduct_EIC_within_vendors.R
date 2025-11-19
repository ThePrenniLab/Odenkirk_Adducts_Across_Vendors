library(tidyverse)


#negative agilent 326_MADN

df <- read_csv("326_madn_neg_agilents.csv")


datasets <- list(
  UCA = c("UCA_rt", "UCA_waterloss", "UCA_deprot", "UCA_Cl", "UCA_FA", "UCA_Ac"),
  ICL = c("ICL_rt", "ICL_waterloss", "ICL_deprot", "ICL_Cl", "ICL_FA", "ICL_Ac"),
  MAH = c("MAH_rt", "MAH_waterloss", "MAH_deprot", "MAH_Cl", "MAH_FA", "MAH_Ac"),
  VAN = c("VAN_rt", "VAN_waterloss", "VAN_deprot", "VAN_Cl", "VAN_FA", "VAN_Ac")
)


clean_dataset <- function(df, cols, dataset_name) {
  
  rt_col <- cols[1]         
  intensity_cols <- cols[-1] 
  
  df %>%
    select(all_of(cols)) %>%
    rename(RT = !!rt_col) %>%
    pivot_longer(cols = all_of(intensity_cols),
                 names_to = "Adduct",
                 values_to = "Intensity") %>%
    filter(RT >= 5, RT <= 6) %>%
    mutate(Dataset = dataset_name)
}


results <- map2(
  datasets,
  names(datasets),
  ~clean_dataset(df, .x, .y)
)

names(results) <- names(datasets)

uca_df <- results$UCA
icl_df <- results$ICL
mah_df <- results$MAH
van_df <- results$VAN


adduct_colors <- c(
  UCA_waterloss = "#EDE580",
  UCA_deprot    = "#B56576",
  UCA_Cl        = "#E0B7B7",
  UCA_FA        = "#FAA275",
  UCA_Ac        = "#E56B6F",
  
  ICL_waterloss = "#EDE580",
  ICL_deprot    = "#B56576",
  ICL_Cl        = "#E0B7B7",
  ICL_FA        = "#FAA275",
  ICL_Ac        = "#E56B6F",
  
  MAH_waterloss = "#EDE580",
  MAH_deprot    = "#B56576",
  MAH_Cl        = "#E0B7B7",
  MAH_FA        = "#FAA275",
  MAH_Ac        = "#E56B6F",
  
  VAN_waterloss = "#EDE580",
  VAN_deprot    = "#B56576",
  VAN_Cl        = "#E0B7B7",
  VAN_FA        = "#FAA275",
  VAN_Ac        = "#E56B6F"
)

plot_dataset <- function(data,
                         line_width = 1,
                         title_size = 18,
                         axis_title_size = 9,
                         axis_text_size = 7) {
  
  ggplot(data, aes(x = RT, y = Intensity, color = Adduct)) +
    
    geom_line(linewidth = line_width) +
    
    scale_color_manual(values = adduct_colors, guide = "none") +  # removes legend
    
    scale_y_continuous(labels = scales::label_scientific(format = TRUE)) +             # scientific notation for intensity
    scale_x_continuous(labels = function(x) sprintf("%.1f", x)) + # one decimal for RT
    
    theme_classic(base_size = 7) +
    
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_line(color = "black", linewidth = 0.5),
      axis.text = element_text(size = axis_text_size, face = "bold"),
      axis.title = element_text(size = axis_title_size, face = "bold"),
      plot.title = element_blank()  # removes title
    ) +
    
    labs(
      x = "Retention Time (min)",
      y = "Intensity"
    )
}


plot_UCA <- plot_dataset(uca_df)
plot_ICL <- plot_dataset(icl_df)
plot_MAH <- plot_dataset(mah_df)
plot_VAN <- plot_dataset(van_df)

plots <- list(
  UCA = plot_UCA,
  ICL = plot_ICL,
  MAH = plot_MAH,
  VAN = plot_VAN
)

for (name in names(plots)) {
  ggsave(
    filename = paste0(name, "_NEG_326_madn_plot.png"),
    plot = plots[[name]],
    width = 1.8, height = 1.8, dpi = 1200
  )
}


#thermo negative madn

df <- read_csv("326_madn_neg_thermo.csv")


datasets <- list(
  DTU = c("DTU_rt", "DTU_waterloss", "DTU_deprot", "DTU_Cl", "DTU_FA", "DTU_Ac"),
  WCMC = c("WCMC_rt", "WCMC_waterloss", "WCMC_deprot", "WCMC_Cl", "WCMC_FA", "WCMC_Ac"),
  WUR = c("WUR_rt", "WUR_waterloss", "WUR_deprot", "WUR_Cl", "WUR_FA", "WUR_Ac")
)


results <- map2(
  datasets,
  names(datasets),
  ~clean_dataset(df, .x, .y)
)

names(results) <- names(datasets)

DTU_df <- results$DTU
WCMC_df <- results$WCMC
WUR_df <- results$WUR


adduct_colors <- c(
  DTU_waterloss = "#EDE580",
  DTU_deprot    = "#B56576",
  DTU_Cl        = "#E0B7B7",
  DTU_FA        = "#FAA275",
  DTU_Ac        = "#E56B6F",
  
  WCMC_waterloss = "#EDE580",
  WCMC_deprot    = "#B56576",
  WCMC_Cl        = "#E0B7B7",
  WCMC_FA        = "#FAA275",
  WCMC_Ac        = "#E56B6F",
  
  WUR_waterloss = "#EDE580",
  WUR_deprot    = "#B56576",
  WUR_Cl        = "#E0B7B7",
  WUR_FA        = "#FAA275",
  WUR_Ac        = "#E56B6F"
)

plot_dataset <- function(data,
                         line_width = 1,
                         title_size = 18,
                         axis_title_size = 9,
                         axis_text_size = 7) {
  
  ggplot(data, aes(x = RT, y = Intensity, color = Adduct)) +
    
    geom_line(linewidth = line_width) +
    
    scale_color_manual(values = adduct_colors, guide = "none") +  # removes legend
    
    scale_y_continuous(labels = scales::label_scientific(format = TRUE)) +             # scientific notation for intensity
    scale_x_continuous(labels = function(x) sprintf("%.1f", x)) + # one decimal for RT
    
    theme_classic(base_size = 7) +
    
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_line(color = "black", linewidth = 0.5),
      axis.text = element_text(size = axis_text_size, face = "bold"),
      axis.title = element_text(size = axis_title_size, face = "bold"),
      plot.title = element_blank()  # removes title
    ) +
    
    labs(
      x = "Retention Time (min)",
      y = "Intensity"
    )
}


plot_DTU <- plot_dataset(DTU_df)
plot_WCMC <- plot_dataset(WCMC_df)
plot_WUR <- plot_dataset(WUR_df)

plots <- list(
  DTU = plot_DTU,
  WCMC = plot_WCMC,
  WUR = plot_WUR
)

for (name in names(plots)) {
  ggsave(
    filename = paste0(name, "_NEG_326_madn_plot.png"),
    plot = plots[[name]],
    width = 2, height = 2, dpi = 300
  )
}
