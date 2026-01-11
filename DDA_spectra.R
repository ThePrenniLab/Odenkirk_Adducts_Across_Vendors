library(dplyr)
library(ggplot2)

df <- read.csv("205sggz_ms2_191.csv")

df_long <- bind_rows(
  df %>%
    transmute(
      mz = irts_mz,
      intensity = irts_intensity,
      series = "IRTS"
    ),
  df %>%
    transmute(
      mz = blank_mz,
      intensity = -blank_intensity,  # make blank negative
      series = "Blank"
    )
)

max_val <- max(abs(df_long$intensity), na.rm = TRUE)

custom_colors <- c(
  "IRTS"  = "#355070",  
  "Blank" = "#A5BC32"   # red
)

axis_col <- "black" 
text_col <- "black"

x_ref1 <- 608.4 
x_ref2 <-191.0


ggplot(df_long, aes(x = mz, y = intensity, color = series)) +

  geom_vline(
    xintercept = x_ref2,
    color = "grey85",
    linewidth = 1
  ) +
  
  geom_line(linewidth = .6) +
  geom_point(size = 0) +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = custom_colors) +
  scale_y_continuous(limits = c(-max_val, max_val))+
  scale_y_continuous(
    labels = scales::scientific          # y-axis in scientific notation
  ) +
  scale_x_continuous(
    limits = c(50, 200),      
    breaks = seq(0, 1200, by = 400)  
  ) +
  theme_bw() +
  theme(
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(color = axis_col, linewidth = 0.8),
      axis.text.x = element_text(color = text_col, face = "bold"),
      axis.text.y = element_text(color = text_col, face = "bold"),
      axis.title.x = element_text(color = text_col, face = "bold"),
      axis.title.y = element_text(color = text_col, face = "bold"),
      legend.position = "none",
      legend.text = element_blank(),
      legend.key = element_blank(),
      legend.background = element_blank()
    ) +
  labs(
    x = "Mass to Charge (m/z)",
    y = "Intensity"
  )


ggsave(
  "205sggz_ms2_191_main.png",
  width = 3,
  height = 2,
  units = "in",
  dpi = 900
)
