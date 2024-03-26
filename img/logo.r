library(hexSticker)
library(ggplot2)
library(glue)
library(magick)
library(fs)

img <- image_read("img/logo_2.png")
logo <- image_ggplot(img, interpolate = TRUE)

sticker(
  logo,
  package = "zmpg",
  p_size = 20,
  s_width = 0.8,
  s_height = 0.8,
  s_x = 1,
  s_y = 0.84,
  h_fill =  "#EBCF9D",
  h_color = "#C6786B",
  p_color = "#335955",
  p_family = "Aller_Rg",
  h_size = 2.4,
  white_around_sticker = T,
  filename = "img/logo/logo_final.png",
  url = "    https://prdm0.github.io/zmpg/",
  u_size = 4.5,
  spotlight = T,
  l_alpha = 0.6,
  dpi = 300,
  u_color = "#0F2536"
)
