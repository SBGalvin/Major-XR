#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Hex sticker download
# Run once

hex_folder <- "hex_files"
##
download.file(
  url = "https://github.com/rstudio/hex-stickers/archive/master.zip",
  destfile = file.path(hex_folder, "rstudio-hex.zip")
)
##
unzip(
  zipfile = file.path(hex_folder, "rstudio-hex.zip"),
  exdir = hex_folder
)

path <- file.path(hex_folder, "hex-stickers-master", "PNG")

# Remove the non-image files
unlink(list.files(path, full.names = TRUE)[tools::file_ext(list.files(path)) %in% c("md", "Rmd")])

list.files(path)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(magick)
library(purrr)

sticker_files <- list.files(path)
stickers <- file.path(path, sticker_files) %>% 
  map(function(path){
    switch(tools::file_ext(path),
           svg = image_read_svg(path),
           pdf = image_read_pdf(path),
           image_read(path))
  }) %>%
  map(image_transparent, "white") %>%
  map(image_trim) %>%
  set_names(sticker_files)

# Desired sticker resolution in pixels
sticker_width <- 121

# Scale all stickers to the desired pixel width
stickers <- stickers %>%
  map(image_scale, sticker_width)

# Identify low resolution stickers
stickers %>%
  map_lgl(~ with(image_info(.x),
                 width < (sticker_width-1)/2 && format != "svg")
  )

# Identify incorrect shapes / proportions (tolerance of +-2 height)
stickers %>%
  map_lgl(~ with(image_info(.x),
                 height < (median(height)-2) | height > (median(height) + 2))
  )

# Extract correct sticker height (this could also be calculated directly from width)
sticker_height <- stickers %>%
  map(image_info) %>%
  map_dbl("height") %>%
  median

# Coerce sticker dimensions
stickers <- stickers %>%
  map(image_resize, paste0(sticker_width, "x", sticker_height, "!"))

stickers[["tidyverse.png"]] ## Went this far


sticker_row_size <- 9
# Calculate row sizes
sticker_col_size <- ceiling(length(stickers)/(sticker_row_size-0.5))
row_lens <- rep(c(sticker_row_size,sticker_row_size-1), length.out=sticker_col_size)
row_lens[length(row_lens)] <- row_lens[length(row_lens)]  - (length(stickers) - sum(row_lens))

sticker_rows <- map2(row_lens, cumsum(row_lens),
                     ~ seq(.y-.x+1, by = 1, length.out = .x)) %>%
  map(~ stickers[.x] %>%
        invoke(c, .) %>%
        image_append)

sticker_rows[[1]]

# Add stickers to canvas
canvas <- image_blank(sticker_row_size*sticker_width, 
                      sticker_height + (sticker_col_size-1)*sticker_height/1.33526,
                      "#2A2A2A")

reduce2(sticker_rows, seq_along(sticker_rows), 
        ~ image_composite(
          ..1, ..2,
          offset = paste0("+", ((..3-1)%%2)*sticker_width/2,
                          "+", round((..3-1)*sticker_height/1.33526))
        ),
        .init = canvas)
