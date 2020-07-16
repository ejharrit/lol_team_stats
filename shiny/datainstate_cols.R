library(ggplot2)

theme_set(theme_minimal())

datainstate_colors <- c(
    `green`        = rgb(52,168,83, max = 255),
    `red`      = rgb(233,67,53, max = 255),
    `blue`       = rgb(67,133,245, max = 255),
    `yellow`     = rgb(252,188,5, max = 255),
    `orange`     = rgb(251,146,28, max = 255),
    `grey` = rgb(51,51,51, max = 255)
    )

datainstate_cols <- function(...) {
    cols <- c(...)
    
    if (is.null(cols))
        return (datainstate_colors)
    
    datainstate_colors[cols]
}

datainstate_palettes <- list(
    `bl_rd` = datainstate_cols("blue", "red"),
    `grn_ylw` = datainstate_cols("green", "yellow"),
    `bl_gry_rd` = datainstate_cols("blue", "grey", "red"),
    `bl_grn_rd` = datainstate_cols("blue", "green", "red"),
    `gl_ylw_rd` = datainstate_cols("blue", "yellow", "red")
    
)

datainstate_pal <- function(palette = "bl_rd", reverse = FALSE, ...) {
    pal <- datainstate_palettes[[palette]]
    
    if (reverse) pal <- rev(pal)
    
    colorRampPalette(pal, ...)
}

scale_color_datainstate <- function(palette = "bl_gry_rd", discrete = TRUE, 
                              reverse = FALSE, ...) {
    pal <- datainstate_pal(palette = palette, reverse = reverse)
    
    if (discrete) {
        discrete_scale("colour", paste0("datainstate_", palette), 
                       palette = pal, ...)
    } else {
        scale_color_gradientn(colours = pal(256), ...)
    }
}

scale_fill_datainstate <- function(palette = "bl_gry_rd", discrete = TRUE, 
                             reverse = FALSE, ...) {
    pal <- datainstate_pal(palette = palette, reverse = reverse)
    
    if (discrete) {
        discrete_scale("fill", paste0("datainstate_", palette), 
                       palette = pal, ...)
    } else {
        scale_fill_gradientn(colours = pal(256), ...)
    }
}

