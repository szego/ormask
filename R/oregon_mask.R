#' Masks out the area just outside Oregon with a solid color.
#'
#' Include this function in a ggplot2 sequence to plot a mask of the area outside Oregon.
#'   Also includes the option to draw roads and bodies of water.
#'
#' * `road_size`: The thickness of the road lines.
#' * `road_color`: The color of the road lines. When set to NA (the default),
#'   roads are not rendered. Set this to any color to include roads in the plot.
#' * `water_color`: The color of the water. When set to NA (the default), water
#'   is not rendered. Set this to any color to include roads in the plot. Water
#'   may take quite a while to render.
#' * `bg_color`: The color of the mask surrounding Oregon.
#' * `xlim`: Limits of the x-axis. Defaults to include all of Oregon.
#' * `ylim`: Limits of the y-axis. Defaults to include all of Oregon.
#'
#' @return A list of ggplot2 geoms of types [ggplot2::geom_sf()] and [ggplot2::coord_sf()].
#' @export
#' @examples
#' ggplot() + oregon_mask()
#'
oregon_mask <- function(
    road_size = 0.01,
    road_color = NA,
    water_color = NA,
    bg_color = "#222222",
    xlim = c(-124.6, -116.4),
    ylim = c(41.9, 46.3)
) {
    p <- list(
        ggplot2::geom_sf(
            data = or_mask,
            color = NA,
            fill = bg_color
        ),
        ggplot2::coord_sf(
            xlim = xlim,
            ylim = ylim,
            crs = sf::st_crs(or_signed_routes)
        )
    )

    if(!is.na(water_color))
        p <- c(
            ggplot2::geom_sf(
                data = or_water_bodies,
                fill = water_color,
                color = NA
            ),
            p
        )

    if(!is.na(road_color))
        p <- c(
            ggplot2::geom_sf(
                data = or_signed_routes,
                color = road_color,
                fill = NA,
                size = road_size
            ),
            p
        )

    return(p)
}
