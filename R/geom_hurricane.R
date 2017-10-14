#' Wind radii charts
#'
#' This wind radii chart displays how far winds of a certain intensity (e.g., 34, 50, or 64 knots)
#' extended from a hurricane’s center, with separate values given for the northeast, northwest,
#' southeast, and southwest quadrants of the storm.
#'
#' @section Aesthetics:
#' \aesthetics{geom}{hurricane}
#'
#' @param mapping  Set of aesthetic mappings created by aes or aes_.
#' If specified and inherit.aes = TRUE (the default), it is combined
#' with the default mapping at the top level of the plot. You must
#' supply mapping if there is no plot mapping.
#'
#' @param data The data to be displayed in this layer. There are three options:
#' If NULL, the default, the data is inherited from the plot data as specified in the call to ggplot.
#'
#' A data.frame, or other object, will override the plot data. All objects will be fortified to produce
#' a data frame. See fortify for which variables will be created.
#'
#' A function will be called with a single argument, the plot data. The return value must be a data.frame.,
#' and will be used as the layer data.
#'
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#'
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function.
#'
#' @param na.rm If FALSE, the default, missing values are removed with a warning.
#' If TRUE, missing values are silently removed
#'
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them.
#' This is most useful for helper functions that define both data and aesthetics and shouldn't inherit
#' behaviour from the default plot specification, e.g. borders.
#' @param scale_radii A numeric of scale factor for wind radii chart, default value is 10%.
#'
#' @param ... other arguments passed on to layer. These are often aesthetics, used to set an aesthetic to
#' a fixed value, like color = "red" or size = 3. They may also be parameters to the paired geom/stat.
#' @export
#'
#' @examples
#' \dontrun {
#' # hurrican data of one observation
#' library(ggmap)
#' library(ggplot2)
#' ike_2008_09_13 <- hurricanes[hurricanes$storm_id=="IKE-2008" &
#' hurricanes$date_time=="2008-09-13 12:00:00", ]
#' # Get base map near Houston area where hurrican IKE passed by
#' map_data <- get_map("Houston", zoom = 6, maptype = "toner-background")
#' # Plot the hurricane
#' base_map <- ggmap(map_data, extent = "device")
#' base_map +
#' geom_hurricane(data = ike_2008_09_13, aes(x = longitude, y = latitude,
#' r_ne = ne, r_se = se, r_nw = nw, r_sw = sw, fill = wind_speed2,
#' color = wind_speed2), scale_radii = 0.2) +
#' scale_color_manual(name = "Wind speed (kts)",
#'                    values = c("red", "orange", "yellow")) +
#' scale_fill_manual(name = "Wind speed (kts)",
#'                    values = c("red", "orange", "yellow"))
#' }
geom_hurricane <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity",  na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE, scale_radii=0.1, ...) {
  ggplot2::layer(
    geom = GeomHurricane, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(scale_radii=scale_radii, na.rm = na.rm, ...)
  )
}


#' GeomHurricane object
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export

GeomHurricane <- ggplot2::ggproto("GeomHurricane", Geom,
                                  required_aes = c("x", "y","r_ne", "r_se", "r_nw", "r_sw","fill", "colour"),
                                  default_aes = ggplot2::aes(color = NA, fill = "grey20", size = 0.5,
                                                             linetype = 1, alpha = 1),
                                  draw_key = draw_key_polygon,

                                  draw_group = function(data, panel_scales, coord, scale_radii=0.1) {
                                     ## Transform the data first
                                     coords <- coord$transform(data, panel_scales)
                                     ## Compute mapping data for four directions
                                    x=coords$x
                                    y=coords$y
                                    r_ne=coords$r_ne * 1609.34 * scale_radii
                                    r_nw=coords$r_nw * 1609.34 * scale_radii
                                    r_sw=coords$r_sw * 1609.34 * scale_radii
                                    r_se=coords$r_se * 1609.34 * scale_radii
                                    data_all = data.frame()
                                    for (i in 1:nrow(coords)) {
                                      data_ne=geosphere::destPoint(c(x[i],y[i]), b=1:89, d=r_ne[i])
                                      data_nw=geosphere::destPoint(c(x[i],y[i]), b=90:179, d=r_nw[i])
                                      data_sw=geosphere::destPoint(c(x[i],y[i]), b=180:239, d=r_sw[i])
                                      data_se=geosphere::destPoint(c(x[i],y[i]), b=240:360, d=r_se[i])
                                      data=rbind(data_ne, data_nw, data_sw, data_se)
                                      data <- as.data.frame(data)
                                      data$group=rep(coords$group[i], nrow(data))
                                      data_all=bind_rows(data_all, data)
                                    }

                                     ## Construct a grid grob
                                    grid::polygonGrob(data_all$lon,
                                                data_all$lat,
                                                #  id=data_all$group,
                                                default.units = "native",
                                                gp=grid::gpar(
                                                  fill=coords[, "fill"],
                                                  color=coords[, "colour"]
                                                ))
                                  })

