require(proto)
require(ggplot2)
geom_rug2 <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", sides = "bl", ...) {
  GeomRug2$new(mapping = mapping, data = data, stat = stat, position = position, sides = sides, ...)
}

GeomRug2 <- proto(ggplot2:::Geom, {
  objname <- "rug2"
  draw <- function(., data, scales, coordinates, sides, ...) {
    rugs <- list()
    data <- coord_transform(coordinates, data, scales)
    if (!is.null(data$x)) {
      if(grepl("b", sides)) {
        rugs$x_b <- segmentsGrob(
          x0 = unit(data$x, "native"), x1 = unit(data$x, "native"),
          y0 = unit(0, "npc"), y1 = unit(0, "npc")+unit(0.3, "cm"),
          gp = gpar(col = alpha(data$colour, data$alpha), lty = data$linetype, lwd = data$size * .pt)
        )
      }
      
      if(grepl("t", sides)) {
        rugs$x_t <- segmentsGrob(
          x0 = unit(data$x, "native"), x1 = unit(data$x, "native"),
          y0 = unit(1, "npc"), y1 = unit(1, "npc")-unit(0.3, "cm"),
          gp = gpar(col = alpha(data$colour, data$alpha), lty = data$linetype, lwd = data$size * .pt)
        )
      }
    }
    
    if (!is.null(data$y)) {
      if(grepl("l", sides)) {
        rugs$y_l <- segmentsGrob(
          y0 = unit(data$y, "native"), y1 = unit(data$y, "native"),
          x0 = unit(0, "npc"), x1 = unit(0, "npc")+unit(0.3, "cm"),
          gp = gpar(col = alpha(data$colour, data$alpha), lty = data$linetype, lwd = data$size * .pt)
        )
      }
      
      if(grepl("r", sides)) {
        rugs$y_r <- segmentsGrob(
          y0 = unit(data$y, "native"), y1 = unit(data$y, "native"),
          x0 = unit(1, "npc"), x1 = unit(1, "npc")-unit(0.3, "cm"),
          gp = gpar(col = alpha(data$colour, data$alpha), lty = data$linetype, lwd = data$size * .pt)
        )
      }
    }
    
    gTree(children = do.call("gList", rugs))
  }
  
  default_stat <- function(.) StatIdentity
  default_aes <- function(.) aes(colour="black", size=0.5, linetype=1, alpha = NA)
  guide_geom <- function(.) "path"
})
