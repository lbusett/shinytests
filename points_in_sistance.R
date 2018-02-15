points_in_distance <- function(in_pts,
                               dist,
                               ncuts = 10) {

  # convert points to data.table and create a unique identifier
  pts <-  data.table(in_pts)
  pts <- pts[, or_id := 1:dim(in_pts)[1]]

  # divide the extent in quadrants in ncuts*ncuts quadrants and assign each
  # point to a quadrant, then create the index over "xcut"
  range_x  <- range(pts$x)
  limits_x <-(range_x[1] + (0:ncuts)*(range_x[2] - range_x[1])/ncuts)
  range_y  <- range(pts$y)
  limits_y <- range_y[1] + (0:ncuts)*(range_y[2] - range_y[1])/ncuts
  pts[, `:=`(xcut =  as.integer(cut(x, ncuts, labels = 1:ncuts)),
             ycut = as.integer(cut(y, ncuts, labels = 1:ncuts)))]  %>%
    setkey(x, y)


  results <- list()
  count <- 0
  # start cycling over quadrants
  for (cutx in unique(pts$xcut)) {

    # get the points included in a x-slice, then create an index over ycut
    count <- 0
    subpts_x <- pts[xcut == cutx] %>%
      setkey(ycut)

    # get the points included in a x-slice, extended by `dist`, and build
    # an index over y
    min_x_comp    <- ifelse(cutx == 1, limits_x[cutx], (limits_x[cutx] - dist))
    max_x_comp    <- ifelse(cutx == ncuts, limits_x[cutx + 1], (limits_x[cutx + 1] + dist))
    subpts_x_comp <- pts[x >= min_x_comp & x < max_x_comp] %>%
      setkey(y)

    # # # get the points included in a x-slice, then create an index over ycut
    # subpts_x <- subpts_x_comp[xcut == cutx] %>%
    #    setkey(ycut)

    for (cuty in unique(pts$ycut)) {
      count <- count + 1

      # Now find the points with which we need to "compare" the points in
      # subpts_y_buf
      min_y_comp  <- ifelse(cuty == 1, limits_y[cuty], (limits_x[cuty] - dist))
      max_y_comp  <- ifelse(cuty == ncuts, limits_x[cuty + 1], (limits_x[cuty + 1] + dist))
      subpts_comp <- subpts_x_comp[y >= min_y_comp & y < max_y_comp]
      # %>%
      #   sf::st_as_sf()
       # subset over `y` coordinates and buffer. subpts_y_buf contains the points
      # which "neighbours" we want to find.
      subpts_y <- subpts_x_comp[ycut == cuty & xcut == cutx]%>% sf::st_as_sf()

      subpts_comp <- st_as_sf(subpts_comp)
      # subpts_y_buf <- subpts_y %>% sf::st_as_sf()
      subpts_y_buf <- subpts_y %>%
        st_buffer(dist)


      # compute the intersection and save results in a element of "results".
      # For each point, save its "or_id" and the "or_ids" of the points within "dist"

      inters <- sf::st_intersects(subpts_y_buf, subpts_comp)

      # save results
      results[[count]] <- data.table(
        id = subpts_y_buf$or_id,
        int_ids = lapply(inters, FUN = function(x) subpts_comp$or_id[x])
      )
    }
  }
  out <- data.table::rbindlist(results)
  # data.table::rbindlist(out)
  out
}
