points_in_distance <- function(in_pts,
                               maxdist,
                               ncuts = 10) {

  require(data.table)
  require(sf)
  # convert points to data.table and create a unique identifier
  pts <-  data.table(in_pts)
  pts <- pts[, or_id := 1:dim(in_pts)[1]]

  # divide the extent in quadrants in ncuts*ncuts quadrants and assign each
  # point to a quadrant, then create the index over "x" and "y"
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

    # get the points included in a x-slice extended by `dist`, and build
    # an index over y
    min_x_comp    <- ifelse(cutx == 1,
                            limits_x[cutx],
                            (limits_x[cutx] - maxdist))
    max_x_comp    <- ifelse(cutx == ncuts,
                            limits_x[cutx + 1],
                            (limits_x[cutx + 1] + maxdist))
    subpts_x <- pts[x >= min_x_comp & x < max_x_comp] %>%
      setkey(y)

    for (cuty in unique(pts$ycut)) {
      count <- count + 1

      # subset over subpts_x to find the final set of points needed for the
      # comparisons
      min_y_comp  <- ifelse(cuty == 1,
                            limits_y[cuty],
                            (limits_x[cuty] - maxdist))
      max_y_comp  <- ifelse(cuty == ncuts,
                            limits_x[cuty + 1],
                            (limits_x[cuty + 1] + maxdist))
      subpts_comp <- subpts_x[y >= min_y_comp & y < max_y_comp]

      # subset over subpts_comp to get the points included in a x/y chunk,
      # which "neighbours" we want to find. Then buffer them.
      subpts_buf <- subpts_comp[ycut == cuty & xcut == cutx] %>%
        sf::st_as_sf() %>%
        st_buffer(maxdist)

      # retransform to sf since data.tables lost the geometric attrributes
      subpts_comp <- sf::st_as_sf(subpts_comp)

      # compute the intersection and save results in a element of "results".
      # For each point, save its "or_id" and the "or_ids" of the points within "dist"
      inters <- sf::st_intersects(subpts_buf, subpts_comp)

      # save results
      results[[count]] <- data.table(
        id = subpts_buf$or_id,
        int_ids = lapply(inters, FUN = function(x) subpts_comp$or_id[x]))
    }
  }
  data.table::rbindlist(results)
}
