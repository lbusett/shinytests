points_in_distance_parallel <- function(in_pts,
                               maxdist,
                               ncuts = 10) {

  require(doParallel)
  require(foreach)
  require(data.table)
  require(sf)
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
             ycut = as.integer(cut(y, ncuts, labels = 1:ncuts)))] %>%
    setkey(xcut, ycut)

  results <- list()

  cl <- parallel::makeCluster(parallel::detectCores() - 2, type =
                                ifelse(.Platform$OS.type != "windows", "FORK",
                                       "PSOCK"))
  doParallel::registerDoParallel(cl)
  # start cycling over quadrants
  out <- foreach(cutx = 1:length(unique(pts$xcut)), .packages = c("sf", "data.table")) %dopar% {

    count <- 0

    # get the points included in a x-slice, extended by `maxdist`, and build
    # an index over y
    min_x_comp    <- ifelse(cutx == 1, limits_x[cutx], (limits_x[cutx] - maxdist))
    max_x_comp    <- ifelse(cutx == ncuts, limits_x[cutx + 1], (limits_x[cutx + 1] + maxdist))
    subpts_x <- pts[x >= min_x_comp & x < max_x_comp] %>%
      setkey(y)

    for (cuty in unique(pts$ycut)) {

      count <- count + 1

      min_y_comp  <- ifelse(cuty == 1, limits_y[cuty], (limits_x[cuty] - maxdist))
      max_y_comp  <- ifelse(cuty == ncuts, limits_x[cuty + 1], (limits_x[cuty + 1] + maxdist))
      subpts_comp <- subpts_x[y >= min_y_comp & y < max_y_comp]

      subpts_buf <- subpts_comp[ycut == cuty & xcut == cutx] %>%
        sf::st_as_sf() %>%
        st_buffer(maxdist)

      subpts_comp <- st_as_sf(subpts_comp)

      inters <- sf::st_intersects(subpts_buf, subpts_comp)

      results[[count]] <- data.table(
        id = subpts_buf$or_id,
        int_ids = lapply(inters, FUN = function(x) subpts_comp$or_id[x])

      )

    }

    return(data.table::rbindlist(results))

  }

  parallel::stopCluster(cl)
  data.table::rbindlist(out)
}
