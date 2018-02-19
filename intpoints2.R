
 Last week, I replied to this interesting stackoverflow question posted by @Tim_K:
 https://stackoverflow.com/questions/48650274/spatial-efficient-way-of-finding-all-points-within-x-meters-of-a-point

 where, the OP was seeking  efficient solutions to identify all points falling
 within a maximum distance of 5000 meters with respect to each single point in
 the dataset.

 If you have a look at the thread, you will see that a simple solution based
 on creating a "buffered" polygon dataset beforehand and then intersecting it
 with the original points such as:

library(sf)
library(magrittr)
# create test data: 50000 uniformly distributed points on a "square" of 100000
# metres
maxdist <- 500
pts     <- data.frame(x = runif(50000, 0, 100000),
                      y = runif(50000, 0, 100000),
                      id = 1:50000) %>%
  st_as_sf(coords = c("x", "y"))
# create buffered polygons
pts_buf <- sf::st_buffer(pts, maxdist)
# Find points within 5000 meters wrt each point
int <- sf::st_intersects(pts_buf, pts)
int

#' is quite fast for "reasonably sized" datasets, thanks to `sf` spatial indexing
#' capabilities which reduce the number of the required comparisons to be done
#' (See http://r-spatial.org/r/2017/06/22/spatial-index.html).
#'
#' However, for really large datasets this is not going to cut it, because the
#' total number of comparisons to be done will still rapdily increase.
#' A test done by changing the number of points in the above example
#' shows for example this kind of behaviour, for two different values of `maxdist`:
#'
npoints_list <- seq(25000, 475000, 50000)
timings_st_intersect <- NULL
for (maxdist in c(500, 2000)) {
  for (npoints in npoints_list) {
    pts     <- data.frame(x = runif(npoints, 0, 100000),
                          y = runif(npoints, 0, 100000),
                          id = 1:npoints) %>%
      st_as_sf(coords = c("x", "y"))
    t0 <- Sys.time()
    pts_buf <- sf::st_buffer(pts, maxdist)
    out <- sf::st_intersects(pts_buf, pts)
    time = as.numeric(difftime(Sys.time() , t0, units = "secs"))
    timings_vanilla <- rbind(timings_vanilla,
                             data.frame(npoints = npoints, time = time,
                                        maxdist = maxdist,
                                        method = "st_intersect"))
    print(time)
    rm(out)
    gc()
  }
}
save(st_intersect, file = "vanilla.RData")

library(ggplot2)
ggplot(timings_vanilla, aes(x = npoints, y = time)) + geom_point() + theme_light() +
  geom_smooth(formula = y ~ x^2) +
  ggtitle("Time vs. Number of point", subtitle = "\"Vanilla\" st_intersect approach")


reg_500 <- with(subset(timings_vanilla, maxdist == 500), lm(time ~ npoints+I(npoints^2)))
reg_2000 <- with(subset(timings_vanilla, maxdist == 2000), lm(time ~ npoints+I(npoints^2)))
predict(reg_500, newdata = data.frame(npoints = 10E6))/60/60/24
predict(reg_2000, newdata = data.frame(npoints = 10E6))/60/60/24


#' On the test dataset, the relationship is almost perfectly quadratic, due to the
#' uniform distribution of points. Extrapolating it to a 12M points dataset, it
#' would lead to an execution time of about 10 days.
#
#' My suggestion to the OP to reduce the execution time was therefore to "split"
#' the points in chunks based on the x-coordinate and then work on a per-split
#' basis, eventually assigning each chunk to a different core within a parallellized
#' cycle.
#'
#' Afterwards, I got curious and decided to givie it a go to see what kind of
#' performance improvement it was possible to squeeze-out by that kind of approach. You
#' can see the results below.
#'
#' ## Speeding up computation by combining data.table and sf_intersect
#'
#' The idea here is a simple divede and conquer approach.
#'
#' We first split the total spatial extent of the dataset in a certain number of
#' regular quadrants. We then cycle over the quadrants and at each cycle we:
#'
#'    1) Extract the points contained into a quadrant and apply the buffer to them;
#'    2) Extract the points contained in a slightly larger area, computed by expanding
#'       the quadrant by an amount equal to the maximum distance for which we
#'       want to identify the "neighbours";
#'    3) Compute and save the intersection between the bufferd points and the
#'       points contained in the "expanded" quadrant
#'
#' The picture below gives you a graphical representation of the approach: in green
#' you see the boundaries of a "quadrant" and the points included into it, in red
#' the (additional) points to be extracted for the intersection operation.
#'
#' Provided that the subsetting operations do not introduce an excessive overhead
#' this should provide a performance boost, because it will consistently reduce
#' the total number of comparisons to be done. Now, every "R" expert will tell
#' you that if you need to perform fast subsetting over large dataset the way to
#' go is to use **properly indexed** `data.tables`, which provide lightning-speed
#' subsetting capabilities.
#'
#' So, let's see how we could code this in a functions:
#'
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

#'
#' The function takes as input a points `sf` object, a target distance and a number
#' of "cuts" to use to divide the extent in quadrants, and provide in output
#' a data frame in which for each original point the "ids" of the points within
#' `maxdist` are reported in the `int_ids` list column.
#'
#' Let's see if it works:
#'
pts     <- data.frame(x = runif(20000, 0, 100000),
                      y = runif(20000, 0, 100000),
                      id = 1:20000) %>%
  st_as_sf(coords = c("x", "y"), remove = FALSE)
out <- points_in_distance(pts, maxdist = 1000, ncut = 10)
out

# get a random point
sel_id  <- sample(pts$id,1)
pt_sel  <- pts[sel_id, ]
pt_buff <- pt_sel %>%  sf::st_buffer(1000)
# get ids of points within maxdist
id_inters <- unlist(out[id == sel_id, ]$int_ids)
pt_inters <- pts[id_inters,]

#plot results
ggplot(pt_buff)  + theme_light() +
  geom_point(data = pts, aes(x = x, y = y), size = 1) +
  geom_sf(col = "blue", size = 1.2, fill = "transparent") +
  geom_sf(data = pt_inters, col = "red", size = 1.5) +
  geom_point(data = pt_sel, aes(x = x, y = y), size = 2, col = "green") +
  xlim(st_bbox(pt_buff)[1] - maxdist, st_bbox(pt_buff)[3] + maxdist) +
  ylim(st_bbox(pt_buff)[2] - maxdist, st_bbox(pt_buff)[4] + maxdist)

#' So far, so good. Now, let's have a look if this is really faster:
#'

npoints_list <- seq(25000, 20000, 25000)
timings_dtint <- NULL
for (npoints in npoints_list) {
  pts     <- data.frame(x = runif(npoints, 0, 100000),
                        y = runif(npoints, 0, 100000),
                        id = 1:npoints) %>%
    st_as_sf(coords = c("x", "y"), remove = FALSE)
  t0 <- Sys.time()
  out <- points_in_distance(pts, dist = 500, ncut = 10)
  time = as.numeric(difftime(Sys.time() , t0, units = "secs"))
  timings_dtint <- rbind(timings_dtint, data.frame(npoints = npoints, time = time,
                                                   method = "dt_int"))
  print(time)
}

ggplot(timings_dtint, aes(x = npoints, y = time)) + geom_point() + theme_light() +
  geom_smooth(formula = y ~ x^2) +
  ggtitle("Time vs. Number of point", subtitle = "\"Vanilla\" st_intersect approach")
reg <- with(timings_dtint, lm(time ~ npoints+I(npoints^2)))
summary(reg)$r.squared
predict(reg, newdata = data.frame(npoints = 10E6))

#'
#'
#'
