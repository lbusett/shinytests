library(sf)
library(spdep)
library(sf)
library(data.table)

# create some points
npoints_list <- c(1000, 3000, 5000, 10000, 30000, 50000,100000, 200000)
dist    <- 200
ncuts   <- 10
times <- NULL

points_in_distance <- function(in_pts, dist, ncuts) {

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
    setkey(xcut)
browser()
  results <- list()
  count <- 0
  # start cycling over quadrants
  for (cutx in unique(pts$xcut)) {
    # get the points included in a x-slice, then create an index over ycut
    subpts_x <- pts[xcut == cutx] %>%
      setkey(ycut)
    # get the points included in a x-slice, extended by `dist`, and build
    # an index over y
    min_x_comp    <- ifelse(cutx == 1, limits_x[cutx], (limits_x[cutx] - dist))
    max_x_comp    <- ifelse(cutx == ncuts, limits_x[cutx + 1], (limits_x[cutx + 1] + dist))
    subpts_x_comp <- pts[x >= min_x_comp & x < max_x_comp] %>%
      setkey(y)
    for (cuty in unique(pts$ycut)) {
      count <- count + 1
      # subset over `y` coordinates and buffer. subpts_y_buf contains the points
      # which "neighbours" we want to find.
      subpts_y_buf <- subpts_x[ycut == cuty] %>%
        sf::st_as_sf() %>%
        st_buffer(dist)

      # Now find the points with which we need to "compare" the points in
      # subpts_y_buf
      min_y_comp  <- ifelse(cuty == 1, limits_y[cuty], (limits_x[cuty] - dist))
      max_y_comp  <- ifelse(cuty == ncuts, limits_x[cuty + 1], (limits_x[cuty + 1] + dist))
      subpts_comp <- subpts_x_comp[y >= min_y_comp & y < max_y_comp] %>%
        sf::st_as_sf()

      # compute the intersection and save results in a element of "results".
      # For each point, save its "or_id" and the "or_ids" of the points within "dist"

      inters <- sf::st_intersects(subpts_y_buf, subpts_comp)
      results[[count]] <- data.table(
        id = subpts_y_buf$or_id,
        int_ids = lapply(inters, FUN = function(x) subpts_comp$or_id[x])
      )
    }
  }

  # when all quadrants processed, convert to a data frame
  results_df <- data.table::rbindlist(results) %>%
    setkey(id)
  results_df()

}



for (npoints in  npoints_list) {
  pts     <- data.frame(x = runif(npoints, 0, 50000),
                        y = runif(npoints, 0, 50000),
                        id = 1:npoints) %>%
    sf::st_as_sf(coords = c("x", "y"), remove = F)  %>%
    data.table()
  t0 <- Sys.time()
}

results_df <- dplyr::left_join(pts, results_df)
print(time <- as.numeric(difftime(Sys.time() , t0, units = "secs")))

times <- rbind(times, time)

ggplot(data = data.frame(points = npoints_list, time = times),
       aes(x = points, y = time)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_light()


sel_id <- sample(pts$id,1)
pt_sel <- pts %>%
  dplyr::filter(id == sel_id) %>%
  sf::st_as_sf()

pt_buff <- pt_sel %>%
  sf::st_buffer(500)

id_inters <- results_df %>%
  dplyr::filter(id == sel_id) %>%
  .$int_ids %>% .[[1]]

pt_inters <- pts %>% dplyr::filter(id %in% id_inters) %>%
  sf::st_as_sf()
plot(pt_buff["x"])
plot(sf::st_as_sf(pts)["x"], add = T, col = "green")
plot(pt_inters["x"], add = T, col = "red")

subpts_sf      <- pts[x >= limits[cut]  & x < limits[cut + 1]] %>% st_as_sf() %>%
  sf::st_buffer(dist)
subpts_comp <- pts[x >= (limits[cut] - dist) & x < (limits[cut + 1] + dist)] %>% st_as_sf()

st_intersection(subpts_sf[1,], subpts_comp)

pts_buf <- sf::st_buffer(pts, 5000)%>%
  data.table()
coords <- sf::st_coordinates(pts)
pts <- data.table(pts)






aa <- subpts_x[ycut == cuty] %>%
  sf::st_as_sf() %>% st_bbox() %>% st_as_sfc()

bb <- st_bbox(subpts_comp) %>% st_as_sfc()

plot(st_as_sfc(st_bbox(pts_sf)), main = "xcut = 6, ycut = 7")
plot(st_geometry(sf::st_as_sf(pts)), add = T, cex = 0.3)
plot(st_geometry(subpts_comp), add = T, cex = 0.5, col = "red")
plot(st_geometry(subpts_x[ycut == cuty] %>%
                   sf::st_as_sf()), add = T, cex = 0.5, col = "green")

plot(st_cast(aa, "LINESTRING"), col = "green", add = T)
plot(st_cast(bb, "LINESTRING"), col = "red", add = T)


microbenchmark::microbenchmark(sf_int = {
  int <- sf::st_intersects(pts_buf, pts)},
  spdep = {x <- spdep::dnearneigh(coords, 0, 5000)}, times = 1)

subs <- c(1000, 3000, 5000, 10000, 15000, 30000, 50000)
times <- NULL
for (sub in subs[1:7]) {
  pts_sub <- pts[1:sub,]
  buf_sub <- pts_buf[1:sub,]
  t0 <- Sys.time()
  int <- sf::st_intersects(buf_sub, pts_sub)
  times <- cbind(times, Sys.time() - t0)
}

plot(subs, times)
times <- as.numeric(times)
reg <- lm(times~subs+I(subs^2))
reg <- lm(times~subs)
summary(reg)
predict(reg, newdata = data.frame(subs = 10E6))/60/60



pts_buf <- sf::st_buffer(pts, 5000) %>%
  dplyr::mutate(x = pts$x, y = pts$y)

pts <- data.frame(x = runif(15000000, 0, 100000),
                  y = runif(15000000, 0, 100000),
                  id = 1:1500000)
pts <- data.table(pts)
setkey(pts, x)
setkey(pts_buf, x,y)

min(pts$x)
sub1 = list()
for(i in 1:50000) {

  sub <- pts[x > 25000 & x < 35000 & y > 10000 & y < 20000]

  xi = pts$x[i]
  yi = pts$y[i]
  sub[, distx := (x - xi)^2]
  # setkey(pts1, distx)
  sub1  <- sub[distx < 25000]
  sub1[, d := ((x - xi)^2 + (y - yi)^2)]
  # setkey(pts1, d)
  aa= sub1[d < 25000, id]
}


%>% st_as_sf()
# subbuf1 <- subset(pts_buf, distx < 5000 & disty < 5000) %>% st_as_sf()
int1 <- sf::st_distance(sub1, sub1, , which = TRUE)



t0 <- Sys.time()
coords <- sf::st_coordinates(pts_sub)
x <- spdep::dnearneigh(coords, 0, 5000)
Sys.time() - t0
