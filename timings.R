npoints_list <- seq(25000, 475000, 50000)
timings_vanilla <- NULL
for (dist in c(500, 2000)) {
  for (npoints in npoints_list) {
    pts     <- data.frame(x = runif(npoints, 0, 100000),
                          y = runif(npoints, 0, 100000),
                          id = 1:npoints) %>%
      st_as_sf(coords = c("x", "y"))
    t0 <- Sys.time()
    pts_buf <- sf::st_buffer(pts, dist)
    int <- sf::st_intersects(pts_buf, pts)
    time = as.numeric(difftime(Sys.time() , t0, units = "secs"))
    timings_vanilla <- rbind(timings_vanilla,
                             data.frame(npoints = npoints, time = time, dist = dist,
                                        method = "st_intersect"))
    print(time)
    gc()
  }
}

npoints_list <- seq(25000, 200000, 50000)
timings_dtint <- NULL
# for (dist in c(500, 2000)) {
  for (npoints in npoints_list) {
    pts     <- data.frame(x = runif(npoints, 0, 100000),
                          y = runif(npoints, 0, 100000),
                          id = 1:npoints) %>%
      st_as_sf(coords = c("x", "y"), remove = FALSE)
    t0 <- Sys.time()
    out <- points_in_distance(pts, dist = dist, ncut = 10)
    time = as.numeric(difftime(Sys.time() , t0, units = "secs"))
    timings_dtint <- rbind(timings_dtint, data.frame(npoints = npoints, time = time, dist = dist,
                                                     method = "points_in_distance"))
    print(time)
    gc()
  # }
}

npoints_list <- seq(25000, 200000, 50000)
timings_dtint_parallel <- NULL
# for (dist in c(500, 2000)) {
  for (npoints in npoints_list) {
    pts     <- data.frame(x = runif(npoints, 0, 100000),
                          y = runif(npoints, 0, 100000),
                          id = 1:npoints) %>%
      st_as_sf(coords = c("x", "y"), remove = FALSE)
    t0 <- Sys.time()
    out <- points_in_distance_parallel(pts, dist = dist, ncut = 10)
    time = as.numeric(difftime(Sys.time() , t0, units = "secs"))
    timings_dtint_parallel <- rbind(timings_dtint_parallel, data.frame(npoints = npoints, time = time, dist = dist,
                                                     method = "points_in_distance_parallel"))
    print(time)
    gc()
  # }
}

timings = rbind (timings_dtint, timings_dtint_parallel)
ggplot(timings, aes(x = npoints, y = time, color = method, group = method)) +
  geom_point() + theme_light() +
  geom_smooth(formula = y ~ poly(x,2), method = "lm") +
  facet_wrap(~dist)


