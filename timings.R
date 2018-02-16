npoints_list <- seq(25000, 475000, 50000)
timings_vanilla <- NULL
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
save(timings_vanilla, file = "vanilla.RData")

npoints_list <- seq(25000, 475000, 50000)
timings_dtint <- NULL
for (maxdist in c(500, 2000)) {
  for (npoints in npoints_list) {
    pts     <- data.frame(x = runif(npoints, 0, 100000),
                          y = runif(npoints, 0, 100000),
                          id = 1:npoints) %>%
      st_as_sf(coords = c("x", "y"), remove = FALSE)
    t0 <- Sys.time()
    out <- points_in_distance(pts, maxdist, ncut = 10)
    time = as.numeric(difftime(Sys.time() , t0, units = "secs"))
    timings_dtint <- rbind(timings_dtint, data.frame(npoints = npoints,
                                                     time = time,
                                                     maxdist = maxdist,
                                                     method = "points_in_distance - serial"))
    print(time)
    rm(out)
    gc()
  }
}
save(timings_dtint, file = "dtint.RData")
npoints_list <- seq(25000, 475000, 50000)
timings_dtint_parallel <- NULL
for (maxdist in c(500, 2000)) {
  for (npoints in npoints_list) {
    pts     <- data.frame(x = runif(npoints, 0, 100000),
                          y = runif(npoints, 0, 100000),
                          id = 1:npoints) %>%
      st_as_sf(coords = c("x", "y"), remove = FALSE)
    t0 <- Sys.time()
    out <- points_in_distance_parallel(pts, maxdist, ncut = 15)
    time = as.numeric(difftime(Sys.time() , t0, units = "secs"))
    timings_dtint_parallel <- rbind(timings_dtint_parallel,
                                    data.frame(npoints = npoints, time = time,
                                               maxdist = maxdist,
                                               method = "points_in_distance - parallel"))
    rm(out)
    print(time)
    gc()
  }
}
save(timings_dtint_parallel, file = "dtint_parallel.RData")
timings = rbind(timings_vanilla, timings_dtint, timings_dtint_parallel)
ggplot(timings, aes(x = npoints, y = time, color = method, group = method)) +
  geom_point() + theme_light() +
  geom_smooth(formula = y ~ poly(x,2), method = "lm", se = F) +
  facet_wrap(~maxdist)


