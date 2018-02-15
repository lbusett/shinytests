#' ---
#' output:
#'   md_document:
#'     pandoc_args: [
#'       --wrap=preserve
#'     ]
#' ---

#'<!-- language-all: lang-r -->

#+ reprex-setup, include = FALSE
options(tidyverse.quiet = TRUE)
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", error = TRUE)
knitr::opts_knit$set(upload.fun = knitr::imgur_upload)



#+ reprex-body

#' When working with `sf` objects, explicitly looping over features to perform
#' binary operations such as intersects is usually counterproductive (see also
#' https://stackoverflow.com/questions/48551470/how-can-i-speed-up-spatial-operations-in-dplyrmutate)
#'
#' An approach similar to yours (i.e., buffering and intersecting), but without
#' the explicit for loop works better (and is faster than the `dnearneigh` one).
#'
#' Let's see how it performs on a reasonably big dataset of 50000 points:
#'

library(sf)
library(spdep)
library(sf)

pts <- data.frame(x = runif(50000, 0, 100000),
                  y = runif(50000, 0, 100000))
pts     <- sf::st_as_sf(pts, coords = c("x", "y"), remove = F)
pts_buf <- sf::st_buffer(pts, 5000)
coords  <- sf::st_coordinates(pts)

microbenchmark::microbenchmark(
  sf_int = {int <- sf::st_intersects(pts_buf, pts)},
  spdep  = {x   <- spdep::dnearneigh(coords, 0, 5000)}
  , times = 1)

#' You can see here that the `st_intersects` approach is 5 time faster than
#' the `dnearneigh` one.
#'
#' Unfortunately, this is unlikely to solve your problem. Looking at execution
#' times for datasets of different sizes we get:

subs <- c(1000, 3000, 5000, 10000, 15000, 30000, 50000)
times <- NULL
for (sub in subs[1:7]) {
  pts_sub <- pts[1:sub,]
  buf_sub <- pts_buf[1:sub,]
  t0 <- Sys.time()
  int <- sf::st_intersects(buf_sub, pts_sub)
  times <- cbind(times, as.numeric(difftime(Sys.time() , t0, units = "secs")))
}

plot(subs, times)
times <- as.numeric(times)
reg <- lm(times~subs+I(subs^2))
summary(reg)

#' Here, we see an almost perfect quadratic relationship between time and
#' number of points (as would be expected). On a 10M points subset, assuming
#' that the behaviour does not change, you would get:

predict(reg, newdata = data.frame(subs = 10E6))

#' , which corresponds to about 10 days, assuming that the trend is constant
#' when further increasing the number of points (but the same would happen for
#' `dnearneigh`...)
#'
#' My suggestion would be to "split" your points in chunks and then work on a
#' per-split basis. You could for example order your points at the beginning along
#' the x-axis and then easily and quickly extract subsets of buffers and of points
#' with which to compare them using data.table.
#' The "points" buffer would need to be larger than that of points according
#' to the comparison distance. So, for example, if you make a subset of `pts_buf` with
#' centroids in [50000 - 55000], the corresponding subset of `pts` should include
#' points in the range [49500 - 55500].
#' This approach is easily parallelizable by assigning the different subsets to
#' different cores in a `foreach` or similar construct.
#' I do not even know if using spatial objects/operations is beneficial here, since once
#' we have the coordinates all is needed is computing and subsetting euclidean
#' distances: I suspect that a carefully coded brute force `data.table`-based
#' approach could be also a feasible solution.
#'
#' HTH!


#' Created on `r Sys.Date()` by the [reprex package](http://reprex.tidyverse.org) (v`r utils::packageVersion("reprex")`).

