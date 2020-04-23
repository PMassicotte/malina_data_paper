interpolate_2d <- function(df, x, y, z, nx = 500, ny = 500, extend = FALSE, n = 1, m = 1, h = 8) {

  res <- df %>%
    dplyr::select({{ x }}, {{ y }}, {{ z }}) %>%
    drop_na() %>%
    mba.surf(nx, nx, extend = extend, n = n, m = m, h = h)

  res2 <- expand.grid(
    x = res$xyz.est$x,
    y = res$xyz.est$y
  ) %>%
    mutate(z = as.vector(res$xyz.est$z)) %>%
    as_tibble()

  return(res2)
}

