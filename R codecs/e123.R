par(mfrow = c(2, 2), mar = c(2, 2, 2, 2))
ix <- seq(1, nrow(table), length.out = 20)
iy <- seq(1, ncol(table), length.out = 20)
ribbon3D(z = Volcano[, iy])
ribbon3D(z = Volcano[ix, ], along = "y",
         curtain = TRUE, space = 0.8, shade = 0.2)
ribbon3D(z = Volcano[ix, iy], along = "xy")
hist3D(z = Volcano[ix,iy], shade = 0.5)