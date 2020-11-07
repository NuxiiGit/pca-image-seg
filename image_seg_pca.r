# load data table
table <- read.table("dataset/image_seg.csv",
                    skip             = 3,     # the height of the header
                    header           = TRUE,  # whether a header exists
                    sep              = ",",   # the line separator
                    stringsAsFactors = FALSE)

# make sure output directory exists
dir.create("fig",
           showWarnings = FALSE)

# separate classes and numerical data
classes <- as.factor(table$CLASS)
dat     <- subset(table,
                  select = -c(CLASS))

# compute covariance matrix
co  <- cov(dat)
co2 <- as.matrix(dat) %*% t(dat)

# create colours
cols <- c("tomato2",
          "lightsteelblue4",
          "forestgreen",
          "darkolivegreen3",
          "gray16",
          "dodgerblue",
          "gold")

# some plots
pdf('fig/intensity-versus-row.pdf')
par(mar = c(4, 4, 0, 0))
plot(INTENSITY.MEAN ~ REGION.CENTROID.ROW,
     main = "",
     data = dat,
     col  = cols[classes],
     pch  = 20)
legend("topright",
       legend = levels(classes),
       col    = cols,
       pch    = 20,
       bty    = "n")
dev.off()

pdf('fig/edge-correlation.pdf')
par(mar = c(4, 4, 0, 0))
plot(HEDGE.MEAN ~ VEDGE.MEAN,
     main = "",
     data = dat,
     col  = cols[classes],
     pch  = 20,
     xlim = c(0, 5),
     ylim = c(0, 5))
legend("topright",
     legend = levels(classes),
     col    = cols,
     pch    = 20,
     bty    = "n")
dev.off()

# perform PCA
datpca <- princomp(dat, cor = TRUE)
ev     <- datpca$sdev^2
n.ev   <- sum(ev)

# plot eigenvalues
pdf('fig/eigenvalues.pdf')
par(mar = c(4, 4, 0, 0))
plot(ev,
     main = "",
     xlab = "Principal Component",
     ylab = "Eigenvalue",
     pch  = 20)
dev.off()

pdf('fig/eigenvalues-cumulative.pdf')
par(mar = c(4, 4, 0, 0))
plot(cumsum(100 * ev / n.ev),
     main = "",
     xlab = "Principal Component",
     ylab = "Cumulative Variance (%)",
     pch  = 20,
     ylim = c(0, 100))
dev.off()

# plot graph
pdf('fig/principal-components.pdf')
par(mar = c(4, 4, 0, 0))
plot(Comp.1 ~ Comp.2,
     main = "",
     data = datpca$scores,
     col  = cols[classes],
     pch  = 20,
     xlim = c(-3, 2))
legend("topleft",
       legend = levels(classes),
       col    = cols,
       pch    = 20,
       bty    = "n")
dev.off()

pdf('fig/principal-components-2.pdf')
par(mar = c(4, 4, 0, 0))
plot(Comp.1 ~ Comp.3,
     main = "",
     data = datpca$scores,
     col  = cols[classes],
     pch  = 20)
legend("topleft",
       legend = levels(classes),
       col    = cols,
       pch    = 20,
       bty    = "n")
dev.off()