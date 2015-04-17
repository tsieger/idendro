## Visualization of spectroscopy data in dendrogram and heat map with
## cranvas scatter plot and parallel coordinate plot integrated.
##

library(hyperSpec) # chondro data

# preprocess chondro data
cat('preprocessing the chondro data (it takes a while)\n')
chondro <- spc.loess(chondro, newx = seq (602, 1800, by = 4))
chondro <- chondro - spc.fit.poly.below(chondro)
chondro <- sweep(chondro, 1, rowMeans(chondro), "/")
overall.composition <- quantile(chondro, 0.05)
chondro <- sweep(chondro, 2, overall.composition, "-")

# HCA
dst <- dist(chondro)
dndr <- hclust(dst, method = "ward")

# mutable data frame construction
mdf.chondro <- as.wide.df(chondro)
colnames(mdf.chondro)[-(1:3)] <- paste("wl", colnames(mdf.chondro)[-(1:3)], sep = ".")
names <- as.character(wl(chondro))
names[wl(chondro) %% 50 != 0] <- ""
mdf.chondro <- qdata(mdf.chondro)

# dendrogram
idendro(dndr, mdf.chondro, heatmapRelSize = 0.75, heatmapColors = alois.palette(25))

# scatter plot
print(qscatter(x, y, data = mdf.chondro, unibrushcolor = FALSE))

# parallel coordinate plot
print(qparallel(vars = var_names(~., mdf.chondro)[-(1:3)], data = mdf.chondro,
  names = names, scale = "I", glyph = "line"))
