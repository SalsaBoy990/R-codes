# A 250 méteres felbontású MODIS EVI (továbbfejlesztett vegetációindex)
# 16 napos kompozitkép adatokból 2000-2016 között mindegyik évre egy
# tenyészidõszaki (március - szeptember) átlagképet számoltam (a SAGA GIS-szel).
# Ez a kis szkript ezekbõl készít egy dobozdiagramot. Az eredmény: 600 dpi-s JPG fájl.

library(extrafont)
library(rgdal)
library(fields)

#SET CURRENT WORKING DIR

setwd("g:/modis/all/lomb/")


#READ RASTER DATA

lomb0 <- readGDAL('evi_standard_2000.tif')
lomb0 <- data.frame(lomb0)

lomb1 <- readGDAL('evi_standard_2001.tif')
lomb1 <- data.frame(lomb1)

lomb2 <- readGDAL('evi_standard_2002.tif')
lomb2 <- data.frame(lomb2)

lomb3 <- readGDAL('evi_standard_2003.tif')
lomb3 <- data.frame(lomb3)

lomb4 <- readGDAL('evi_standard_2004.tif')
lomb4 <- data.frame(lomb4)

lomb5 <- readGDAL('evi_standard_2005.tif')
lomb5 <- data.frame(lomb5)

lomb6 <- readGDAL('evi_standard_2006.tif')
lomb6 <- data.frame(lomb6)

lomb7 <- readGDAL('evi_standard_2007.tif')
lomb7 <- data.frame(lomb7)

lomb8 <- readGDAL('evi_standard_2008.tif')
lomb8 <- data.frame(lomb8)

lomb9 <- readGDAL('evi_standard_2009.tif')
lomb9 <- data.frame(lomb9)

lomb10 <- readGDAL('evi_standard_2010.tif')
lomb10 <- data.frame(lomb10)

lomb11 <- readGDAL('evi_standard_2011.tif')
lomb11 <- data.frame(lomb11)

lomb12 <- readGDAL('evi_standard_2012.tif')
lomb12 <- data.frame(lomb12)

lomb13 <- readGDAL('evi_standard_2013.tif')
lomb13 <- data.frame(lomb13)

lomb14 <- readGDAL('evi_standard_2014.tif')
lomb14 <- data.frame(lomb14)

lomb15 <- readGDAL('evi_standard_2015.tif')
lomb15 <- data.frame(lomb15)

lomb16 <- readGDAL('evi_standard_2016.tif')
lomb16 <- data.frame(lomb16)


# create a list for the data frames (easier to create boxplot from a list)
my.list <- list(
	lomb0$band1,
	lomb1$band1,
	lomb2$band1,
	lomb3$band1,
	lomb4$band1,
	lomb5$band1,
	lomb6$band1,
	lomb7$band1,
	lomb8$band1,
	lomb9$band1,
	lomb10$band1,
	lomb11$band1,
	lomb12$band1,
	lomb13$band1,
	lomb14$band1,
	lomb15$band1,
	lomb16$band1
)

# Plot data
bp <- boxplot(my.list)


# Calculate mean for each year
N <- 17
vektor <- vector(N, mode="numeric")

for (i in 1:N)
{
	vektor[i] <- mean(my.list[[i]])
}

# Create x axis labels
lab <- c(2000:2016)

sor <- c(1:17)

# Export boxplot to JPG
jpeg(
	"EVI_tulevelu_dobozdiagram.jpg",
	width=5000,
	height=5000,
	units='px',
	res=600,
	quality=100,
	antialias="default"
)

par(
	mar=c(4, 4.7, 0.1, 0.1),
	oma=c(0,0,0,0),
	family="Times New Roman",
	cex.axis=1.9,
	cex.main=1.9,
	cex.lab=1.7,
	cex.sub=1.9
)

bp <- boxplot(
	my.list,
	at = sor,
	outline=F,
	ylab=expression(bold("EVI standardizált eltérés")),
	axes=F,
	col="lightblue",
	las=2,
	names=lab
)
axis(1, at=sor, labels=FALSE)

text(
	sor,
	par("usr")[3]-0.05,
	labels = lab,
	srt = 45,
	cex=1.7,
	adj=c(1.1,1.1),
	xpd = TRUE
)

axis(
	2,
	at=seq(-1, 1, by=.2),
	labels=chartr('.', ',', as.character(seq(-1, 1, by=.2)))
)

abline(h=0, col="red", lwd=2.0)
box()


text(
	3.5, 1,
	expression(paste(italic("Tûlevelû erdõk"))),
	cex=1.7
)

points(
	sor, vektor,
	pch=20,
	cex=1.3,
	lwd=3,
	col="red"
)

legend(
	"bottomright",
	legend="átlag",
	bty="n",
	pch=20,
	col="red",
	text.col = "black",
	cex=1.7
)

dev.off()
