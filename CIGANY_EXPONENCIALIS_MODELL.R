# 1. ábra: A cigányfelmérések eredményeire számolt exponenciális függvény.
# Szerzõ: Gulácsi András, 2015

library(extrafont)

setwd("C:/Users/Guland/Desktop")

x <- c(1893,1971,1993,2003)
y <- c(60000,320000,455000,585000)

expon <- lm(log(y)~x)
summary(expon)

jpeg(
	"Cigany_modell.jpg",
	width = 2500,
	height = 2500,
	units = 'px',
	res = 600,
	quality = 100,
	antialias = "default"
)
par(
	mar = c(4,3.8, 0.6, .8),
	oma = c(0,0,0,0),
	family = "Times New Roman",
	cex.axis = 1.0,
	cex.main = 1.4,
	cex.lab = 1.0,
	cex.sub = 1.0
)

plot(
	y ~ x,
	xlim = c(1890,2020),
	ylim = c(0,800000),
	pch = 21,
	bg = "green",
	xlab = "Év",
	ylab = "Létszám (ezer fõ)",
	xaxs = "i",
	yaxs = "i",
	las = 3,
	axes = F
)
axis(
	2,
	at = seq(0,800000, by = 200000),
	labels = c(0,200,400,600,800)
)
axis(
	1,
	pos = 0
)
box()

elemek <- c(
	as.expression(substitute(y == "7,2661" %.% 10^"-13" %.%  e^"0,02058x")),
	NA,
	as.expression(substitute(r^2~"="~"0,9983, p<0,001"))
)

legend(
	"topleft",
	cex = .8
	bty = "n",
	legend = elemek,
	col = "black",
)

points(
	x = 1893,
	y = 60000,
	pch = 21,
	bg = "red"
)
x <- c(1893:2015)
y <- 7.26607e-13*exp(0.02058*x)
lines(
	x,
	y,
	type = "l",
	lwd = 2,
	col = "blue"
)


legend("bottomright",
	cex = .8,
	bty = "n", 
	legend = c("A szociológiai adatfelvételek becslése", "Az 1893. évi cigányösszeírás"), 
	text.col = c("black", "black"),
	col = c("black", "black"),
	pch = c(21, 21),
	pt.bg=c("green", "red"),
)
dev.off()