## ---------------------------
##
## Script name: boot
##
## Purpose of script: To contain all definitions, packages and functions
## required by a project. Only place definitions that you want to be
## executed in every script throughout the project here. This is the "boot"
## file for all the scripts on the project
##
## Author: Daniel Estévez-Barcia // Greenland Institute of Natural Resources, Greenland
## Email: daeb@natur.gl
##
## Date Created: 2022-06-29
##
## ---------------------------

###################
#### Libraries ----

# Package names
packages <- c("tidyverse", "here", "reshape2", "readxl", "knitr", "kableExtra", "adegenet", "poppr", 
              "pegas", "ape", "hierfstat", "vcfR", "pcadapt", "diveRsity", "ggOceanMaps", "ggnewscale",
              "lubridate")
# Notice that hierfstat is installed from Github, the CRAN version has not been updated in a while

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

##########################
## Function shortcuts ----

h <- head

########################
## Custom functions ----

## Read clipboard for Mac

read.clipboard <- function() read.table(pipe("pbpaste"), sep = "\t", header = T)

## Standard error of mean

#' @title Standard error of mean
#' @param x numeric vector

se <- function (x){
  sd(x, na.rm = T)/sqrt(sum(!is.na(x)))}

## Column numbers of a data frame

#' @title Column numbers of a data frame
#' @param x data.frame
#' @return retuns a named vector with column names as names and order of columns as elements
#' @author Mikko Vihtakari

coln <- function (x)
{
  y <- rbind(seq(1, ncol(x)))
  colnames(y) <- colnames(x)
  rownames(y) <- "col.number"
  return(y)
}

## Check colors

#' @title Plot color vector to inspect the colors visually
#' @param cols a character vector containing accepted R \link[grDevices]{colors}
#' @return returns a base R plot with the colors given in \code{cols} argument
#' @author Mikko Vihtakari

check_cols <- function(cols) {
  
  if (is.null(names(cols))) {
    labs <- seq_along(cols)
  } else {
    labs <- paste0(names(cols), "\n[", seq_along(cols), "]")
  }
  
  mp <- barplot(rep(1, length(cols)), yaxt = "n", col = cols, border = NA, names.arg = labs, xlab = "Color sequence",  ylim = c(0,1.2))
  points(mp, rep(1.1, length(cols)), col = cols, pch = 16, cex = 4)
}

## Select element from a list

#' @title Select an element of each vector from a list
#' @description Selects y'th element of each vector from a list
#' @param x list
#' @param y number of element. Must be integer

select.element <- function(x,y) sapply(x, "[", y)

## round_any

#' @title Round to multiple of any number
#' @param x numeric vector to round
#' @param accuracy number to round to; for POSIXct objects, a number of seconds
#' @param f rounding function: \code{\link{floor}}, \code{\link{ceiling}} or
#'  \code{\link{round}}
#' @author Hadley Wickham

round_any <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}

## Font and line size conversions for ggplot2

#' @title Convert font sizes measured as points to ggplot font sizes
#' @description Converts font sizes measured as points (as given by most programs such as MS Word etc.) to ggplot font sizes
#' @param x numeric vector giving the font sizes in points
#' @return Returns a numeric vector of lenght \code{x} of ggplot font sizes
#' @author Mikko Vihtakari

FS <- function(x) x/2.845276

#' @title Convert line sizes measured as points to ggplot line sizes
#' @description Converts line sizes measured as points (as given by most programs such as Adobe Illustrator etc.) to ggplot font sizes
#' @param x numeric vector giving the lines sizes in points
#' @return Returns a numeric vector of lenght \code{x} of ggplot line sizes
#' @author Mikko Vihtakari

LS <- function(x) x/2.13

###################
## Definitions ----

## Sizes and definitions for figures in Frontiers in Marine Science

colwidth <- 85 # mm
pagewidth <- 180 # mm
unit <- "mm"

colwidth_in <- colwidth * 0.0393701
pagewidth_in <- pagewidth * 0.0393701

## ggplot theme

ggtheme <- theme_classic(base_size = 8) %+replace%
  theme(strip.background = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_blank())

####################
## Color themes ----

## Functions to lighten and darken colors, source: https://url12.mailanyone.net/v1/?m=1noPSv-0004Ws-5U&i=57e1b682&c=NZN4J8PnmZL2J3o81D632r7eyA-msXahkvjd1Vzr1Mwn6DtRwn-xb9ZxPSRkvsWIza788SmqrFwsAv07hPNjTW-_yo_m0apA8T2VLhaTu3zmLn2FnQelwF1qlGKUCvWGd9NO83JD4RnskEXTOjY6NF39TtkCHuKz4YNOxx6DmfPcOWYv0GTIbaWmCdH84GvK5GTd8VQo-Rg51YyycARt2sHto4vYATHsR6pjaFdDMznvDSnQepQ1nCkN-0arSgFk

darken <- function(color, factor = 1.2){
  col <- col2rgb(color)
  col <- col/factor
  col <- rgb(t(col), maxColorValue = 255)
  col
}


lighten <- function(color, factor = 1.2){
  col <- col2rgb(color)
  col <- col*factor
  col[col > 255] <- 255
  col <- rgb(t(col), maxColorValue = 255)
  col
}

## Vector of standard colors

cols <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")

## Give cols function

#' @title Get a ramp from a vector of 8 colorblind friendly colors
#' @description From the original vector, the function can obtain a gradient or ramp of any size
#' @param numcat by default 2, number of different colors for categories you wish to obtain
#' @author Daniel Estevez-Barcia

give_cols <- function(numCat = 2) {
  if (any(numCat == 0 | numCat == 1)) {
    stop("You must provide n > 1 colors")
  }
  blindFriend <- cols
  colorRampPalette(colors = blindFriend)(numCat)
}

# check_cols(cols)

size_colors <- cols[1:3]
size_hues <- unlist(lapply(size_colors, function(k) c(lighten(k), k, darken(k))))

## END -----------------------