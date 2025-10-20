library(dartRverse)
library(ggplot2)
library(tidyverse)
library(related)
library(reshape2)
library(learnr)
library(shiny)
library(gbRd)
library(tools)
library(semantic.dashboard)
devtools::install_github("https://github.com/green-striped-gecko/dartR.captive/tree/dev_ethan")
library("dartR.captive")

pig <- readRDS("/Users/ethanhalford/Desktop/DArT_Coding/gl.relatedness_Shiny/pig.rds") %>%
  {position(.) <- 1:nLoc(.); .} %>%
  {chromosome(.) <- rep("1", nLoc(.));.}
sheep <- readRDS("/Users/ethanhalford/Desktop/DArT_Coding/gl.relatedness_Shiny/sheep.rds") %>%
  {position(.) <- 1:nLoc(.); .} %>%
  {chromosome(.) <- rep("1", nLoc(.));.}

pigTest <- pig[1:1000] %>%
  {. <- gl.subsample.loci(., 3000); .}
sheepTest <- sheep[1:1000] %>%
  {. <- gl.subsample.loci(., 3000); .}


setup_item <- gl.diagnostics.relatedness(pigTest, 
                                         cleanup = T, 
                                         run_sim =, 
                                         which_tests = c("wang", "lynchli"),
                                         IncludePlots = T,
                                         rmseOut =T,
                                         varOut = F,
                                         numberIterations = 1, 
                                         numberGenerations = 3,
                                         includedPed = T
)
