library(shiny)
library(shinydashboard)
library(sp)
library(data.table)
library(DT)
library(leaflet)
library(data.table)
library(RColorBrewer)
load("streets.RData")

ratings_sort=data.table(ratings)[order(rank(streetID),overall,Potholes,dateLastOverlay)]
ratings_sort=ratings_sort[,.SD[1],by=streetID]  
ratings_sort=ratings_sort[order(overall)]

