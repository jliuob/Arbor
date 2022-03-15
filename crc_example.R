source('prepare_data.R')
source('draw-functions.R')

# ps<-load("ps.Rdata")
bacteria <- read.csv("~/Documents/Tree/crc/eocrc.vs.ctrl.male.csv")
crc<-paste("crc")

prepare_data(ps, bacteria, crc)
