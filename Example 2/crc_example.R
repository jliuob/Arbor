source('prepare_data.R')

ps<-load("ps.Rdata")
bacteria <- read.csv("crc/eocrc.vs.ctrl.male.csv")
crc<-paste("crc")

prepare_data(ps, bacteria, crc)

