# iotc.R - DESC
# /iotc.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

library(ggplot2)

# LOAD the IOTC dataset
data(iotc)

# INSPECT ionc (IOTC NC) data.table
ionc

summary(ionc)

# CATCH per gear group in 2014
ionc[year == 2014, .(catch=sum(catch)), by = fishgr]

# CATCH per cpc in 2014
ionc[year == 2014, .(catch=sum(catch)), by = cpccde]

# CATCH per yeat for ALB
ionc[sppcde == "ALB", .(catch=sum(catch)), keyby = year]

ggplot(ionc[sppcde == "ALB", .(catch=sum(catch)), keyby = year],
  aes(x=year, y=catch)) + geom_line() + geom_point() +
  xlab("") + ylab("Catch (t)")
