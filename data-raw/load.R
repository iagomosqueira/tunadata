# load.R - DESC
# tunadata/data/load.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

library(tunadata)

# -- ICCAT {{{

# NC
icnc <- fread("iccat/t1nc_20151102.csv", sep=",", header=TRUE)

cols <- c("Fleet", "Flag", "AreaT1", "YearC",  "GearCode",
  "Species", "ScieName", "Qty_t")
icnc <- icnc[, cols, with=FALSE]

# SET new names
names(icnc) <- nmsnc[c(1, 2, 4, 5, 8, 10, 12, 13)]

# }}}

# -- IATTC {{{

# NC
ianc <- fread("iattc/CatchFlagGear1918-2014.csv",
  sep=",", header=TRUE)

names(ianc) <- nmsnc[c(5, 1, 8, 10, 13)]
setkey(ianc, year, flcde, grcde, spp, catch)

# }}}

# -- WCPFC {{{

library(foreign)

# NC

# driftnet
wcdn <- data.table(read.dbf("wcpfc/DRIFTNET.DBF"))
wcll60 <- data.table(read.dbf("wcpfc/LONGLINE_60.DBF"))
wcll70 <- data.table(read.dbf("wcpfc/LONGLINE_70.DBF"))
wcll80 <- data.table(read.dbf("wcpfc/LONGLINE_80.DBF"))
wcllpl <- data.table(read.dbf("wcpfc/POLE_AND_LINE.DBF"))
wcllps <- data.table(read.dbf("wcpfc/PURSE_SEINE.DBF"))

# }}}

# -- tRFMOs

# NC
nc <- rbindlist(list(iattc=ianc, iccat=icnc, iotc=ionc), idcol="rfmo",
  use.names=TRUE, fill=TRUE)
