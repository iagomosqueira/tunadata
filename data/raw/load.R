# load.R - DESC
# tunadata/data/load.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

library(data.table)

# -- IOTC {{{

nmsnc <- c("flcde", "fleet", "arcde", "area", "year", "ficde", "fishery",
  "grgrp", "grcde", "gear", "spp", "species", "spname", "spgrp", "catch") 

# NC
ionc <- fread("iotc/IOTC-2015-DATASETS-NCDB_230216-Catches.csv",
  sep=",", header=TRUE)

# SELECT cols
cols <- c("FlCde", "Fleet", "ArCde", "AreaIOTC", "Year/An", "TFCde", "TypeFishery",
  "GrGroup", "GrCde", "Gear", "SpCde", "Species", "SpLat", "SpGroup", "Catch/Capture(t)")
ionc <- ionc[, cols, with=FALSE]

# SET new names
names(ionc) <- nmsnc 

# cpc
ionc[, cpcde := ifelse(grepl("EU.*", ionc$fleet), "EU", ionc$flcde)]
ionc[, cpc := ifelse(grepl("EU.*", ionc$fleet), "European Union", ionc$fleet)]

setkey(ionc, "cpcde", "flcde", "arcde", "ficde", "grgrp", "grcde", "year", "spp", "spgrp")

# SAVE iotc
save(ionc, file="../iotc.RData", compress="xz") 


# CE LL
ce <- fread("iotc/IOTC-2016-WPNT06-DATA04-CELongline.csv", sep=",", header=TRUE)

# cen
cen <- ce[, c(names(ce)[1:11], grep("-NO", names(ce), value=TRUE)), with=FALSE]
names(cen) <- sub("-NO", "", names(cen))
cen <- melt(cen, id.vars=c(1:11), measure.vars=c(12:34),
  variable.name="spp", value.name="no")

# cet
cet <- ce[, c(names(ce)[1:11], grep("-MT", names(ce), value=TRUE)), with=FALSE]
names(cet) <- sub("-MT", "", names(cet))
cet <- melt(cet, id.vars=c(1:11), measure.vars=c(12:34),
  variable.name="spp", value.name="catch")

# BIND cell
cell <- cbind(cen, cet[,list(catch)])

# SUBSET cols and SET names
cell <- cell[,c("Fleet", "Gear", "Year", "MonthStart", "MonthEnd", "Grid",
  "Effort", "EffortUnits", "spp", "no", "catch"), with=FALSE]
names(cell) <- c("flcde", "grcde", "year", "msta", "mend", "grid",
  "effort", "effunits", "spp", "no", "catch")

# CLEAN flcde
cell[, flcde := gsub(" ", "", flcde)]

# ADD cpcde
cell[, cpcde := ifelse(grepl("EU*", flcde), "EU", flcde)]

# SAVE iotc
# save(ionc, cell, file="../iotc.RData", compress="xz") 


# -- EXAMPLE 1 - Efforty by year for LL (HOOKS)
dat <- cell[effunits == "HOOKS", .(effort=sum(effort)), by=list(flcde, year)]
ggplot(dat, aes(x=year, y=effort)) + geom_line() +
  facet_wrap(~ flcde, scales="free")

# -- EXAMPLE 2 - Effort of JPN by year
dat <- cell[flcde == "JPN", .(effort=sum(effort)/1000), by=list(year)]
ggplot(dat, aes(x=year, y=effort)) + geom_line() +
  geom_point(data=subset(dat, year == 2014), size=2) +
  geom_point(data=subset(dat, year == min(year)), size=2, shape=1) +
  ylab("Effort (000s hooks)") + xlab("")


# CE SURF
ce <- fread("iotc/IOTC-2016-WPNT06-DATA05-CESurface.csv", sep=",", header=TRUE)

# cen
cen <- ce[, c(names(ce)[1:11], grep("-NO", names(ce), value=TRUE)), with=FALSE]
names(cen) <- sub("-NO", "", names(cen))
cen <- melt(cen, id.vars=c(1:11), measure.vars=c(12:34),
  variable.name="spp", value.name="no")

# cet
cet <- ce[, c(names(ce)[1:11], grep("-MT", names(ce), value=TRUE)), with=FALSE]
names(cet) <- sub("-MT", "", names(cet))
cet <- melt(cet, id.vars=c(1:11), measure.vars=c(12:34),
  variable.name="spp", value.name="catch")

# ce
cell <- cbind(cen, cet[,list(catch)])


# CE COAST
ce <- fread("iotc/IOTC-2016-WPNT06-DATA06-CECoastal.csv", sep=",", header=TRUE)


# FC

nmsfc <- c("flcde", "fleet", "year", "ficde", "fishery",
  "grgrp", "grcde", "gear", "loa", "boats") 

iofc <- fread("iotc/results_57862ff1616c1.csv", sep=",", header=TRUE)

# SELECT cols
cols <- c("FlCde", "Fleet", "Year_An", "TFCde", "TypeFishery", "GrGroup",
  "GrCde", "Gear", "LOA_LHT", "noBoats_nBateaux")

iofc <- iofc[, cols, with=FALSE]
names(iofc) <- nmsfc

# cpc
iofc[, cpcde := ifelse(grepl("EU.*", iofc$fleet), "EU", iofc$flcde)]
iofc[, cpc := ifelse(grepl("EU.*", iofc$fleet), "European Union", iofc$fleet)]

setkey(iofc, "cpcde", "flcde", "ficde", "grgrp", "grcde", "year")

# SAVE iotc
save(ionc, iofc, file="../iotc.RData", compress="xz") 

# }}}

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

