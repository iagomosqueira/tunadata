# iotc.R - DESC
# /iotc.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# getNC {{{

getNC <- function(key) {

  url <- "http://statistics.iotc.org/rest/services/data/nc/summary/results"

  jfilter = paste0(
    '{',
      '"includePreliminary": false, ',
      '"includeConfidential": false, ',
      '"iotcSpecies": false, ',
      '"fromYear": 1950, ',
      '"toYear": 2015, ',
      '"groupByYear": true, ',
      '"groupByFleet": true, ',
      '"groupByFisheryType": true, ',
      '"groupByFisheryGroup": true, ',
      '"groupByFishery": true, ',
      '"groupBySpeciesGroup": true, ',
      '"groupBySpecies": true, ',
      '"groupByArea": true, ',
      '"groupByQuality": true, ',
      '"species": [] ',
    '}'
  )

  req <- POST(url, accept_json(),
    add_headers("Content-Type" = "application/json"),
    add_headers("X-IOTC-STATS-API-key" = key),
    body = jfilter)
 
  if(http_error(req))
    stop(paste(http_status(req)$message, url, sep = " - "))

  nc <- data.table(fromJSON(content(req, "text", encoding="UTF-8")))
  setkey(nc, fleetCode)

  return(nc)
}
# }}}

# getCodeList {{{
getCodeList <- function(list, key,
  name=paste0(tolower(substring(list, 1, 1)), substring(list, 2))) {
  
  # CONSTRUCT url
  url <- paste0("http://statistics.iotc.org/rest/services/reference/codelists/retrieve/",
    list)
  
  # GET codelist
  req <- httr::GET(url, add_headers("X-IOTC-STATS-API-key" = key))

  # STOP if error
  if(http_error(req))
    stop(paste(http_status(req)$message, url, sep = " - "))
 
  # deJSON table
  res <- fromJSON(content(req, "text", encoding="UTF-8"))

  # SUBSET code and nameEn
  res <- data.table(res[["data"]][,c("code", "nameEn")])

  # RENAME to match list
  names(res) <- c(paste0(name, "Code"), name)

  setkeyv(res, paste0(name, "Code"))

  return(res)
} # }}}

# getCPCCodeList {{{
getCPCCodeList <- function(key) {
  
  list <- "Fleet"

  # CONSTRUCT url
  url <- paste0("http://statistics.iotc.org/rest/services/reference/codelists/retrieve/",
    list)
  
  # GET codelist
  req <- httr::GET(url, add_headers("X-IOTC-STATS-API-key" = key))

  if(http_error(req))
    stop(paste(http_status(req)$message, url, sep = " - "))
 
  # deJSON table
  res <- fromJSON(content(req, "text", encoding="UTF-8"))

  res <- data.table(res[["data"]][["fleetOfficial"]][,c("code", "nameEn", "nameFr")])

  setkey(res, code)

  return(res)
} # }}}
