#Sean Hyland
library(httr)
library(jsonlite)
library(lubridate)
library(plyr)
library(dplyr)
library(readr)
library(pander)
library(RCurl)
library(RJSONIO)
library(sendmailR)

#Get date and time range
Date <- as_date(paste(format_ISO8601(Sys.time(), tz = "UTC"),"Z", sep = ""), tz = NULL)


#Get the data for which spaces have been occupied for the day.
text <- GET(paste("https://fnc-customerapi-pr.azurewebsites.net/api/v2/spaces/getByHour?buildingId=xxxxx&startTime=",Date,"T05%3A00%3A00.000Z&endTime=",Date,"T17%3A00%3A00.000Z&pageSize=10000",sep=""),
            add_headers("Accept" = "application/json",
                        "Authorization" = "Bearer API_Code_Provided_By_Relogix"))
text <- fromJSON(rawToChar(text$content))
sensors <- do.call(rbind.data.frame, text$spaces)
sensors <- sensors[sensors$fullnessPercent != 0,]
sensors <- sensors[!duplicated(sensors$spaceId),]
sensors$id <- as.numeric(sensors$id)

locations <- GET("https://fnc-customerapi-pr.azurewebsites.net/api/v2/buildings/xxxxxx/floor/xxxxx/spaces",
                 add_headers("Accept" = "application/json",
                             "Authorization" = "Bearer API_Code_Provided_By_Relogix"))
locations <- fromJSON(rawToChar(locations$content))
location <- data.frame(spaceId = integer(),
                       name = character(),
                       stringsAsFactors = FALSE)

for(i in 1:length(locations)){
  location[i,1] <- locations[[i]][1]
  location[i,2] <- locations[[i]][2]
}
location <- merge(x = sensors, y = location, by="spaceId", all.x = TRUE)
location <- location$name

#Prepare email for distribution
#Remove the first row of Final since this was only used to prepare the data frame
if (length(location) <1) {
  from <- sprintf("<name@company.com>","The Sender") # the sender's name is an optional value
  to <- sprintf(c("<name@company.com>"))
  subject <- "No spaces needing to be cleaned today"
  body <- "There are no spaces in location that need to be cleaned today as derived from the Relogix sensors."
  sendmail(from,to,subject,body,control=list(smtpServer= "smtp.company.com"))
} else if 
(length(location) <2) {
  Finals <- location
  Finals <- paste(Finals, collapse=', ')
  Finals <- paste("The following space needs to be cleaned as of ", Date, ":", Finals, sep = " ")
  from <- sprintf("<name@company.com>","The Sender") # the sender's name is an optional value
  to <- sprintf(c("<name@company.com>"))
  sprintf(c("<name@company.com>"))
  subject <- "Daily notifications of what spaces need to be cleaned as part of NEXT as derived from the Relogix sensors."
  body <- Finals
  sendmail(from,to,subject,body,control=list(smtpServer= "smtp.company.com"))
} else {
  Finals <- location
  Finals <- paste(Finals, collapse=', ')
  Finals <- paste("The following spaces need to be cleaned as of ", Date, ":", Finals, sep = " ")
  from <- sprintf("<name@company.com>","The Sender") # the sender's name is an optional value
  to <- sprintf(c("<name@company.com>"))
  subject <- "Daily notifications of what spaces need to be cleaned as part of NEXT as derived from the Relogix sensors."
  body <- Finals
  sendmail(from,to,subject,body,control=list(smtpServer= "smtp.company.com"))
}
