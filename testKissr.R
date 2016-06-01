files <- c("R/utilities.R",
           "R/read.R",
           "R/kiss-report.R",
           "R/kiss-reports.R",
           "R/kissr.R")
lapply(files, source)

library(dplyr)
library(devtools)
library(lubridate)
library(reshape2)

Sys.setenv(KM_API_TOKEN='ZZT0J_ii9OYKl1W5vw6nkKiM17LTT3dgVZO--HWeVsuAII-gdQWKfVUR3zEF3qrF')
startDate <- as.Date("2016-04-19")
endDate <- as.Date("2016-04-19")
KM_Report_Name <- "bi-906-pricingpage-01"

reportInfo  <- read(KissReports()) %>%
  filter( report_type == "people_search_v2" &
            name==KM_Report_Name)

reportUrl <- reportInfo$url
report <- KissReport(reportUrl,
                     interval = lubridate::interval(startDate, endDate),
                     columnNames = c("KM_Email",
                                     "KM_Test_Group",
                                     "First_Recurly_Registration_Date",
                                     "Registration_Stage",
                                     "Starter_Monthly_Signup_Date",
                                     "Starter_Annual_Signup_Date",
                                     "pro99_Monthly_Signup_Date",
                                     "pro99_Annual_Signup_Date",
                                     "pro199_Monthly_Signup_Date",
                                     "pro199_Annual_Signup_Date"))
pullReportData <- browser(read(report))

####

reportInfo2  <- read(KissReports()) %>%
  filter( report_type == "people_search_v3" &
            name == "Fact_KM_Visit")

reportUrl2 <- reportInfo2$url
report2 <- KissReport(reportUrl2,
                     interval = lubridate::interval(startDate, endDate),
                     columnNames = c("KM_Email",
                                     "KM_Viewed_Url_First_Time",
                                     "KM_Viewed_Url_Last_Time",
                                     "KM_Viewed_Url_First_Value",
                                     "KM_Viewed_Url_Last_Value"))
pullReportData2 <- read(report2)
