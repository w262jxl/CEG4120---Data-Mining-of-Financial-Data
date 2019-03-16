install.packages("finreportr",dependencies = TRUE)
require("finreportr") #load the namespace of the package and attach it on search list
library(finreportr)

install.packages("rvest", dependencies = TRUE)
library(rvest)

install.packages("xml2",dependencies = TRUE)
library(xml2)


library(shiny)
#?install.packages


CompanyInfo("JPM")
CompanyInfo("GOOG")
AnnualReports("FB")

#Income Sheet
JPM13.IS <- GetIncome("JPM",2013)
JPM15.IS <- GetIncome("JPM",2015)
JPM10.IS <- GetIncome("JPM",2010)
AAPL.IS <-GetIncome("AAPL",2011)
AAPL17.IS <-GetIncome("AAPL", 2017)
AAPL18.IS <-GetIncome("AAPL", 2018)
FB13.IS <-GetIncome("FB", 2013)
WMT13.IS <-GetIncome("WMT", 2013)

#Balance Sheet
JPM.BS <- GetBalanceSheet("JPM",2013)
FB.BS <- GetBalanceSheet("FB",2013)
AAPL.BS <- GetBalanceSheet("AAPL",2013)
 AAPL15.BS <-GetBalanceSheet("AAPL",2015)
TGT12.BS <-GetBalanceSheet("TGT",2011)
TGT14.BS <-GetBalanceSheet("TGT",2014)

#Cash Flow
AAPL.SCF <- GetCashFlow("AAPL", 2013)
AAPL15.SCF <- GetCashFlow("AAPL", 2015)
WMT.SCF <- GetCashFlow("WMT",2013)
FB17.SCF <- GetCashFlow("FB", 2017)

############

write.csv(FB17.SCF,'FB17SCF.CSV') #save as .csv
write.csv(AAPL15.BS,'AAPL15.txt') #save as .txt
read.csv(AAPL15.txt) 


#?read.csv
MyData <- read.csv(file = "C:/Users/dang1/Documents/DataProject/AAPL15.csv", header=TRUE, sep=",")
write.table(MyData,"C:/Users/dang1/Documents/DataProject/AAPL15.txt", quote = FALSE, sep = "" ,row.names = TRUE, col.names = TRUE)



