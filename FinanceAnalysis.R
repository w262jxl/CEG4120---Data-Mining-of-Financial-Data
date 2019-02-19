install.packages("finreportr",dependencies = TRUE)
require("finreportr") #load the namespace of the package and attach it on search list
library(finreportr)


CompanyInfo("JPM")
CompanyInfo("GOOG")
AnnualReports("FB")

#Income Sheet
JPM13.IS <- GetIncome("JPM",2013)
isJPM13_df <-data.frame(JPM13.IS)
JPM15.IS <- GetIncome("JPM",2015)
JPM10.IS <- GetIncome("JPM",2010)
AAPL.IS <-GetIncome("AAPL",2011)


#Balance Sheet
JPM.BS <- GetBalanceSheet("JPM",2013)
FB.BS <- GetBalanceSheet("FB",2013)
AAPL.BS <- GetBalanceSheet("AAPL",2013)
AAPL15.BS <-GetBalanceSheet("AAPL",2015)

#Cash Flow
AAPL.SCF <- GetCashFlow("AAPL", 2013)
AAPL15.SCF <- GetCashFlow("AAPL", 2015)
