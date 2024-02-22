# Install dependencies
chooseCRANmirror(ind=1)
list.of.packages <- c("data.table","devtools","RCurl","jsonlite", "eodhd")
new.packages <- list.of.packages[!(list.of.packages %in% 
                                     installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
install_github("joshuaulrich/rfimport")
library(rfimport)

# Download US index
index <- as.data.table(import_ohlc(sym_yahoo("^GSPC"), dates='1985/'))
fwrite(index, "GSPC.csv")

# Download risk free rate
rf_free <- as.data.table(import_series(sym_fred("DTB3")))
fwrite(rf_free, "rf_free.csv")

# Download AAPL
aapl <- as.data.table(import_ohlc(sym_yahoo("AAPL"), dates='1985/'))
aapl[,ticker:='AAPL.US']
fwrite(aapl, "AAPL.US_eod.csv")

# Download exchanges
api_token = "61b967af5d0035.68435230" # Demo API token
exchanges = get_list_of_exchanges(api_token)
exchanges = as.data.table(do.call(rbind,sapply(exchanges,unlist)))

# Merge exchanges with respective Index
code_country = fread('code_country.csv')
code_country[,Index:=Code2]
index_eodhd_reference = merge(code_country[,c("Country","Index")], exchanges[,c('Country','Code')], by='Country')
fwrite(index_eodhd_reference, "index_eodhd_reference.csv")

# Load fundamentals
aapl_fund <-get_fundamentals_data(api_token,"AAPL.US")
aapl_fund_dt = getFundamentals(aapl_fund)
aapl_fund_dt[,ticker:='AAPL.US']
fwrite(aapl_fund_dt, "AAPL.US_fund.csv")

# Load splits 
aapl_split <-get_historical_splits_data(api_token,"AAPL.US")
aapl_split_dt = as.data.table(t(sapply(aapl_split,unlist)))
aapl_split_dt[,ticker:='AAPL.US']
fwrite(aapl_split_dt, "AAPL.US_split.csv")

