# Install dependencies
chooseCRANmirror(ind=1)
list.of.packages <- c("data.table","foreach","doParallel","doMC","bizdays",'bit64',
                      "stringr", "anytime", "xts", "roll","qlcal","bit64",
                      "moments","bidask", "fasttime")
new.packages <- list.of.packages[!(list.of.packages %in% 
                                     installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

# Multi-threaded environment setup
os_type= Sys.info()["sysname"]
cores = detectCores() 

if (os_type == "Windows") {
  registerDoParallel(cores=cores)
} else {
  registerDoMC(cores=cores)
}

# Load files and set calendar
source('eodhd_naming_convention.R')
source('factor_factory.R')
if(!file.exists("AAPL.US_split.csv")) source('required_data_for_simulation.R')
setCalendar('UnitedStates/NYSE')

# Load matching indices
code_country = fread('index_eodhd_reference.csv')

# Find Fundamental files
path_fundamentals = list.files(recursive=T, pattern='_fund.csv',full.names = T)
path_eods = list.files(recursive=T, pattern='_eod.csv', full.names = T)
path_index = list.files(pattern = 'GSPC',full.names = T)
path_rf = list.files(pattern = 'rf',full.names = T)
path_splits = list.files(pattern = '_split.csv',full.names = T)

# Load fundamental_data 
load_fundamentals = function(path_fundamentals) {
  # path_fundamentals = tail(path_fundamentals, 100)
  if(os_type == 'Windows') {
    fund_files = lapply(path_fundamentals, function(x) {
      #x = path_fundamentals
      file = fread(x)
      exch = strsplit(x, '[.]|_')[[1]]
      file[,exchange:=exch[length(exch)-2]]
      file[,index:=code_country$Index[which(file$exchange[1] == code_country$Code)]]
      file
    })
  } else {
    fund_files = mclapply(path_fundamentals, function(x) {
      file = fread(x)
      exch = strsplit(x, '[.]|_')[[1]]
      file[,exchange:=exch[length(exch)-2]]
      file[,index:=code_country$Index[which(file$exchange[1] == code_country$Code)]]
    },mc.cores = cores)
  }
  removal = sapply(fund_files, function(x) nrow(x) < 2 | is(x, 'try-error'))
  if(any(removal==T)) fund_files = fund_files[!removal]
  
  # Get the column names of all files
  all_cols <- unique(unlist(lapply(fund_files, colnames)))
  fund_files = lapply(fund_files, function(x) {
    if (length(colnames(x)) < length(all_cols)) {
      missing_cols <- setdiff(all_cols,colnames(x))
      x[,paste0(missing_cols):=NA]
    }
    x = setcolorder(x, sort(colnames(x)))
    x[,reportDate:=NULL]
    x[,filing_date:=NULL]
    x[,filing_date.x:=NULL]
    
    numerics = names(which(sapply(x, is.numeric)==T))
    # /1e6 for avoiding bit64
    x[,(numerics):=.SD/1e6,.SDcols = numerics]
  })
  all_files = do.call(rbind,fund_files)
  all_files =all_files[order(date)]
  # BELOW: SHIFTING BY 3MONTHS VALUES
  # FIXME: add synthetic values for missing quarters from yearly
  # Assuming quarter
  # all_files[order(date),diff_date:=c(diff(date),90),by=ticker]
  all_files[,original_date:=date]
  # Quick check if it follows next quarter
  all_files = all_files[order(date)]
  all_files[,date:=shift(date,1,type='lead',fill=NA),by=ticker]
  set(all_files, which(is.na(all_files$date)), 'date',Sys.Date())
  all_files[,diff_date:=original_date - date]
  all_files = all_files[(abs(diff_date)>80 & abs(diff_date)<100) | date == Sys.Date()]
  all_files =all_files[is.finite(date)] 
  all_files = all_files[order(date)]
  ##fwrite(all_files,paste0(path_output,'option_fundamentals.csv'))
  all_files
}

# Load eod data
load_eods = function(path_eods) {
  # path_eods = tail(path_eods, 100)
  if(os_type == 'Windows') {
    # FIXME: parLapply ?
    eod_files = lapply(path_eods, function(x) {
      #x = path_eods
      file = fread(x)
      exch = strsplit(x, '[.]|_')[[1]]
      file[,exchange:=exch[length(exch)-2]]
      file[,date:=index]
      file[,index:=NULL]
      file[,index:=code_country$Index[which(file$exchange[1] == code_country$Code)]]
    })
  } else {
    eod_files = mclapply(path_eods, function(x) {
      file = fread(x)
      exch = strsplit(x, '[.]|_')[[1]]
      file[,exchange:=exch[length(exch)-2]]
      file[,date:=index]
      file[,index:=NULL]
      file[,index:=code_country$Index[which(file$exchange[1] == code_country$Code.y)]]
    },mc.cores = cores)
  }
  removal = sapply(eod_files, function(x) nrow(x) < 2 | is(x, 'try-error'))
  if(any(removal==T)) eod_files = eod_files[!removal]
  all_files = do.call(rbind,eod_files)
  names(all_files) = tolower(names(all_files))
  ##fwrite(all_files,paste0(path_output,'option_eods.csv'))
  all_files
}

# Load respective index based on Country and US risk free rate
load_index_and_riskfree_rate = function(eod_data) {
  # eod_data = load_eods(path_eods)
  index = fread(path_index)
  r <- fread(path_rf)
  names(r) = tolower(names(r))
  names(index) = tolower(names(index))
  r[,date:=index]
  
  # Convert yearly to monthly interest rate
  r[,close:=dtb3]
  r = r[,c('date','close'),with=F] #close
  r[,r:= (1 + close/100)^(1/252) - 1]
  r[,rf_monthly:= (1 + close/100)^(30/252) - 1]
  r[,close:=NULL]
  colnames(index)[1] = 'date'
  colnames(index)[2:length(colnames(index))] = paste0('mkt',colnames(index)[2:length(colnames(index))])
  market_vars = merge(index,r, by='date')
  # Compute market index returns
  market_vars[,mktref:=(mktadjusted/shift(mktadjusted,1,type='lag',fill=NA) -1)- r] #_close
  
  # Merge it with eod data
  eod_data = merge(eod_data,market_vars,by='date',all=T)
  eod_data = eod_data[order(date)]
  
  # Quick check if it follows next working day
  eod_data[,oracle_lag_date:=as.IDate(advanceUnits(date[1],-1)),by=date]
  eod_data[,actual_lag_date:=shift(date,1,type='lag',fill=NA),by=ticker]
  eod_data = eod_data[actual_lag_date == oracle_lag_date]
  #eod_data = eod_data[date > '1990-01-01']
  
  # Fill missing values with previous values
  for(i in names(market_vars)[-which(names(market_vars)=='date')]) 
    eod_data[,paste0(i):=na.locf0(get(paste0(i))),by=ticker]
  eod_data
}

# Load splits
load_splits = function(path_splits) {
  data = lapply(path_splits,fread)
  whi = which(sapply(data,nrow)!=0)
  if(length(whi)) data = data[whi]
  data = do.call(rbind, data)
  #data[,date:=Date]
  data[,c('first','second'):=lapply(tstrsplit(get('split'),'/'),as.numeric)]
  data[,adj_fac:=first/second]
  last_row = data.table(date = as.IDate(rep(Sys.Date(), length(unique(data$ticker)))),
                        ticker = sort(unique(data$ticker)),
                        adj_fac=rep(1,length(unique(data$ticker)))) 
  all = rbind(data[,c('date','ticker','adj_fac')],last_row)
  all = all[order(-date)]
  all[,adjfct:=cumprod(adj_fac),by=ticker]
  all
  #splits = copy(all)
}

# Merge splits with eod
merge_eod_splits = function(eod_data, splits) {
  all = merge(eod_data, splits[,c('date','ticker','adjfct')], by=c('date','ticker'),all=T)
  all = all[order(-date)]
  all[, adjfct :=  ifelse(.I == 1L | all(!is.finite(adjfct)), 1, adjfct), by = ticker]
  all[,adjfct:= na.locf0(adjfct),by=ticker]
  #all = all[date > '1990-01-01']
  all = all[order(date)]
  #all[ticker == 'AAIIQ'][,c('date','ticker','close','adjfct')]
  all = all[is.finite(close)] # adjusted_
  all
}

# Merge data for fundamental factors creation
merge_fund_data = function(fund_data, eod_data) {
  #fund_data = load_fundamentals(path_fundamentals)
  
  # Get EOD end of month attached to fundamentals
  eod_data[,year_month:=paste0(year(date),'-',month(date))]
  fund_data[,year_month:=paste0(year(date),'-',month(date))]
  
  # Aggregate values per month
  eod_data[,prc_high:=max(abs(high),na.rm=T),by=list(ticker,year_month)]
  eod_data[,prc_low:=min(abs(low),na.rm=T),by=list(ticker,year_month)]
  eod_data[,volume:=as.integer(sum(abs(volume),na.rm=T)/1e3)
           , by = .(ticker, year_month)]
  eod_data[,eod_date:=date]
  
  # Keep month-end dates and prediction date (with most recent data)
  eod_data = eod_data[,.SD[isEndOfMonth(date) | date == Sys.Date()],by=list(year_month,ticker)]
  
  # FIXME: missing dates because of discontinuation --> Check prev month == lag month before making predictions
  # eod_data2[,dif:=c(0,diff(date)),by=ticker]
  # table(ifelse(eod_data2$dif > 20 & eod_data2$dif < 40,1,0)) / nrow(eod_data2)
  # table(eod_data2$dif)
  # eod_data2[dif == max(dif)]
  # eod_data2[ticker == 'AAXT'] # & year_month > '2019-01'
  
  # Merge eod and fundamentals
  eod_names = c('eod_date','open','prc_high','prc_low','close','volume','adjfct')
  data = merge(fund_data, eod_data[,c('year_month','ticker',eod_names),with=F], 
               by=c('year_month','ticker'),all=T)
  data = data[order(eod_date)]
  
  data
}

# Merge fundamentals to eod for daily factors creation
merge_eod_data = function(fund_data2, eod_data) {
  # Concatenate fund data to eod for FFM...
  cols_from_fund_data = c('shares','be','roe','inv')
  data = merge(eod_data,fund_data2[,c('date','ticker',cols_from_fund_data),with=F],
               by=c('date','ticker'),all=T)
  data = data[order(date)]
  
  # Fill missing values with previous values
  data[,(cols_from_fund_data):=lapply(.SD, 
                                      function(x) {
                                        locf = x[cummax((!is.na(x)) * seq_along(x))]
                                        # FIXME: necessary ?
                                        if(class(x) == 'integer64') c.integer64(rep(NA,length(x) - length(locf)),locf) else
                                          c(rep(NA,length(x) - length(locf)),locf)
                                      })
       ,.SDcols=cols_from_fund_data , by=ticker]
  eod_data = data[complete.cases(open)]
  eod_data
}

# Merge daily factors with fundamental factors
merge_fund_data_daily_factors = function(fund_data, daily_fac) {
  # fund_data = copy(fund_data2)
  daily_fac[,year_month:=paste0(year(date),'-',month(date))]
  fund_data[,year_month:=paste0(year(date),'-',month(date))]
  
  daily_fac = daily_fac[,.SD[isEndOfMonth(date)],by=ticker]
  daily_fac[, date2:=date]
  
  cols_to_keep = names(daily_fac)[!names(daily_fac) %in% names(fund_data)]
  all_fund_data = merge(fund_data,daily_fac[,c('year_month','ticker',cols_to_keep),with=F],
                        by=c('year_month','ticker'), all=T)
  
  all_fund_data = all_fund_data[order(date2,ticker)]
  sdcols = names(all_fund_data)[sapply(all_fund_data,is.numeric)]
  all_fund_data[, (sdcols):=lapply(.SD,na.locf0),.SDcols=sdcols,by=ticker]
  
  all_fund_data[,me_lag1:=shift(me, 1, type='lag',fill=0),by=ticker]
  
  # all_fund_data[,diff(date),by=ticker]
  ##fwrite(all_fund_data,paste0(path_output,'monthly_data.csv'))
  all_fund_data
}

main = function() {
  fund_data = load_fundamentals(path_fundamentals)
  eod_data = load_eods(path_eods)
  eod_data = load_index_and_riskfree_rate(eod_data)
  splits = load_splits(path_splits)
  # Below still quarterly
  eod_data = merge_eod_splits(eod_data, splits)
  # Below monthly 
  fund_data = merge_fund_data(fund_data, eod_data)
  fund_data[,netIncome:=netIncome.x]
  fund_data[,date:=eod_date]
  fund_data2 = eod_to_bryan_kelly(fund_data)
  # FIXME: check why there are missing dates --> missing latest month actually you need to fill missing values with previous ones
  fund_data = fund_data2[is.finite(date)]
  eod_data = merge_eod_data(fund_data, eod_data)
  daily_fac = load_daily_factors_fast(eod_data)
  # Below monthly
  data = merge_fund_data_daily_factors(fund_data2, daily_fac)
  # Replace not finite values with NAs
  data[,(names(which(sapply(data,is.numeric)==T))):=lapply(.SD,function(x) replace(x,!is.finite(x),NA)),by=ticker,
       .SDcols=names(which(sapply(data,is.numeric)==T))]
  data2 = get_accounting_factors(data)
  # Replace zeros with missing values
  data2[,(names(which(sapply(data,is.numeric)==T))):=lapply(.SD,function(x) replace(replace(x,!is.finite(x),NA),x==0.,NA))
        ,.SDcols=names(which(sapply(data,is.numeric)==T))]
  data3 = get_market_factors(data2)
  output = quality_minus_junk(data3)
  #fwrite(data, paste0(path_output,'tmp_data_first_pass.csv'))
  output
}

output = main()
fwrite(output, "output.csv")

source('ff_debug.R')
