parseRawFundamentals = function(df) {
  # df= copy(earn)
  df2 = data.table()
  for (col in names(df)) {
    df[,paste0(col):=lapply(get(col), function(x) replace(x, is.null(x), NA))]
    raw = if( (is.numeric(df[[col]]) | is.character(df[[col]]) | is.na(df[[col]][1])) 
              & length(df[[col]])<=1) df[[col]] else do.call(c,df[[col]])
    raw <- tryCatch(as.numeric(raw), warning = function(w) raw)
    df2[,paste0(col):=raw]
  }
  df2
}

getFundamentals <- function(fund_data) {
  # fund_data = copy(parsed_data)
  bal <- t(fund_data$Financials$Balance_Sheet$quarterly)
  bal = as.data.table(do.call(rbind, bal))
  bal = parseRawFundamentals(bal)
  if(length(bal)==0) return(data.table())
  cf <- t(fund_data$Financials$Cash_Flow$quarterly)
  cf = as.data.table(do.call(rbind, cf))
  cf = parseRawFundamentals(cf)
  inc <- t(fund_data$Financials$Income_Statement$quarterly)
  inc = as.data.table(do.call(rbind, inc))
  inc = parseRawFundamentals(inc)
  earn <- t(fund_data$Earnings$History)
  earn = as.data.table(do.call(rbind, earn))
  earn = parseRawFundamentals(earn)
  
  # Merging them together
  data_tables <- list(bal, cf, inc, earn)
  data_tables <- data_tables[sapply(data_tables, function(dt) !is.null(dt) && nrow(dt) > 0)]
  
  # Merge the non-null and non-empty data.tables by date
  if (length(data_tables) > 0) {
    df <- Reduce(function(x, y) merge(x, y, by = "date", all = TRUE), data_tables)
  } else {
    df <- NULL
  }
  
  # Dropping redundant date and duplicate columns
  dup_cols <- grep("[.]y", names(df), ignore.case = TRUE, value = TRUE)
  df <- df[, !(names(df) %in% dup_cols),with=F]
  
  df = df[order(date)]
  df = df[date <= Sys.Date()]
  return(df)
}


eod_to_bryan_kelly = function(data) {
  #data = copy(fund_data)
  data = data[order(date)]
  
  # Accounting variables
  
  # Income statement
  data[,sale:=tryCatch(frollsum(totalRevenue,12, na.rm=T), error=function(e) 0),by=ticker]
  data[,cogs:=tryCatch(frollsum(costOfRevenue,12,na.rm=T),error = function(e) 0),by=ticker]
  data[,gp:=tryCatch(frollsum(grossProfit,12,na.rm=T),error = function(e) 0),by=ticker]
  data[,xsga:=tryCatch(frollsum(sellingAndMarketingExpenses,12,na.rm=T),error = function(e) 0),by=ticker]
  data[,xad:=0]
  data[,xrd:=tryCatch(frollsum(researchDevelopment,12,na.rm=T),error = function(e) 0),by=ticker]
  data[,xlr:=0]
  data[,spi:=0]
  data[,opex:=tryCatch(frollsum(totalOperatingExpenses,12,na.rm=T),error = function(e) 0),by=ticker]
  data[,ebitda:=tryCatch(frollsum(ebitda,12,na.rm=T),error = function(e) 0),by=ticker]
  data[,dp:=tryCatch(frollsum(depreciationAndAmortization,12,na.rm=T),error = function(e) 0),by=ticker]
  data[,ebit:=tryCatch(frollsum(ebit,12,na.rm=T),error = function(e) 0),by=ticker]
  data[,int:=tryCatch(frollsum(interestExpense,12,na.rm=T),error = function(e) 0),by=ticker]
  data[,op:=ebitda + fifelse(is.finite(xrd),xrd,0)]
  data[,ope:=ebitda - int]
  data[,nopi:=tryCatch(frollsum(nonOperatingIncomeNetOther,12,na.rm=T), error=function(e) 0),by=ticker]
  data[,pi:=ebit - int + fifelse(is.finite(spi),spi,0) + fifelse(is.finite(nopi),nopi,0)]
  data[,tax:=tryCatch(frollsum(incomeTaxExpense,12,na.rm=T), error = function(e) 0),by=ticker]
  data[,xi:=tryCatch(frollsum(extraordinaryItems,12,na.rm=T), error = function(e) 0),by=ticker]
  data[,do:=tryCatch(frollsum(discontinuedOperations,12,na.rm=T), error = function(e) 0),by=ticker]
  data[,xido:=xi + fifelse(is.finite(do),do,0)]
  data[,nix:=tryCatch(frollsum(netIncome,12, na.rm=T), error=function(e) 0),by=ticker]
  data[,ni:=nix - fifelse(is.finite(xido),xido,0)]
  data[,fi:=ni + int]
  data[,dvc:=tryCatch(frollsum(dividendsPaid,12, na.rm=T),error = function(e) 0),by=ticker]
  data[,div:=dvc]
  data[,ni_qtr:=tryCatch(na.locf0(netIncome),error = function(e) 0),by=ticker]
  data[,sale_qtr:=tryCatch(na.locf0(totalRevenue),error = function(e) 0),by=ticker]
  
  # Cash Flow Statement
  data[,capx:=tryCatch(frollsum(capitalExpenditures,12,na.rm=T),error = function(e) 0),by=ticker]
  data[,capex_sale:=capx/sale]
  data[,fcf:=tryCatch(frollsum(freeCashFlow,12,na.rm=T),error = function(e) 0),by=ticker]
  data[,eqbb:=tryCatch(frollsum(fifelse(is.finite(salePurchaseOfStock),-salePurchaseOfStock,0),12,na.rm=T)
                       ,error = function(e) 0),by=ticker]
  data[,eqis:=tryCatch(frollsum(fifelse(is.finite(issuanceOfCapitalStock),issuanceOfCapitalStock,0),12,na.rm=T),error = function(e) 0),by=ticker]
  data[,eqnetis:=fifelse(is.finite(eqis),eqis,0) - fifelse(is.finite(eqbb),eqbb,0)]
  data[,eqpo:=div + eqbb]
  data[,eqnpo:=div - eqnetis]
  data[,dltnetis:=tryCatch(na.locf0(longTermDebt) - shift(na.locf0(longTermDebt),12,type='lag'),error = function(e) 0),by=ticker]
  data[,dstnetis:=tryCatch(na.locf0(shortTermDebt) - shift(na.locf0(shortTermDebt),12,type='lag'),error = function(e) 0),by=ticker]
  data[,dbnetis:=fifelse(is.finite(dltnetis),dltnetis,0) + fifelse(is.finite(dstnetis),dstnetis,0)]
  data[,netis:=eqnetis + dbnetis]
  data[,fincf:=tryCatch(frollsum(totalCashFromFinancingActivities,12,na.rm=T),error = function(e) 0),by=ticker]
  
  # Balance Sheet - Assets
  data[,at:=tryCatch(na.locf0(totalAssets),error = function(e) 0),by=ticker]
  data[,ca:=tryCatch(na.locf0(totalCurrentAssets),error= function(e) 0),by=ticker]
  data[,rec:=tryCatch(na.locf0(netReceivables),error = function(e) 0),by=ticker]
  data[,cash:=tryCatch(na.locf0(cash),error = function(e) 0),by=ticker]
  data[,inv:=tryCatch(na.locf0(inventory),error = function(e) 0),by=ticker]
  data[,nca:=tryCatch(na.locf0(nonCurrentAssetsTotal),error = function(e) 0),by=ticker]
  data[,intan:=tryCatch(na.locf0(intangibleAssets),error = function(e) 0),by=ticker]
  data[,ivao:=tryCatch(na.locf0(investments),error = function(e) 0),by=ticker]
  data[,ppeg:=tryCatch(na.locf0(propertyPlantAndEquipmentGross),error = function(e) 0),by=ticker]
  data[,ppen:=tryCatch(na.locf0(propertyPlantEquipment),error = function(e) 0),by=ticker]
  
  # Balance Sheet - Liabilities
  data[,lt:=tryCatch(na.locf0(totalLiab),error = function(e) 0),by=ticker]
  data[,cl:=tryCatch(na.locf0(totalCurrentLiabilities),error = function(e) 0),by=ticker]
  data[,ap:=tryCatch(na.locf0(accountsPayable),error = function(e) 0),by=ticker]
  data[,debtst:=tryCatch(na.locf0(shortTermDebt),error = function(e) 0),by=ticker]
  data[,txp:=tryCatch(na.locf0(incomeTaxExpense),error = function(e) 0),by=ticker]
  data[,ncl:=tryCatch(na.locf0(nonCurrentLiabilitiesTotal),error = function(e) 0),by=ticker]
  data[,debtlt:=tryCatch(na.locf0(longTermDebt),error = function(e) 0),by=ticker]
  data[,txditc:=0]
  
  # Balance Sheet - Financing
  data[,pstk:=tryCatch(na.locf0(preferredStockTotalEquity),error = function(e) 0),by=ticker]
  data[,debt:=fifelse(is.finite(debtlt),debtlt,0) + fifelse(is.finite(debtst),debtst,0)]
  data[,netdebt:=tryCatch(na.locf0(netDebt),error = function(e) 0),by=ticker]
  data[,seq:=tryCatch(na.locf0(totalStockholderEquity),error = function(e) 0),by=ticker]
  data[,be:=seq + fifelse(is.finite(txditc),txditc,0) - fifelse(is.finite(pstk),pstk,0)]
  data[,mib:=tryCatch(na.locf0(temporaryEquityRedeemableNoncontrollingInterests),error=function(e) 0),by=ticker]
  data[,bev:=seq + netdebt + fifelse(is.finite(mib),mib,0)]
  
  # Balance Sheet - Summary
  data[,nwc:=ca+cl]
  data[,coa:=ca-cash]
  data[,col:=cl -  0]
  data[,cowc:=coa + col]
  data[,ncoa:=at-ca-ivao]
  data[,ncol:=lt - cl - dltnetis]
  data[,nncoa:=ncoa - ncol]
  data[,ivst:=tryCatch(na.locf0(shortTermInvestments),error=function(e) 0),by=ticker]
  data[,fna:=fifelse(is.finite(ivst),ivst,0) + fifelse(is.finite(ivao),ivao,0)]
  data[,fnl:=debt + fifelse(is.finite(pstk),pstk,0)]
  data[,nfna:=fna - fnl]
  data[,oa:=coa + ncoa]
  data[,ol:=col + ncol]
  data[,noa:=oa + ol]
  data[,ao:=tryCatch(na.locf0(otherAssets), error=function(e) 0),by=ticker]
  data[,lo:=tryCatch(na.locf0(otherLiab), error=function(e) 0),by=ticker]
  data[,lnoa:=ppen + intan + ao + lo + dp]
  data[,caliq:=ca - inv]
  data[,ppeinv:=ppen + inv]
  data[,aliq:=cash+0.75*coa+0.5*(at-ca-intan)]
  
  
  # Market Based
  data[,tmp_shares:=tryCatch(na.locf0(abs(commonStockSharesOutstanding)),error=function(e) 0),by=ticker]
  data[,shares:=tryCatch(tmp_shares/na.locf0(adjfct, fromLast=T),error = function(e) 0),by=ticker]
  data[,me:=shares*abs(close) * na.locf0(adjfct, fromLast=T)] 
  data[,mev:=me + netdebt]
  data[,mat:=at - be + mev]
    
  # Accruals
  data[,oancf:=tryCatch(frollsum(totalCashFromOperatingActivities,12,na.rm=T), error=function(e) 0),by=ticker]
  data[,oacc:=ni - oancf]
  data[,tacc:=oacc + (nfna - shift(nfna,12,type='lag')),by=ticker]
  data[,ocf:=oancf]
  data[,ocf_qtr:=tryCatch(na.locf0(totalCashFromOperatingActivities), error=function(e) 0),by=ticker]
  data[,cop:=ebitda + fifelse(is.finite(xrd),xrd,0) - oacc]
  
  data[,roe:=ni / shift(be,12,type='lag'),by=ticker]
  data
}