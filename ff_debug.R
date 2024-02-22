library(data.table)

output = fread('output.csv')
ref = fread("usa_8tickers.csv")
ref[,be:=book_equity]
ref[,at:=assets]
check = output[ticker == 'AAPL.US' &  date > '1990-01-01' & date < '2019-01-01'] #
ref = ref[ symbol == 'AAPL' & date > '1990-01-01' & date < '2019-01-01']
tmp_names = names(ref)[names(ref) %in% names(data) & sapply(ref,is.numeric)]
tmp_names = sort(tmp_names)
ref = ref[,tmp_names,with=F]
check = check[,tmp_names,with=F]


t_val_results <- sapply(tmp_names, function(col_name) {
  t_val <- tryCatch(
    summary(lm(ref[[col_name]] ~ check[[col_name]]))$coefficients[,'t value'][2]
    , error = function(e) 0)
  return(t_val)
})

t_val_df<- data.table(feature = tmp_names, t_val = t_val_results)
t_val_df_sorted <- t_val_df[order(-t_val_df$t_val), ]
to_check = t_val_df_sorted[t_val_df_sorted$t_val < 2 & t_val_df_sorted$t_val != 0]$feature

print(paste0( round(100*(1- length(to_check)/nrow(t_val_df_sorted))), ' % of features passed the test'))


print('Features to check')
print(to_check) 
