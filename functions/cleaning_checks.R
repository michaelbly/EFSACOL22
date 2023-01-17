# DO CLEANING
# read cleaning conditions csv list
conditionDf_1 <- read.csv("data/conditions/v2_conditions_log.csv", as.is = TRUE, sep = ";")

#debug(read_conditions_from_excel_limited_row)

# return logs
logs <- read_conditions_from_excel_limited_row(df, conditionDf_1);


# create new columns "log_number"
logs$log_number = seq.int(nrow(logs))
# order data frame by log_number
ordered_df <- logs[order(logs$log_number),]

incoherences_registro <- data.frame(table(ordered_df$registro))
names(incoherences_registro) <- c("registro", "nr_inhoherencias")
#incoherences_registro$registro <- as.numeric(as.factor(incoherences_registro$registro))

