
list1 <- c("item 1","item 2")
list1 <- c(list1, c("item 3"))
list1


regex.test <- c("2.3","2","1.4",".4","45","A4","2.3.4")
density.regex <- "^[0-9]*$"
density.grepl <- grepl(density.regex, regex.test, perl=TRUE, ignore.case = TRUE)
density.grepl

density.regex <- "([0-9]*)+([\\.]{1})([0-9]*)"
density.grepl <- grepl(density.regex, regex.test, perl=TRUE, ignore.case = TRUE)
regex.test
density.grepl

# =======================================================
# postcode
# =======================================================
regex.postcode <- c("HP11 2AX","SW19 8LZ")
density.regex = "^([A-PR-UWYZ0-9][A-HK-Y0-9][AEHMNPRTVXY0-9]?[ABEHMNPRVWXY0-9]? {1,2}[0-9][ABD-HJLN-UW-Z]{2}|GIR 0AA)$"
density.grepl <- grepl(density.regex, regex.postcode, perl=TRUE, ignore.case = TRUE)

density.grepl


# =======================================================
# datedd-mm-yyyy
# =======================================================
data.date <- c("1-1-2016","04/09/2013","30/12/2014")

regex.day = "^(0?[1-9]|1[0-9]|2[0-9]|3[0-1])" # 1-31, with optional leading zero
regex.month = "(0?[1-9]|1[1-2])" # 1-12, with optional leading zero
regex.year = "([1-2][7890][0-9][0-9])" # matches 1000 to 2999
regex.date = paste(regex.day,"(/)",regex.month,"(/)",regex.year,sep="")

data.date.log <- grepl(regex.date, data.date, perl=TRUE, ignore.case = TRUE)
data.date.log

#
# =======================================================
# scale210120
# =======================================================
data.scale <- c("Very likely","Quite likely","Neither way","Quite unlikely","Very unlikely","DK","Don't know","N/A","NA","Not answered")
data.scale.log <- grepl("^(very.)|(quite.)|(neither.)|(neutral.)|(DK)|(don't .)|(na)|(Not answered)", data.scale, perl=TRUE, ignore.case = TRUE)
data.scale.log
# #
# # =======================================================
# # numerics
# # =======================================================
# data.num <- c(1,2.3,0,45)
# data.num.log <- grepl("^[0-9]*$", data.num)             # only numbers
# data.num.decimal.log <- grepl("^[0-9\\.]*$", data.num)  # numbers and dots
# 
# # =======================================================
# # Excel scientific
# # =======================================================
# data.test <- c("8.89E+17","1.30E+11","-11316","7.32E+11","8.00E-17","-7.00E-24")
# data.testcleaned <- gsub("[[:space:]]", "", data.test) 
# data.regex <- "(-)?([0-9]\\.)(E\\+|E-)([0-9])"
# data.regex <- "(-)?([0-9]\\.?)(E)(\\+|-)([0-9])"
# data.regex.log <- grepl(data.regex, data.testcleaned, perl=TRUE, ignore.case = TRUE)
# data.regex.log
# 
# # =======================================================
# # x10
# # =======================================================
# data.test <- c("8.89 x10+17","1.30 X10+11","-11316","7.32x10+11","8.00X10-17","-7.00 x10 -24")
# data.testcleaned <- gsub("[[:space:]]", "", data.test) 
# data.regex <- "(-)?([0-9]\\.)(x10\\+|x10-)([0-9])"
# data.regex <- "(-)?([0-9]\\.?)(x10)(\\+|-)([0-9])"
# data.regex.log <- grepl(data.regex, data.testcleaned, perl=TRUE, ignore.case = TRUE)
# data.regex.log
# 
# # =======================================================
# # percentages
# # =======================================================
# data.pcent <- c("1%","2.3%","200 %", "- 40%", "10 percent","+22.4%")
# data.pcentcleaned <- gsub("[[:space:]]", "", data.pcent) 
# data.pcent.log <- grepl("^[0-9\\.\\%\\+-]*$|^[0-9\\.\\+percent-]*$", data.pcentcleaned, perl=TRUE, ignore.case = TRUE)  # numbers and dots
# 
# # =======================================================
# # currency
# # =======================================================
# data.test <- c("£30.00","$42","42.00")
# data.testcleaned <- gsub("[[:space:]]", "", data.test) 
# data.test.log <- grepl("^([£\\$])([0-9\\.])", data.testcleaned, perl=TRUE, ignore.case = TRUE)  # numbers and dots
# data.test.log
# 
# # =======================================================
# # logical
# # =======================================================
# data.bit <- c(0,1,2,1,0)
# data.bit.log <- grepl("^[1]$|^[0]$", data.bit)
# 
# data.yesno <- c("yes","yesterday","No","YES","No","Maybe")
# data.yesno.log <- grepl("^[yes]*$|^[no]*$", data.yesno, perl = TRUE, ignore.case = TRUE)
# 
# data.tf <- c("true","True","FALSE","Truely","Falseify")
# data.tf.log <- grepl("^[true]*$|^[false]*$", data.tf, perl = TRUE, ignore.case = TRUE)
# 
# # =======================================================
# # emails
# # =======================================================
# # General Email Regex (RFC 5322 Official Standard)
# # http://emailregex.com/
# 
# data.test <- c("suzanne_fox@hotmail.com","fred@fred.co.uk","jim@bloggs")
# data.testcleaned <- gsub("[[:space:]]", "", data.test) 
# data.regex = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,6}"
# data.regex.log <- grepl(data.regex, data.testcleaned, perl=TRUE, ignore.case = TRUE)
# data.regex.log
# 
# # =======================================================
# # urls
# # =======================================================
# data.test <- c("https://fgfh.com","www.fred.co.uk","http://x.x","myspace.com","fred.www.ha")
# data.testcleaned <- gsub("[[:space:]]", "", data.test) 
# data.regex = "^(https://)|^(www.)|^(http://)"
# data.regex.log <- grepl(data.regex, data.testcleaned, perl=TRUE, ignore.case = TRUE)
# data.regex.log
# 
# # =======================================================
# # UK Postcode
# # =======================================================
# # http://stackoverflow.com/questions/164979/uk-postcode-regex-comprehensive
# data.test <- c("SW19 8lz","kt34jl","xyz567")
# data.regex = "^(([gG][iI][rR] {0,}0[aA]{2})|((([a-pr-uwyzA-PR-UWYZ][a-hk-yA-HK-Y]?[0-9][0-9]?)|(([a-pr-uwyzA-PR-UWYZ][0-9][a-hjkstuwA-HJKSTUW])|([a-pr-uwyzA-PR-UWYZ][a-hk-yA-HK-Y][0-9][abehmnprv-yABEHMNPRV-Y]))) {0,}[0-9][abd-hjlnp-uw-zABD-HJLNP-UW-Z]{2}))$"
# data.regex.log <- grepl(data.regex, data.test, perl=TRUE, ignore.case = TRUE)
# data.regex.log

# #
# # =======================================================
# # correlations
# # =======================================================
# x.cor <- cor(mtcars, use="complete.obs", method="kendall")
# x.cov <- cov(mtcars, use="complete.obs") 
# 
# library(corrgram)
# corrgram(mtcars, order=NULL, 
#          lower.panel=panel.shade,
#          upper.panel=panel.pts, 
#          text.panel=panel.txt,
#          main="Car Milage Data (unsorted)")
# 
# library(corrplot)
# corrplot.mixed(x.cor, lower="circle", upper="number")
# 

# get all the regex lines

feats <- read.dcf()
