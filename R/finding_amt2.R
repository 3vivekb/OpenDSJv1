mayor2014 <- read.csv("/Users/Lynna/Dropbox/opendisclosure/efile_CSJ_2014 2.csv")
mayor2014 <- read.csv("/Users/Vivek/Dropbox/opendisclosure/efile_CSJ_2014 2.csv")


str(mayor2014)
names(mayor2014)
## taking only the fields that we need
mayor2014 <- mayor2014[,c("Filer_ID", "Filer_NamL", "Tran_Date", "Tran_Amt1", "Tran_Amt2")]
## summary of "Amt1" by major
# We don't want Amt2.
aggregate(Tran_Amt1 ~ Filer_NamL, data=mayor2014, FUN=sum)


## write a function for this?
## input: csvfile, Filer_ID
## output: Filer_ID and Tran_Amt1
totalamt1 <- function(nameData, Filer_ID){
	data <- read.csv(nameData, header = T)
	data <- data[,c(1, 2, 30, 31)]	## Taking only the Filer_ID, Filer_NamL, and Amt1 Amt2
	sumTranAmtByFilerID <- aggregate(Tran_Amt1 ~ Filer_ID, data=data, FUN=sum)
	sumTranAmtByFilerID[(sumTranAmtByFilerID$Filer_ID == Filer_ID), c(1,2)]
}

totalamt2("/Users/Lynna/Dropbox/opendisclosure/efile_CSJ_2014 2.csv", 1359560)
## ex:1359560


## include Tran_Date info

mayor2014$Tran_Date <- as.Date(as.character(mayor2014$Tran_Date), "%m/%d/%Y")
str(mayor2014$Tran_Date) ## checking

totalamt2 <- function(nameData, Filer_ID){
	data <- read.csv(nameData, header = T)
	data <- data[,c(1, 2, 28, 31)]	## Taking only the Filer_ID, Filer_NamL, and Amt1 Amt2
	sumTranAmtByFilerID <- aggregate(Tran_Amt1 ~ Filer_ID + Tran_Date, data=data, FUN=sum)
    sumTranAmtByFilerID <- sumTranAmtByFilerID[(sumTranAmtByFilerID$Filer_ID == Filer_ID), c(1,2, 3)]
    within(sumTranAmtByFilerID, {
        cumsumAmt <- ave(Tran_Amt1, FUN = cumsum)
    })
}

## ex: 1362278
