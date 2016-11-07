#' Sorting a dataset
#'
#' Takes in any data.frame and given input and assigns row weights. On the animals dataset, it is a bit pointless. I imagine this will be useful in finding a large homogeneous set of individuals which might be useful in finding a market to sell something to. For example, we select customers which bought "X" and the algorithm tries to find a large group with similar characteristics. The "weights" are a bit arbitrary at this moment. Say one column has 100 dogs and 50 cats, each dog row will receive 100 points. The rows which have the highest "sums" for each column overall will go towards the top. As such, there might be usefulness for this function with some critical thought and improvement.
#' @param dataFrame A dataframe
#' @param columnNumber Column with variable to be sorted
#' @param categoryName Name of variable in columnNumber
#' @param columnNumbersBeSorted Column numbers which hold data to be sorted
#' @examples
#' # This might take a moment to run.
#' forLoopAnimals <- sortit(animals, 4, "Euthanasia", c(5, 6, 7, 8, 9))
#' @export

sortit <- function(dataFrame = "Where data is stored", columnNumber = "Column number which holds category to sort by", categoryName = "Name of category to sort by", columnNumbersBeSorted = "Rows to be sorted, list as vector i.e., c(4,5,7,9)") {

  # Creates a dataframe based on variable of interest
  subsetOfDataFrame <- subset(dataFrame, dataFrame[,columnNumber] == as.character(categoryName))

  # Separate unneeded data from needed data i.e., data that is stored in columnNumbersToBeSorted
  # unsortedDataFrame will contain all the data we desire to sort

  unsortedDataFrame <- subsetOfDataFrame[,columnNumbersBeSorted]

  # The code below is not yet necessary, but might be here soon.
  residualDataFrame <- subsetOfDataFrame[,-columnNumbersBeSorted]

  # Make a simple counter and initialize a dataframe
  i <- 1
  frequencyTable <- NULL

  # Create a dataframe which holds number of each type of variable

  while (i <= length(columnNumbersBeSorted)) {

    frequencyTable <- rbind(as.data.frame(table(unsortedDataFrame[,i])), frequencyTable)


    i <- i + 1
  }

  # Initialize a weight column

  unsortedDataFrame$weight <- 0

  # Add weights from frequency table to weight column in unsortedDataFrame
  # Re-initialize counter

  j <- 1

  while (j < length(unsortedDataFrame[1,])) {

    i <- 1
    n <- 1

    while(i <= length(unsortedDataFrame[,1])) {


      if (unsortedDataFrame[i,j] == frequencyTable[n,1]) {
        unsortedDataFrame$weight[i] <- frequencyTable[n,2] + unsortedDataFrame$weight[i]
        i <- i + 1
        n <- 1
      }
      else if (unsortedDataFrame[i,j] != frequencyTable[n,1]) {
        n <- n + 1
      }

    }

    j <- j + 1
  }



  sorted <- unsortedDataFrame[order(-unsortedDataFrame$weight),]


}

#' Stock beta generator
#'
#'
#' @param stock A stock ticker,i.e, "AAPL"
#' @param index A stock index, i.e, "SPY"
#' @return beta for stock
#' @examples
#' d_beta("AAPL", "SPY")
#' @export
#'

d_beta <- function(Stock, Index) {
  if(is.element("quantmod", installed.packages()) && packageLoaded("quantmod")) {
    Stock <- getSymbols(Stock, src="google", env = NULL)
    Ind <- getSymbols(Index, src ="google", env = NULL)

    data_for_beta <- merge(Stock, Ind, by=0, all=TRUE)

    stock_close <- data_for_beta[,4]
    index_close <- data_for_beta[,9]

    data_for_beta <- merge(stock_close, index_close)
    data_for_beta <- as.data.frame(data_for_beta)
    data_for_beta <- data_for_beta[complete.cases(data_for_beta),]

    i <- 2
    stock_df <- NULL
    index_df <- NULL
    while(i+1 <= length(data_for_beta[,1])) {

      stock_df_add <- (data_for_beta[i,1]-data_for_beta[i-1,1])/data_for_beta[i-1,1]*100
      stock_df <- rbind(stock_df, stock_df_add)
      index_df_add <- (data_for_beta[i,2]-data_for_beta[i-1,2])/data_for_beta[i-1,2]*100
      index_df <- rbind(index_df, index_df_add)

      #index_df <- data_for_beta[i,2]-data_for_beta[i-1,2]

      i <- i+1
    }
    names(index_df) <- NULL
    names(stock_df) <- NULL
    list("beta" = cov(stock_df, index_df)/var(index_df))
  }
  else {
    print("You must install and load quantmod")
  }
}

#' Fitting any curve you like, really.
#'
#'
#' @param dataFrame A dataframe
#' @param columns Column with variable to be sorted
#' @return A least squares solution for the given curve
#' @examples
#'
#' coef <- curve_fitter(mtcars, "x", columns=c(4, 6))
#' plot(mtcars[[4]], mtcars[[6]])
#' curve(coef[1] + coef[2]*x, add = TRUE)
#'
#' coef <- curve_fitter(mtcars, "x, x^2", columns=c(1, 3))
#' plot(mtcars[[1]], mtcars[[3]])
#' curve(coef[1] + coef[2]*x + coef[3]*x^2, add = TRUE)
#'
#' # Everything below should run together.
#' x <- -10:10
#' y <- jitter(-(-10:10)*10 + (-10:10)^2 + (-10:10)^3, 500)
#' xy <- data.frame("x" = x, "y" = y)
#'
#' plot(x, y)
#' coeff <- curve_fitter(dataFrame = xy, "x, x^2, x^3", c(1, 2))
#' # coeff[[1]] is the constant value. Everything else corresponds to the order in which it was inputted.
#' curve(coeff[[1]] + coeff[[2]]*x + coeff[[3]]*x^2 + coeff[[4]]*x^3, add = TRUE)
#'
#' @export

curve_fitter <- function(dataFrame = "dataframe", expr="", columns = "c(x, y)") {
  x <- dataFrame[[columns[[1]]]]
  y <- dataFrame[[columns[[2]]]]
  expr <- unlist(strsplit(expr,","))
  nCol <- length(expr) + 1
  ex <- rep("rep(1,length(x))", nCol)
  ex[1:length(expr)] <- expr
  A <- sapply(1:length(ex), function(i) eval(parse(text=ex[i])))
  AtA <- t(A)%*%A
  B <- t(A) %*% y
  coeff <- solve(AtA)%*%B
  #coeff <- c(curve_fit[length(curve_fit),], curve_fit[1:length(curve_fit) - 1,])
  coeff <- c(coeff[length(coeff)], coeff[1:length(coeff) - 1])
  coeff
}


#' Downloads tables from url
#' @param url A url
#' @examples
#' starTable <- urlTables("https://en.wikipedia.org/wiki/Star")
#' # starTable[[3]]
#' @export
urlTables <- function(url,...) {
  if(is.element("rvest", installed.packages()) && packageLoaded("rvest")) {
    website <- read_html(url)
    website %>%
      html_table(...) -> list
    list
  }
  else
    print("You must install rvest and load it. Try install.packages('rvest') then library(rvest)")
}

#' Checks if a package is loaded
#' @param a package name
#' @examples
#' packageLoaded("ggplot2")
#' @export
packageLoaded <- function(name) 0 != length(grep(paste("^package:", name, "$", sep=""), search()))


#' Expands columns with categorical variables into ones and zeroes
#' @param dataFrame Where the data is stored
#' @param colNum Columns to be expanded
#' @examples
#' expandNum(animals, c(4,5,6,7))
#' @export

expandNum <- function(dataFrame = "A data.frame", colNum = "c(a,b,c,d...)",...) {
  # Splitting data into data to be
  oldDat <- dataFrame[-colNum]
  newDat <- dataFrame[colNum]
  uni <- list(NA)
  newCols <- list(NA)
  returnNewData <- data.frame(NA)
  for(i in 1:length(colnames(newDat))) {
    uni[[i]] <- as.vector(unique(newDat[[i]]))
  }

  for(i in 1:length(uni)) {

    for(n in 1:length(uni[[i]]))  {

      colTitle <- paste(uni[[i]][[n]])
      newCols[[colTitle]] <- ifelse(newDat[[i]]==colTitle, 1, 0)

    }

  }
  newCols <- newCols[-1]
  returnDataFrame <- cbind(oldDat, as.data.frame(newCols))
  returnDataFrame
}

#' Gets stock data that you might want
#' @param a stock symbol
#' @examples
#' AAPL <- yahooFin("AAPL")
#' @export


yahooFin <- function(stock) {

  tags <- c("a", "a2", "a5", "b", "b2", "b3", "b4", "b6", "c", "c1", "c3", "c6", "c8", "d", "d1", "d2", "e", "e1", "e7", "e8", "e9", "f6", "g", "h", "j", "k", "g1", "g3", "g4", "g5", "g6", "i", "i5", "j1", "j3", "j4", "j5", "j6", "k1", "k2", "k3", "k4", "k5", "l", "l1", "l2", "l3", "m", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "n", "n4", "o", "p", "p1", "p2", "p5", "p6", "q", "r", "r1", "r2", "r5", "r6", "r7", "s", "s1", "s7", "t1", "t6", "t7", "t8", "v", "v1", "v7", "w", "w1", "w4", "x", "y")

  returnList <- list(NA)
  id <- 1
  n <- 1
  exit <- 1

  id <- menu(c(
    "Ask",
    "Average Daily Volume",
    "Ask Size",
    "Bid",
    "Ask (Real Time)",
    "Bid (Read Time)",
    "Book Value",
    "Bid Size",
    "Change & Percent Change",
    "Change",
    "Comission",
    "Change (Real-time",
    "After Hours Change (Real-time)",
    "Dividend/Share",
    "Last Trade Date",
    "Trade Date",
    "Earnings/Share",
    "Error Indication (re`turned for symbol changed/invalid)",
    "EPS Estimate Current Year",
    "EPS Estimate Next Year",
    "EPS Estimate Next Quarter",
    "Float Shares",
    "Day's Low",
    "Day's High",
    "52-week Low",
    "52-week High",
    "Holdings Gain Percent",
    "Annualized Gain",
    "Holdings Gain",
    "Holdings Gain Percent (Real time)",
    "Holdings gain (Real-time)",
    "More Info",
    "Order Book (Real-time)",
    "Market Capitalization",
    "Market Cap (Real-time)",
    "EBITDA",
    "Change From 52-week Low",
    "Percent Change From 52-week low",
    "Last Trade (Real-time) with time",
    "Change Percent (Real-time)",
    "Last Trade Size",
    "Change From 52-week High",
    "Percent Change From 52-week High",
    "Last Trade (With Time)	",
    "Last Trade (Price Only)",
    "High Limit",
    "Low Limit",
    "Day's Range",
    "Day's Range (Real-time)",
    "50-day Moving Average",
    "200-day Moving Average",
    "Change from 200-day Moving Average",
    "Percent Change From 200-day Moving Average",
    "Change from 50-day Moving Average",
    "Percent Change From 50-day Moving Average",
    "Name",
    "Notes",
    "Open",
    "Previous Close",
    "Price Paid",
    "Change in Percent",
    "Price/Sales",
    "Price/Book",
    "Ex-Dividend Date",
    "P/E Ratio",
    "Dividend Pay Date",
    "P/E Ratio (Real-time)",
    "PEG Ratio",
    "Price/EPS Estimate Current Year",
    "Price/EPS Estimate Next Year",
    "Symbol",
    "Shares Owned",
    "Short Ratio",
    "Last Trade Time",
    "Trade Links",
    "Ticker Trend",
    "1 yr Target Price",
    "Volume",
    "Holdings Value",
    "Holdings Value (Real-time)",
    "52-week Range",
    "Day's Value Change",
    "Day's Value Change (Real-time)",
    "Stock Exchange",
    "Dividend Yield"
  ))
  exit <- id
  yah1 <- "http://finance.yahoo.com/d/quotes.csv?s="
  stock <- as.character(stock)
  yah2 <- "&f="
  id <- paste0(tags[id],collapse="")
  url <- paste0(yah1,stock,yah2,id,collapse="")
  returnList[n] <- read.table(url)
  n <- n + 1

  returnList
}


#' Converts a matrix to upper triangular form
#' @param A = matrix
#' @param roundedTo = integer to round to
#' @examples
#'  K <- matrix(c(1,4,5,3,5,
#'              6,5,4,3,2,
#'              6,7,8,9,0,
#'              4,3,2,5,6,
#'              6,6,7,8,8), nrow=5, byrow=TRUE)
#'
#' upperTriangular(K)
#' @export

upperTriangular <- function(A, roundedTo = 4)
{
  i <- 1
  j <- 1

  if (ncol(A) < nrow(A)) {
    iterations <- ncol(A)
  }

  if (ncol(A) >= nrow(A)) {
    iterations <- nrow(A)
  }



  while (j <= iterations) {
    if (A[i, j] == 0) {
      r_1 <- A[i, ]
      r_2 <- A[i + 1, ]
      A[i, ] <- r_2
      A[i + 1, ] <- r_1
      if (sum(A[, j]) == 0) {
        j <- j + 1
      }
    }
    A[i, ] <- A[i, ]/A[i, j]
    p <- i + 1
    while (p <= length(A[, 1])) {
      if (A[p, j] != 0) {
        A[p, ] = A[i, ] - A[p, ]/A[p, j]
        p <- p + 1
      }
      else p <- p + 1
    }
    j <- j + 1
    i <- i + 1
  }
  round(A, roundedTo)
}

#' Returns current T-Bill rates
#' @param none
#' @examples
#' tRates <- tBillRates()
#' @export
#'

tBillRates <- function() {
  tBillUrl <- 'https://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=billrates'
  tbill <- fdRen::urlTables(tBillUrl, fill = TRUE)
  tbill[[69]]
}

#' TicTacToe
#' @param none
#' @examples
#' tictactoe()
#' @export
#'

tictactoe <- function() {
theBoard <- data.frame('left' = c(NA, NA, NA), 'middle'= c(NA, NA, NA), 'right'= c(NA, NA, NA), row.names = c('top', 'middle', 'bottom'))

printBoard <- function(board) {

  cat('',theBoard[1,1], "|", theBoard[1,2], "|", theBoard[1,3],'\n----+----+---\n',
      theBoard[2,1], "|", theBoard[2,2], "|", theBoard[2,3],'\n----+----+---\n',
      theBoard[3,1], "|", theBoard[3,2], "|", theBoard[3,3]
  )
}

turn = 'X'

for(i in 1:10) {
  printBoard(theBoard)
  rown <- as.numeric(readline("Enter Row"))
  coln <- as.numeric(readline("Enter column"))
  theBoard[rown, coln] = turn
  if (turn == 'X') {
    turn <- 'O'
  }
  else
    turn = 'X'
}

}

#' Lehmer
#' @param n = number of rows/columns
#' @examples
#' lehmer(10)
#' @export
#'

lehmer <- function(n = "number of columns and rows, nXn matrix",
				   roundTo = "rounding of matrix if desired") {
	theVector <- rep(NA, n*n)
	theMatrix <- matrix(theVector, nrow = n, ncol = n)

	for(j in 1:n) {
		for(i in 1:n) {
		if (j >= i) {
			theMatrix[i,j] <- i/j
		}
		else
			theMatrix[i,j] <- j/i
		}
	}

	return(theMatrix)
}

lehmer(n = 6)

#' stockQuoteBig
#' @param stock = stockTicker
#' @examples
#'
#' aapl <- stockQuoteBig("AAPL", "AKRX")
#' aapl$byDay
#' aapl$byMonth
#' aapl$byYear$Date
#'
#' # Correlation between sh (S&P inverse) and S&P ETF
#' sh  <- stockQuoteBig("SH", 2010)
#' spy  <- stockQuoteBig("SPY", 2010)
#' cor(spy$byMonth$Percent.Change[1:80], sh$byMonth$Percent.Change[1:80])
#' @export
#'
#'

stockQuoteBig <- function(stockName = 'Ex. "AAPL" ', year) {

  returnIt <- list(NA)
  if(missing(year)) {
  year <- 1000
}
  else
    year <- year - 1 # adjusting for yahoo

returnList <- list(NA)

for(i in 1:length(stockName)) {

url <- paste0("http://chart.finance.yahoo.com/table.csv?s=", stockName[i], "&a=11&b=12&c=", year, "&d=8&e=3&f=2016&g=d&ignore=.csv")

# Day to Day information
byDay <- read.csv(url)
byDay$Date <- as.Date(byDay$Date)
byDay <- subset(byDay, Date > as.Date(paste0(year,"-01-01")))
byDay$Date <- as.Date(byDay$Date)

# Week to Week information
library(lubridate)
byWeek <- subset(byDay, wday(byDay$Date) == 2)
rownames(byWeek) <- NULL

# Month to Month information
byMonth <- data.frame("Date" = as.Date(firstDayMonth(byDay$Date)))
byMonth <- merge(byDay, byMonth)
byMonth <- byMonth[order(byMonth$Date,decreasing = TRUE),]
rownames(byMonth) <- NULL

# Year to year Information
# Month to Month information
byYear <- data.frame("Date" = as.Date(firstDayYear(byDay$Date)))
byYear <- merge(byYear, byDay)
byYear <- byYear[order(byYear$Date,decreasing = TRUE),]
rownames(byYear) <- NULL


# Edit of Day to Day Information
byDay$Percent.Change <- (byDay$Adj.Close - shift(byDay$Adj.Close, 1))/shift(byDay$Adj.Close,1)*100

# Edit of Month to Month Information
byMonth$Percent.Change <- (byMonth$Adj.Close - shift(byMonth$Adj.Close, 1))/shift(byMonth$Adj.Close,1)*100

# Edit of Year to Year Information
byYear$Percent.Change <- (byYear$Adj.Close - shift(byYear$Adj.Close, 1))/shift(byYear$Adj.Close,1)*100

byWeek$Percent.Change <- (byWeek$Adj.Close - shift(byWeek$Adj.Close, 1))/shift(byWeek$Adj.Close,1)*100



returnList$byDay <- byDay
returnList$byMonth <- byMonth
returnList$byYear <- byYear
returnList$byWeek <- byWeek

returnIt[[stockName[i]]] <- returnList

}
returnIt <- returnIt[-1]
return(returnIt)
}


#' coinPlot
#' @param n = number of obs
#' @examples
#' coinPlot(1000)
#' @source swirl.com
#' @author swirl.com
#' @export
#'
#'

coinPlot <- function(n){
  library(ggplot2)
  means <- cumsum(sample(0 : 1, n , replace = TRUE)) / (1  : n)
  g <- ggplot(data.frame(x = 1 : n, y = means), aes(x = x, y = y))
  g <- g + geom_hline(size=1.5 ,yintercept = 0.5,alpha=.6,
  linetype="longdash") + geom_line(size = 1)
  if(n<100){
      g <- g + geom_point(colour="red",size=3,alpha=0.8)
  }
  g <- g + labs(x = "Number of observations", y = "Cumulative mean")
  g <- g + scale_x_continuous(breaks=seq(0,n+1,ceiling(n/10)))
  print(g)
  invisible()
}
coinPlot(10)

#' binomConfint
#' @param
#' p_prime = "sample proportion"
#' n = "number of observations"
#' conf = confidence interval
#' @examples
#' binomConfint(.6, 100, .95)
#' @source swirl.com
#' @author swirl.com
#' @export
#'

binomConfint <- function(p_prime = "sample proportion", n = "number of observations", conf = ".9, .95, .99... etc") {
	p_prime1 <- p_prime + c(-1, 1)*qnorm(1-(1-conf)/2)*sqrt((p_prime)*(1-p_prime)/n)
	return(p_prime)
}

#' tTestDifference
#' @examples
#' diffTInv(g1, g2, .95)
#' @seealso t.test(difference)
#' @export

tTestDifference <- function(group1, group2, confidence) {
  difference <- group2 - group1
  mdiff <- mean(difference)
  sigma <- sd(difference)
  mdiff +c(-1, 1)*qt(1-(1-confidence)/2, length(difference)-1)*sigma/sqrt(length(difference))
  }

#' tTest
#' @param
#' sample = sampled data
#' confidence = confidence of choice, usually .95
#' stDev = standard deviaion (ONLY IF KNOWN)
#' @examples
#' students <- rnorm(10000,mean = 70000, sd = 15000)
#' sampleStud <- sample(students,size = 60,replace = FALSE)
#' tTest(sampleStud, .95)
#' @seealso t.test(sampleStud)
#' @export

tTest <- function(sample, confidence, stDev) {
	returnData <- list()
	if(missing(stDev)) {
	  tStar <- qt(1-(1-confidence)/2, length(sample)-1)
	  sigma <- sd(sample)
	}
	else {
	   tStar <- qnorm(1-(1-confidence)/2)
	   sigma <- stDev
	  }
	sampleMean <- mean(sample)
	n <- length(sample)
	interval <- sampleMean + c(-1, 1)*tStar*sigma/sqrt(n)
	returnData$interval <- interval
	returnData$hist <- hist(sample)
	return(returnData)
}

#' tStatistic
#' @param
#' sample = sampled data
#' nullHyp = null hypothesis mean
#' stDev = standard deviation (ONLY IF KNOWN)
#' @examples
#' @export

tStatistic <- function(sample, nullHyp, stDev) {
  if(missing(stDev)) {
    sigma <- sd(sample)
  }
  else {
    sigma <- stDev
    }
	returnData <- list()
	sampleMean <- mean(sample)
	n <- length(sample)
	tStat <- (sampleMean - nullHyp)/(sigma/sqrt(n))
	returnData$tstat <- tStat
	returnData$n <- n
	returnData$stdev <-  sigma
	return(returnData)
}


#' shift
#' @param
#' x = list of numbers
#' shift_by = number to shift by
#' @examples
#' shift(1:10, 1)
#' @export

## Shift Function
shift<-function(x,shift_by){
   stopifnot(is.numeric(shift_by))
   stopifnot(is.numeric(x))

  if (length(shift_by)>1)
      return(sapply(shift_by,shift, x=x))

  out<-NULL
  abs_shift_by=abs(shift_by)
  if (shift_by > 0 )
      out<-c(tail(x,-abs_shift_by),rep(NA,abs_shift_by))
  else if (shift_by < 0 )
      out<-c(rep(NA,abs_shift_by), head(x,-abs_shift_by))
  else
      out<-x
  out
}

#' firstDayYear
#' @description
#' Returns the first date of each year in list of dates
#' @param
#' x = list of dates
#' @examples
#'
#' @export

firstDayYear=function(x)
{

     x=as.Date(as.character(x))
     day = format(x,format="%d")
     monthYr = format(x,format="%Y-%m")
     y = tapply(day,monthYr, min)
     first=as.Date(paste(row.names(y),y,sep="-"))
     first = first[grepl("[[:digit:]]+-01-[[:digit:]]+",first)]
     return(as.factor(first))
}

#' firstDayMonth
#' @description
#' Returns the first date of each month in list of dates
#' @param
#' x = list of dates
#' @examples
#'
#' @export

firstDayMonth=function(x)
{
     x=as.Date(as.character(x))
     day = format(x,format="%d")
     monthYr = format(x,format="%Y-%m")
     y = tapply(day,monthYr, min)
     first=as.Date(paste(row.names(y),y,sep="-"))
     as.factor(first)
}


#' meanplot
#' @description
#' plots the average of points based on distance from one another given by sens
#' @param
#' dataset = a dataset
#' columns = the two columns to be plotted
#' sens = integer
#' @examples
#' Everything below should run together.
#' x <- -10:10
#' y <- jitter(-(-10:10)*10 + (-10:10)^2 + (-10:10)^3, 500)
#' xy <- data.frame("x" = x, "y" = y)
#' meanplot(xy, c(1,2), sens = 1)
#' for(i in 1:3) {
#' meanplot(xy, c(1,2), sens = i)
#' }
#' @export

meanplot <- function(dataset, columns, sens) {
  dataset$meanx <- 0
  dataset$meany <- 0

  x <- columns[1]
  y <- columns[2]

  i <- 1
  j <- sens

  while(i <= length(dataset[[x]])) {
    dataset$meanx[i] <- mean(dataset[i:j, x], na.rm = TRUE)
    dataset$meany[i] <- mean(dataset[i:j, y], na.rm = TRUE)
    i <- i + 1
    j <- j + 1
  }

  plot(dataset[[x]], dataset[[y]])
  points(dataset$meanx, dataset$meany, type = "l")
}

#' bondtable
#' @description
#' returns current treasury bills and notes
#' @examples
#' bondtable()
#' @export

bondTable <- function() {

bondTable <- fdRen::urlTables("http://online.wsj.com/mdc/public/page/2_3020-treasury.html")
tipsTable <- fdRen::urlTables("http://www.wsj.com/mdc/public/page/2_3020-tips.html")
treasury <- list(NA)


# Sep t- Notes
treasuryNotes <- bondTable[[3]]
colnames(treasuryNotes) <- treasuryNotes[1,]
treasuryNotes <- treasuryNotes[-1,]

# Sep t-bills
treasuryBills <- bondTable[[4]]
colnames(treasuryBills) <- treasuryBills[1,]
treasuryBills <- treasuryBills[-1,]

# Sep t- Notes
tips <- tipsTable[[3]]
colnames(tips) <- tips[1,]
tips <- tips[-1,]
tips <- tips[,-5]
tips$Maturity <- gsub(" ", "-", tips$Maturity)
tips$Maturity <- as.Date(tips$Maturity, "%Y-%b-%d")
tips$Maturity <- as.Date(tips$Maturity, "%m/%d/%Y")
tips$Coupon <- as.numeric(tips$Coupon)
tips$Bid <- as.numeric(tips$Bid)
tips$Asked <- as.numeric(tips$Asked)
tips$Yield <- suppressWarnings(as.numeric(tips$Yield))
tips$Accruedprincipal <- as.numeric(tips$Accruedprincipal)

treasuryNotes$Maturity <- as.Date(treasuryNotes$Maturity, "%m/%d/%Y")
treasuryNotes$Coupon <- as.numeric(treasuryNotes$Coupon)
treasuryNotes$Bid <- as.numeric(treasuryNotes$Bid)
treasuryNotes$Asked <- as.numeric(treasuryNotes$Asked)
treasuryNotes$Chg <- suppressWarnings(as.numeric(treasuryNotes$Chg))
treasuryNotes$Askedyield <- as.numeric(treasuryNotes$Askedyield)

treasuryBills$Maturity <- as.Date(treasuryBills$Maturity, "%m/%d/%Y")
treasuryBills$Bid <- as.numeric(treasuryBills$Bid)
treasuryBills$Asked <- as.numeric(treasuryBills$Asked)
treasuryBills$Chg <- suppressWarnings(as.numeric(treasuryBills$Chg))
treasuryBills$Askedyield <- as.numeric(treasuryBills$Askedyield)

treasury$Bills <- treasuryBills[complete.cases(treasuryBills),]
treasury$NotesBonds <- treasuryNotes[complete.cases(treasuryNotes),]
treasury$tips <- tips[complete.cases(tips),]
treasury <- treasury[-1]
treasury
}

#' completest.cases
#' @description
#' returns rows without NAs
#' @examples
#' # Remove na from stockQuoteBig
#' stocks <- stockQuoteBig(c("AAPL"), 2012)
#' aaplNoNA <- completest.cases(stocks$AAPL$byDay)
#' @export
#'

completest.cases <- function(theDataframe) {
  theDataframe <- theDataframe[complete.cases(theDataframe), ]
  }

#' stckData
#' @description
#' Returns data about selected stocks
#' @examples
#' stock <- stockData(c("AAPL", "AKRX", "SPY"), 2009)
#' @export


stockData <- function (stockVector, from, byDay = FALSE, byYear = FALSE, byMonth = FALSE)
{
  stocks <- stockQuoteBig(stockVector, from)
  returnList <- list()
  if (byDay == TRUE) {
    for (i in 1:length(stockVector)) {
      returnList[[as.character(names(stocks)[i])]] <- stocks[[i]][[2]]
    }
  }
  else if (byYear == TRUE) {
    for (i in 1:length(stockVector)) {
      returnList[[as.character(names(stocks)[i])]] <- stocks[[i]][[4]]
    }
  }
  else if (byMonth == TRUE) {
    for (i in 1:length(stockVector)) {
      returnList[[as.character(names(stocks)[i])]] <- stocks[[i]][[3]]
    }
  }
  else for (i in 1:length(stockVector)) {
    returnList[[as.character(names(stocks)[i])]] <- stocks[[i]][[5]]
  }

  oldestDate <- vector()



  oldestDate <- vector()
  maxMin <- min(returnList[[1]][,1])
  for(i in 1:length(stockVector)) {
    maxMin <- max(min(returnList[[i]][,1]), maxMin)
  }


  for(i in 1:length(stockVector)) {
    returnList[[as.character(names(stocks)[i])]] <- subset(returnList[[i]], returnList[[i]][[1]] > maxMin)
  }



  estimated_return <- NA
  std_deviation <- NA
  for (i in 1:length(stockVector)) {
    estimated_return[i] <- mean(returnList[[i]][[8]]/100,
                                na.rm = TRUE)
    std_deviation[i] <- sd(returnList[[i]][[8]]/100, na.rm = TRUE)
  }
  names(estimated_return) <- stockVector
  names(std_deviation) <- stockVector
  percent_change <- data.frame(init = 1:length(returnList[[i]][[8]]))
  for (i in 1:length(stockVector)) {
    percent_change <- cbind(percent_change, returnList[[i]][[8]])
  }
  percent_change <- percent_change[, -1]/100
  if (length(stockVector) > 1) {
    colnames(percent_change) <- stockVector
    rownames(percent_change) <- as.character(returnList[[1]][[1]])
    percent_change <- completest.cases(percent_change)
    cor_change <- cor(percent_change)
    cov_change <- cov(percent_change)
    returnList[["correlation_matrix"]] <- cor_change
    returnList[["covariance_matrix"]] <- cov_change
  }
  else percent_change <- percent_change[!is.na(percent_change)]
  returnList[["estimated_return"]] <- estimated_return
  returnList[["std_deviation"]] <- std_deviation
  returnList[["variance"]] <- std_deviation^2
  returnList[["percent_change"]] <- percent_change
  returnList
}


#' graphPort
#' @description
#' Plots a portfolio of three stocks, must use stockData function
#' @examples
#' stocks <- stockData(c("AAPL", "AKRX", "SPY"), 2009)
#' graphPort(stock = stocks, rf = .005)
#' @export

graphPort <- function(stock, rf) {
if(missing(rf)) {
  rf <- .01
}
weight1 <- runif(n = 1000, min = 0, max = 1)
weight2 <- runif(n = 1000, min = 0, max = 1)
weight3 <- 1 - weight1 - weight2

weight1 + weight2 + weight3


portvar <- weight1^2*stock$covariance_matrix[1,1] + weight2^2*stock$covariance_matrix[2,2] + weight3^2*stock$covariance_matrix[3,3] + 2*weight1*weight2*stock$covariance_matrix[1,2] + 2*weight2*weight3*stock$covariance_matrix[2,3] +
  2*weight1*weight2*stock$covariance_matrix[1,3]

stdev <- sqrt(portvar)
erp <- weight1*stock$estimated_return[1] + weight2*stock$estimated_return[2] +
        weight3*stock$estimated_return[3]
plot(stdev, erp, xlim = c(0, max(stdev)), ylim = c(min(erp),max(erp)), xlab = "Standard Deviation", ylab= "Estmated Return", main = paste(as.character(names(stock)[1:3])),sub = paste("Risk Free Rate = ", rf))

curve(rf+x*max((erp-rf)/stdev), add = TRUE)
}


