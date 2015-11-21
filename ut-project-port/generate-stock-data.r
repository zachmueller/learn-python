# Create stock price data series (use S&P500 over X previous years as baseline for market fluctuations, give stock beta of 1.X, maybe contact Sweet for help on how to simulate a stock's price movement this way)
# --->vector of S&P500 % changes, duplicate into new vector and multiple % changes by chosen Beta, randomly miss that Beta-adjusted vector by roughly +/-7bps, choose starting price then apply % change vector
# Create list of employees (with first/last names, department, title, level, hire date, etc.) [total employee count should grow over time, include term dates]
# Create vector of grant dates (options usually granted across the company in bulk)
# Create data.table of granted options, issuing only to employees hired and not terminated by that date, and issuing number of options based on employee level

set.seed(10123);

# load necessary libraries
library("data.table");
library("lubridate");
library("ggplot2");
library("truncnorm");
library("zoo");
library("timeDate");

library("RODBC");
library("xlsx");
library("quantmod");



#####################################################################
##	Create stock price using input market data (S&P500)
#####################################################################
# set input variables
start.price <- 10.0;
beta <- 1.2;
beta.miss <- 0.0007;
start.date <- as.Date("1990-01-01");
months.pricing.data <- 25 * 12;

start.headcount <- 10;
end.headcount <- 2000;

initial.stock.options <- 1e7; # number of shares for founding team
founder.ownership.percent <- 0.5;
initial.shares.outstanding <- initial.stock.options / founder.ownership.percent;
# the above is mainly for mechanics, need to sync with Stuart to understand
#	the best wording for it to be accurate for reality


# import market csv file
setwd('C:/Users/zmueller/Documents/GitHub/sql-project/');
file.name <- 'data/stock-market/S&P500.csv';
# load csv into data.table
market <- data.table(read.csv(file.name));
# convert Date field to date type
market[, date:=as.Date(date, format = "%m/%d/%Y")];
# set the key to order the data.table data
setkey(market, date);
# remove the extraneous columns
market[, Open:=NULL];
market[, High:=NULL];
market[, Low:=NULL];
market[, Close:=NULL];
market[, Volume:=NULL];
# add in prior date's price and % change day over day
market[, Prior.Close:=c(0, Adj.Close[-.N])];
market[, Pct.Change:=((Adj.Close - Prior.Close)/Prior.Close)];

# extract relevant date range from market data
date.range <- seq(from = start.date, to = (start.date +
	months(months.pricing.data) - 1), by = "1 day");
stock <- market[J(date.range), nomatch=FALSE]; # use nomatch to filter to only dates with market data
# remove extra columns
stock[, Adj.Close:=NULL];
stock[, Prior.Close:=NULL];


# use stock market price data and input Beta to back into a stock price
##	multiply the market % change by the input Beta,
##	using a random factor of "missing" a perfect Beta
stock[, Pct.Change.B:=Pct.Change * beta 
	+ rnorm(stock[,.N], mean = 0, sd = beta.miss)];
# add in starting stock price
##	set randomly around start.price by roughly 5%
stock[, price:=round(rnorm(1, mean = start.price, sd = 0.05),2)];
# calculate the cumulative product of the Beta-adjusted percent changes
stock[, Pct.Change.CumProd:=cumprod(1 + Pct.Change.B)];
# multiply the cumulative product output by the starting stock price
stock[, price:=round(Pct.Change.CumProd * price,2)];
# show a quick plot of the data
qplot(stock[,date], stock[,price]);
# remove extra columns
stock[, pct.change:=Pct.Change.B];
stock[, Pct.Change:=NULL];
stock[, Pct.Change.B:=NULL];
stock[, Pct.Change.CumProd:=NULL];



#####################################################################
##	Create fictitious list of employees, using First and Last name lists
#####################################################################
# import first and last name lists from csv files
setwd('C:/Users/zmueller/Documents/GitHub/sql-project/');
file.name <- 'data/lists/CSV_Database_of_First_Names.csv';
first.names <- data.table(read.csv(file.name));
file.name <- 'data/lists/CSV_Database_of_Last_Names.csv';
last.names <- data.table(read.csv(file.name));


# add some randomization to the inputs
company.start.date <- min(stock[,date]) # - years(5);
start.headcount <- round(rnorm(1, mean = start.headcount, sd = (start.headcount * 0.15)));
end.headcount <- round(rnorm(1, mean = end.headcount, sd = (end.headcount * 0.05)));
company.start.date <- company.start.date + round(rnorm(1, mean = 0, sd = 60), 0);


# create data.table of employees, randomly selecting from both first and last names
employees <- data.table(first.name = sample(first.names[, firstname],
	end.headcount, replace=TRUE), last.name = sample(last.names[, lastname],
	end.headcount, replace=TRUE));
rm(list = c("first.names", "last.names"));

# choose a random set (around years * 8) of dates between company.start.date and
##	the end date as batch hiring days.
end.date <- max(stock[, date]);
hire.days <- round(rnorm(1, mean = (as.integer(end.date
	- company.start.date) / 365) * 8, sd = 10), 0);
hiring.dates <- sample(stock[,date], hire.days);
hiring.dates <- hiring.dates[order(hiring.dates)];
hiring <- data.table(date = hiring.dates);
setkey(hiring, date);
hiring[, I:=.I];
# hiring[stock, roll=TRUE][, sum(price), by = I];

##	Then, using the stock price data as a proxy, fill in each 
##	employee's hiring date as one of the hiring dates randomly selected above
####	Hire count based on area under curve (AUC) between hire dates as percent of total AUC?
####	Approximate AUC calc with day's stock price (assume each closing price is equal to
####	only lasting effectively one day (rather than include weekends in x-axis for calc)
# use cumulative percent of AUC for easy join to hiring.dates
stock[, AUC.pct:=cumsum(price)/sum(price)];
#hiring.prob <- stock[J(hiring.dates), AUC.pct][["AUC.pct"]];
hiring.prob <- stock[J(hiring.dates),][,AUC.pct];
hiring.prob <- diff(c(0, hiring.prob), lag = 1);
hiring.prob[1] <- 1 - sum(hiring.prob) + hiring.prob[1];

# using the calculated sampling percentages and hiring dates
##	create and fill in new field hire.date for all employees
dates <- sample(hiring.dates, employees[,.N], prob = hiring.prob, replace = TRUE);
employees[, hire.date:=dates];


# Terminations - use truncnorm to limit tenure between 1 year to 5 years
##	for employees who leave. Only a small percentage of employees are
##	assumed to be terminated or leave.
# fill employees data.table with termination dates
term.prob <- 0.1;
term.flags <- sample(c(TRUE, FALSE), employees[,.N], 
	prob = c(term.prob, 1 - term.prob), replace = TRUE);
days.employed <- rtruncnorm(employees[,.N], a = 90, b = 365*5,
	mean = 365*2, sd = 180);
employees[, tenure.days:=round(term.flags * days.employed, 0)];
employees[, term.date:=as.Date(ifelse(tenure.days,
	hire.date + days(tenure.days), NA))];

# remove any termination dates that are after the last hiring date
last.hire.date <- max(hiring.dates);
employees[ term.date >= last.hire.date, term.date:=NA];
employees[, tenure.days:=NULL];


# Set "founding" members as first 10 employees
setkeyv(employees, "hire.date");
employees[, i:=.I];
employees[ i <= 10, hire.date:=company.start.date];
employees[, i:=NULL];
# reset key (changing the key values unsets it)
setkeyv(employees, "hire.date");
# create employee id field
employees[, employee.id := .I];


# import departments table
file.name <- 'data/lists/departments.txt';
departments <- data.table(read.table(file.name, header = TRUE, sep = "\t"));
setkeyv(departments, "department.code");

# randomly assign employees to a department, using the input probabilities
employees[, department.code := sample(departments[,department.code], employees[, .N],
	prob = departments[, department.probability], replace = TRUE)];
# useful: merge(employees, departments, by = "department.code");

# add in pay.grade as field to employees data.table for easier calcs
setkeyv(employees, "department.code");
employees[departments, pay.grade := pay.grade, allow.cartesian = TRUE];
setkeyv(employees, "hire.date");



# distribute initial stock options based on pay.grade of founding members
founder.stock.options <- employees[J(company.start.date), list(employee.id, pay.grade)];
founder.stock.options[, options.granted := ceiling((initial.stock.options
	* pay.grade / sum(pay.grade))/1e5) * 1e5];
# assume grant strike price is 70% of 1st day closing price
founder.stock.options[, price := round(0.7 * stock[, price][1],2)];
founder.stock.options[, date := company.start.date];


# extend stock price data to include all dates
#	--> carry forward previous date's stock price
stock.raw <- stock;
# help from: http://stackoverflow.com/a/22957596
all.dates <- data.table(date = seq.Date(min(stock[, date]), max(stock[, date]), by = "day"));
stock <- merge(stock, all.dates, by="date", all = TRUE);
stock[, price := na.locf(stock[,price])];



# Option transaction logic
####	HIRED GRANTS
##	Issue new stock options once each employee is hired
hire.stock.options <- merge(employees[, list(date = hire.date, employee.id, pay.grade)
	], stock[, list(date, price)], by = "date");
hire.stock.options[, options.granted := ceiling(
	((pay.grade * 1000)/price) / 10) * 10];

####	QUARTERLY GRANTS
##	Issue new stock options once each quarter for employees hired but
##	not yet fired on that date, with option count based on pay.grade
##	1. Create vector with grant dates (once a quarter)
# help from: http://stackoverflow.com/a/23354174
tS <- timeSequence(from = company.start.date, to = max(stock[, date]), by = "quarter");
grant.dates <- as.Date(timeNthNdayInMonth(tS, nday = 5, nth = 3, format = "%Y-%m-%d"));

##	2. Create data.table with combined length of grant dates * total employees
# add new field for easier calcs
employees[, term.date.calc := term.date];
employees[is.na(term.date.calc), term.date.calc := as.Date(now())];
setkeyv(employees, "employee.id");
quarterly.grants <- data.table(date = rep(grant.dates, times = 
	employees[, .N]), employee.id = employees[, employee.id], hire.date = employees[, hire.date],
	term.date = employees[, term.date.calc], pay.grade = employees[, pay.grade]);

##	3. Fill new data.table with options granted count based on dates/pay.grade
setkeyv(quarterly.grants, "date");
#quarterly.grants[stock, price := price];
quarterly.grants[stock, price := price, allow.cartesian = TRUE];
quarterly.grants[, options.granted := (hire.date <= date) *
	(term.date >= date) * ceiling(((pay.grade * 200)/price) / 10) * 10];

##	4. Remove any records with 0 options granted
quarterly.grants <- quarterly.grants[options.granted > 0];

####	COMPILE FULL GRANT LIST
##	1. Combine all grants above into one full list, keeping key data points
grants <- data.table(
	employee.id = c(founder.stock.options[,employee.id], hire.stock.options[,employee.id],
		quarterly.grants[,employee.id]),
	grant.date = c(founder.stock.options[,date], hire.stock.options[,date],
		quarterly.grants[,date]),
	strike.price = c(founder.stock.options[,price], hire.stock.options[,price],
		quarterly.grants[,price]),
	count.granted = c(founder.stock.options[,options.granted], hire.stock.options[,options.granted],
		quarterly.grants[,options.granted]),
	grant.type = c(rep('Founder Stock Options', times = founder.stock.options[,.N]),
		rep('Hire Stock Options', times = hire.stock.options[,.N]),
		rep('Quarterly Stock Options', times = quarterly.grants[,.N]))
	# , pay.grade = c(founder.stock.options[,pay.grade], hire.stock.options[,pay.grade], quarterly.grants[,pay.grade])
);
# row number becomes the grant id
grants[, grant.id := .I];
# set key on new id field
setkeyv(grants, "grant.id");

####	VESTING
# create data.table with vesting dates for all grants
vesting.schedule <- data.table(
	grant.id = grants[, grant.id],
	employee.id = grants[, employee.id],
	grant.date = grants[, grant.date],
	strike.price = grants[, strike.price],
	# base number of options divided by 4
	count.granted = grants[, count.granted / 4.0],
	# vesting dates are grant.date + 1:4 years
	vest.date = c(grants[, grant.date] + years(1),
		grants[, grant.date] + years(2),
		grants[, grant.date] + years(3),
		grants[, grant.date] + years(4))
);
setkeyv(vesting.schedule, "grant.id");

####	EXERCISES
##	1. Number of transactions per employee based on tenure (with
##		normal distribution around once per quarter [90 days])
employee.trx.count <- data.table(
	employee.id = employees[, employee.id],
	tenure.days = employees[, term.date.calc - hire.date]
);
option.trx <- ceiling(rnorm(employee.trx.count[,.N],
	mean = employee.trx.count[, tenure.days] / 90,
	sd = 5));
setkeyv(employee.trx.count, "employee.id");
employee.trx.count[, trx.count := option.trx];
max.trx.count <- max(employee.trx.count[, trx.count]);

##	2. Fully expanded data table of employees * max.trx.count
trx <- data.table(
	employee.id = rep(employee.trx.count[, employee.id], each = max.trx.count),
	trx.number = 1:max.trx.count
);
setkeyv(trx, "employee.id");
# shrink trx data table down based on total number of trx per employee
trx <- trx[employee.trx.count][trx.number <= trx.count, list(employee.id, trx.number)];
setkeyv(trx, "employee.id");

##	3. Add in fields for hire.date and days spread for each potential transaction
trx[employees, hire.date := hire.date, allow.cartesian = TRUE];
trx.days <- ceiling(rnorm(trx[,.N], mean = 90, sd = 5));
trx[, days.to.next.trx := trx.days];
# calculate cumulative days for each employee/trx.number
trx[, cumulative.days := cumsum(days.to.next.trx), by = employee.id];
# create field with actual trx date
trx[, trx.date := hire.date + days(cumulative.days)];
# remove any transactions that occur after the last available stock price data
trx <- trx[trx.date <= end.date,];
setkeyv(trx, "employee.id");
# remove any transactions that occur after employees' term.date
trx <- merge(trx, employees[, list(employee.id, term.date.calc)], 
	by = "employee.id")[trx.date < term.date.calc, 
		list(employee.id, trx.number, hire.date,
		days.to.next.trx, cumulative.days, trx.date)];
setkeyv(trx, "employee.id");


##	4. Sum up the cumulative options granted for each employee at each trx.date
# create data table of each employee's cumulative grant count at each vesting date
setkeyv(vesting.schedule, c("employee.id","vest.date"));
cumulative.vested <- vesting.schedule[, list(sum(count.granted)
	,max(strike.price)),by = c("employee.id", "vest.date")][,
	c("cumulative.grants", "strike.price") := list(cumsum(V1), V2)
	,by = employee.id][,c("V1","V2") := list(NULL, NULL)];
setkeyv(cumulative.vested, "employee.id");
setnames(cumulative.vested, "vest.date", "date");
# remove any records after the relevant time period
cumulative.vested <- cumulative.vested[ date <= end.date, ];

# build out temp data table with all employee IDs and date combos
temp <- data.table(employee.id = rep(employees[, employee.id], times = all.dates[, .N]),
	date = rep(all.dates[, date], each = employees[, .N])
);
# fill out data table with all dates and pull forward the cumulative grant count
setkeyv(temp, c("employee.id", "date"));
setkeyv(cumulative.vested, c("employee.id", "date"));
cumulative.vested <- merge(cumulative.vested, temp, by = c("employee.id", "date"), all = TRUE);
# rm(temp);
setkeyv(cumulative.vested, c("employee.id", "date"));
# set grant count to 0 for the first date for each employee
cumulative.vested[date == min(cumulative.vested[, date]) & is.na(cumulative.grants),
	`:=`(cumulative.grants = 0, strike.price = 0)];
cumulative.vested[, `:=`(cumulative.grants = na.locf(cumulative.vested[,cumulative.grants]),
	strike.price = na.locf(cumulative.vested[,strike.price]))];


##	5. Using a beta distribution, calculate % of then-cumulative options to be exercised
# add randomly generated percent.exercised using a normal distribution [use rtruncnorm(n, 2, 7)]
# and join in cumulative vested grants for each record
trx[, date := trx.date];
# trx[, percent.exercised := rbeta(trx[, .N], 2, 7)];
trx[, percent.exercised := rtruncnorm(trx[, .N], mean = .10, sd = .04, a = 0, b = .2)];
setkeyv(trx, c("employee.id", "date"));
trx <- trx[cumulative.vested, nomatch = 0];
trx[, c("hire.date", "days.to.next.trx", "cumulative.days") := list(NULL, NULL, NULL)];

# add in field for grants exercised
trx[, exercised := 0.0];
rm(temp);

#######################
##	Deliver Results - just loop for now (hopefully vectorize later)
#######################
# prepare variables
employee.trx <- trx[, list(employee.id, trx.number)];
setkeyv(employee.trx, c("employee.id"));
remaining.grants.locf <- 0.0;
new.grants <- 0.0;

# merge in stock market price in trx data.table
setkeyv(trx, c("date"));
trx <- merge(trx, stock, all.x = TRUE)[,list(employee.id,
	trx.number, trx.date, date, percent.exercised,
	cumulative.grants, strike.price, exercised, price)];
setkeyv(trx, c("employee.id", "trx.number"));

system.time(
# loop through all employee-transaction combinations
for(e in employees[, employee.id]) {
	remaining.grants.locf <- 0.0;
	for(n in employee.trx[J(e), ][, trx.number]) {

		# determine the number of new grants vested since the last transaction for the employee
		new.grants <- trx[J(e, n),][, cumulative.grants] - trx[J(e, n - 1),][, cumulative.grants];
		if(is.na(new.grants)) new.grants <- 0.0;

		# calculate the number of shares exercised, but only if strike price 
		#	is below market price and would exercise more than 5 shares
		remaining.grants.locf <- remaining.grants.locf + new.grants;
		trx[J(e, n), exercised := round(ifelse(strike.price >= price | remaining.grants.locf * percent.exercised < 5, 0,
			ifelse(percent.exercised * cumulative.grants > remaining.grants.locf, 
			remaining.grants.locf, percent.exercised * cumulative.grants)),0)];
		remaining.grants.locf <- remaining.grants.locf - trx[J(e, n),][, exercised];
	}
}
)
trx[, cumulative.exercised := cumsum(exercised), by = "employee.id"];


# spread transactions across actual grant ids

#############################################

# add in cumulative grant count by employee
setkeyv(grants, c("employee.id", "grant.id"));
grants[, grants.cumulative := cumsum(count.granted), by = employee.id];
setkeyv(grants, "employee.id");
setkeyv(trx, "employee.id");
# combine trx table with original grants keeping only the necessary fields
trx.grants <- merge(trx, grants, by = "employee.id", 
	allow.cartesian = TRUE)[, list(employee.id, trx.number, trx.date, grant.id, strike.price.y,
	exercised, cumulative.exercised, count.granted, grants.cumulative, price)];
setnames(trx.grants, "strike.price.y", "strike.price");
setkeyv(trx.grants, c("trx.date"));
# set key for later use
setkeyv(trx.grants, c("employee.id", "trx.number", "grant.id"));

# add in fields
trx.grants[, c("beginning.grants", "ending.grants") := list(
	grants.cumulative - cumulative.exercised + exercised,
	grants.cumulative - cumulative.exercised)];

# very useful new function in data.table package for calc
# http://stackoverflow.com/a/14689467/1927178
# DT[, D := C + c(NA, B[seq_len(.N-1)])]
trx.grants[, prior.beginning := c(NA, beginning.grants[seq_len(.N-1)]),
	by = c("employee.id", "trx.number")];
trx.grants[, prior.ending := c(NA, ending.grants[seq_len(.N-1)]),
	by = c("employee.id", "trx.number")];

# create functions to improve readability in ifelse statements:
# http://stackoverflow.com/a/13598652/1927178
is.positive <- function(x) {
	(x > 0) & (!is.na(x));
}
is.negative <- function(x) {
	(x < 0) & (!is.na(x));
}
lesser <- function(a, b) {
	# does not handle a being NA
	ifelse((a < b) & (!is.na(b)), a, 
		ifelse(is.na(b), a, b));
}

# add in calculated grants
trx.grants[, grants.exercised := ifelse(
	grants.cumulative > cumulative.exercised & (is.na(prior.beginning) | is.negative(prior.beginning)),
	exercised,
	ifelse(
		is.negative(ending.grants) & is.positive(beginning.grants),
		lesser(count.granted, beginning.grants),
		ifelse(
			is.negative(prior.ending) & is.positive(prior.beginning),
			lesser(count.granted, (exercised - prior.beginning)),
			0
		)
	)
)];

#file.name <- "C:/Users/zmueller/Documents/GitHub/sql-project/project-design/R/grants.log"
#write.table(trx.grants, file = file.name, sep = "\t", row.names = FALSE);

trx.grants <- trx.grants[grants.exercised > 1,];
trx.grants <- trx.grants[strike.price < price,];


#########################################################
#	checks
#########################################################
# exercising more than was granted
trx.grants[exercised > count.granted,];

# exercising when strike price is above market price
trx.grants[strike.price >= price,];

#########################################################







#############################################
##	Create employee #s and grant #s
#############################################
# Employee # --> as.character(100000 + employee.id)
# Grant # --> 'NG' + as.character(1000000 + grant.id)
# Outside Investor --> 'INV' + as.character(20000 + investor.id)
# Warrant # --> 'WT' + as.character(10000 + warrant.id)
# Create functions to apply on the output.table
create.employee.number <- function(x) {
	as.character(100000 + x);
}
create.grant.number <- function(x) {
	paste('NG', as.character(1000000 + x), sep = '');
}

#############################################

####	WARRANTS ISSUED
##	Have a few big issuances of warrants to outside investors
##	(named Huey, Dewey, and Louie)
##	of about 20-30% outstanding shares
outside.investments <- 3;
warrant.grant.dates <- c(as.Date("1997-01-01"),as.Date("2003-01-01"),as.Date("2009-01-01")) +
	days(as.integer(rtruncnorm(outside.investments, a = 10, b = 90,
	mean = 30, sd = 50)));
warrant.exercise.dates <- warrant.grant.dates +
	days(as.integer(rtruncnorm(outside.investments, a = 365, b = 365*2,
	mean = 365, sd = 365*0.5)));

investor.trx <- data.table(employee_number = c("INV50001", "INV50002", "INV50003"),
	grant_id = c("WT200001", "WT200002", "WT200003"),
	grant_date = warrant.grant.dates,
	exercise_date = warrant.exercise.dates,
	shares_exercised = as.double(round(rtruncnorm(outside.investments, a = initial.shares.outstanding * 0.1,
		b = initial.shares.outstanding * 0.5,
		mean = initial.shares.outstanding * 0.3,
		sd = initial.shares.outstanding * 0.15), -5)),
	strike_price = stock[J(warrant.grant.dates),price],
	aggregate_strike_price = 0.0,
	market_price = stock[J(warrant.exercise.dates),price],
	aggregate_market_price = 0.0, increase_in_value = 0.0
);
investor.trx[,`:=`(aggregate_strike_price = as.double(shares_exercised * strike_price),
	aggregate_market_price = as.double(shares_exercised * market_price),
	increase_in_value = as.double((shares_exercised * market_price) -
	(shares_exercised * strike_price)))];


#############################################
# OUTPUT
#trx[,list(employee.id, trx.number, date)]
#trx.grants[,list(employee.id, trx.number, grant.id, grants.exercised)]
#grants[,list(grant.id, grant.date, strike.price)]
#stock[,list(date, price)]
output.table <- merge(merge(merge(
	merge(trx[,list(employee.id, trx.number, date)],
		trx.grants[,list(employee.id, trx.number, grant.id, grants.exercised)],
		by = c("employee.id", "trx.number"), allow.cartesian = TRUE),
	stock[,list(date, "exercise.price" = price)], by = "date", allow.cartesian = TRUE),
	grants[,list(grant.id, grant.date, strike.price)],
	by = "grant.id", allow.cartesian = TRUE),
	employees[,list(employee.id, "name" = paste(first.name, last.name, sep = " "))],
	by = "employee.id", allow.cartesian = TRUE)[,
		list(
	"employee_number" = create.employee.number(employee.id),
	"grant_id" = create.grant.number(grant.id),
	"grant_date" = grant.date, "exercise_date" = date,
	"shares_exercised" = grants.exercised, "strike_price" = strike.price,
	"aggregate_strike_price" = grants.exercised * strike.price,
	"market_price" = exercise.price, "aggregate_market_price" = grants.exercised * exercise.price,
	"increase_in_value" = grants.exercised * (exercise.price - strike.price))];

file.name <- "C:/Users/zmueller/Documents/GitHub/sql-project/project-design/R/output-transactions.txt";
write.table(output.table, file = file.name, sep = "\t", row.names = FALSE);

file.name <- "C:/Users/zmueller/Documents/GitHub/sql-project/project-design/R/output-grants.txt";
setkeyv(grants, c("grant.id"));
output.grants <- grants[,list(
	"grant_id" = create.grant.number(grant.id),
	"employee_number" = create.employee.number(employee.id),
	"grant_date" = grant.date, "strike_price" = strike.price,
	"options_granted" = count.granted)];
write.table(output.grants, file = file.name, sep = "\t", row.names = FALSE);

file.name <- "C:/Users/zmueller/Documents/GitHub/sql-project/project-design/R/output-investors.txt";
write.table(investor.trx, file = file.name, sep = "\t", row.names = FALSE);

file.name <- "C:/Users/zmueller/Documents/GitHub/sql-project/project-design/R/output-stock.txt";
write.table(stock.raw[,list(date, price)],
	file = file.name, sep = "\t", row.names = FALSE);

file.name <- "C:/Users/zmueller/Documents/GitHub/sql-project/project-design/R/output-vest.txt";
setkeyv(vesting.schedule, c("vest.date", "grant.id"));
output.vest <- vesting.schedule[
	vest.date <= end.date,list(
	"grant_id" = create.grant.number(grant.id),
	"employee_number" = create.employee.number(employee.id),
	"grant_date" = grant.date,
	"vest_date" = vest.date,
	"options_vested" = count.granted)];
write.table(output.vest, file = file.name, sep = "\t", row.names = FALSE);

#############################################
