# Set the working directory
setwd("../data")
filenames = list.files()
n = length(filenames)

header.m = function(filename){           #headerline manipulation function : the total number of characters in this line remains the same
  lines = readLines(filename, encoding = "UTF-8")
  lines.new = gsub("[\u00A0]", " ", lines)
  header = grep("^place", lines.new, value = TRUE, ignore.case = TRUE)
  header = toupper(header)
  header = gsub("([A-Z]+)\\sTIM", replacement = "\\1-TIM", header) ##change GUN TIM into GUN-TIME and NET TIM into NET-TIME but still keeps its previous number of characters. will deal with DIV  /TOT and DIV /TOT in the following functions!! (specify this when finishing)
  header = gsub('([0-9]+)\\s([A-Z]+)', '\\1-\\2', header) ##deal with 5 Mi and 10 Km
  header = gsub("MILE", "MI  ", header)
  return(header)
}

headerline = sapply(filenames, header.m) 

for (i in 1: n) {                       ###deal with files which do not have headers using the fact that the first half of the files include men's records from 1999 t0 2010 and the second half includes women's data in corresponding years
  if (identical (headerline[[i]], character(0)) && i > n/2) {
    headerline[[i]] = headerline[[i-n/2]]
  } else if (identical (headerline[[i]], character(0)) && i<= n/2){
    headerline[[i]] = headerline[[i+n/2]]
  } else {
    headerline[[i]] = headerline[[i]]}
}                                      

timeline = function (filename){     #index for lines that contain time
  line <- readLines(filename, encoding = "UTF-8")
  line.new = gsub("[\u00A0]", " ", line)
  index.t = grep("[0-9]+:[0-9]+", line.new)
  return(index.t)
}

width.f = function (filename){
  ind = match(filename, filenames)
  headerstring = headerline[[ind]]
  headersplit = strsplit(headerstring, split= "\\s([0-9]|[A-Z])") #split the header to get width
  width = as.numeric(sapply(headersplit, nchar)) 
  l = length(width)
  width.new = width + c(1, rep(2, l-2), 6) #last string's width does not affect other strings' widths; for files women10Mile_2001 and men10Mile_2001, there is no space after last column name "GUN", so 4 is added in case # or * are contained. 
  return(width.new)
}

width = lapply(filenames, width.f)

header.f = function (filename){        # function of getting names by using headerline
  ind = match(filename, filenames)
  headerline[[ind]] = gsub("\\s+/", replacement = "/", headerline[[ind]]) ##get rid of the space in DIV /TOT
  header = gsub("GUN|GUN-TIM", "TIME", headerline[[ind]])  #TIME equals to GUN TIME  (reference in the website)
  header = strsplit(header, split = "\\s+")
  header.n = unlist(header)
  return(header.n) 
}

header = sapply(filenames, header.f)

readdat = function (filename, indext = timeline(filename)){  #data manipulation function
  ind = match(filename, filenames)
  dat = read.fwf(filename, widths = width[[ind]], encoding = "UTF-8", skip=indext[1] - 1, comment.char="", stringsAsFactors = FALSE, strip.white = TRUE) 
  
  dat.new = data.frame (gsub("[\u00A0]|\\#|\\*", "", as.matrix(dat)))
  time = indext - (indext[1]-1)
  dat.new = dat.new[time, ] #obtain rows that only contain time
  names(dat.new) = header[[ind]]
  names(dat.new)[names(dat.new)=="GUN"] <- "TIME"
  return(dat.new)
} 

data = lapply(filenames, readdat)

time.c = function (x) {               #Time conversion function quoted from TA Nick's postings on piazza.
  time = strsplit(as.character('x'), ':')[[1]]
  conv = 60 ^ seq.int(length(time) - 1, 0)
  time = sum(conv * as.integer(time))
  return(time)
}

usaidentifier = function(x){   #identify whether the participant is from us
  pos = gregexpr('[A-Z]{2}', x)[[1]]
  string = substring(x, pos, pos+1)
  if (grepl("usa|united states|washington DC", x, ignore.case = TRUE)) { #deal with several special cases
    "us"
  } else if (!is.na(match(string, state.abb))| !is.na(match(x, state.name))){
    "us"
  } else {
    "nonus"
  }                                     
}   #this function cannot accurately identify us and non us players in 2006 since most Hometowns only contain city names


data.m = function (filename, time = time.c(x), id = usaidentifier(x),dat = readdat(filename, indext = timeline(filename))){
  ind = match(filename, filenames)
  dat[dat==""]  <- NA    #replace blank entries by NAs
  pattern = "([A-z]+)10Mile_([0-9]+)"
  gender = gsub(pattern, "\\1", filename)
  year = gsub(pattern, "\\2", filename)
  dat$YEAR = rep(year, nrow(dat))
  dat$GENDER = rep(gender, nrow(dat))         
  dat[, c("YEAR", "AG")] = sapply(dat[, c("YEAR", "AG")], as.numeric)
  dat[, c("HOMETOWN", "NAME", "PLACE")] = sapply(dat[, c("HOMETOWN", "NAME", "PLACE")], as.character)
  dat$BIRTH = dat[, "YEAR"] - dat[, "AG"]
  dat$TIME = sapply(dat$TIME, time)
  tmp=lapply(dat$HOMETOWN, usaidentifier)
  tmp=unlist(tmp)
  dat$IDENTIFIER = tmp
  dat = dat[, c("PLACE", "NAME", "AG", "HOMETOWN", "TIME", "YEAR", "GENDER", "BIRTH", "IDENTIFIER")]  ###Obtain common columns in the whole data frames.
  return(dat)
}

dat = lapply(filenames, data.m)  

data_w = do.call("rbind", dat)   #combine 24 data frames into 1

tt= complete.cases(data_w)
table(tt) #frequency table

data_t = data_w [which (tt == TRUE),] # remove 239 incomplete cases
data_f = data_w [which (tt == FALSE),] # incomplete cases
data_f [which(data_f$YEAR== 2008), ] # check removed cases in 2007 

#data summary
summary(data_t$TIME)

par(mfrow = c(1,2))

library("ggplot2")

boxplot(data_t$TIME, main = "Boxplot of Running Time", xlab = "Data from 1999 to 2010")
counts <- table(data_t$GENDER, data_t$YEAR)
barplot(counts, main="Participants by Years and Genders",
        xlab="Years", col=c("darkblue","pink"),
        legend = rownames(counts), args.legend = list(x="topleft"))


###########
#Men and Women group comparison
###########
plot1 <- qplot(YEAR, TIME, data = data_t) + aes(colour = factor(GENDER)) + stat_summary(fun.y = mean, geom="line")
plot2 <- qplot(YEAR, AG, data = data_t) + aes(colour = factor(GENDER)) + stat_summary(fun.y = mean, geom="line")

###Comparison between mean time for both genders from 1999 to 2010
by(data_t[, "TIME"], data_t[, c("GENDER", "YEAR")], mean, na.rm = TRUE)

dat.x = data_t[-which(data_t$YEAR==2006), ]
counts <- table(dat.x$IDENTIFIER, dat.x$YEAR)
barplot(counts, main="Participants by Years and Identifiers",
        xlab="Years", col=c("darkblue","pink"),
        legend = rownames(counts), args.legend = list(x="topleft"))

#participant matching over years
nm <- c("NAME", "HOMETOWN")
res <- do.call(rbind, lapply(split(data_t, as.character(interaction(data_t[, nm]))),function(x) {
  x[duplicated(x[, nm]) | duplicated(x[, nm], fromLast = TRUE), ]}))
nm1 <- c("NAME", "HOMETOWN", "BIRTH")
res1 <- do.call(rbind, lapply(split(data_t, as.character(interaction(data_t[, nm1]))),function(x) {
  x[duplicated(x[, nm1]) | duplicated(x[, nm1], fromLast = TRUE), ]}))