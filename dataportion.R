library(tidyr)
library(dplyr)

#data code
dat=read.csv(file.choose())
#Sales (ouptut variable) is heavily skewed, running log to normalize
#compute log of sales
logSales=log(dat$Sales)
data=data.frame(dat, logSales)
names(data)
#removing unused variables
data=data[, !(names(data) %in% c("Console", "Title", "Publisher", "Genre", "Usedprice", 
                                 "lnUsedPrice", "Handheld", "GBA", "GCN", "Infograme", "Sales", 
                                 "Year.Released","Usedprice", "lnUsedPrice", "Handheld",
                                 "GCN", "GBA", "Infograme", "Acclaim"))]
#59 vars
#turning into factor variables
data$Y04 <- factor(data$Y04)
data$Y05 <- factor(data$Y05)
data$Y06 <- factor(data$Y06)
data$Y07 <- factor(data$Y07)
data$Y08 <- factor(data$Y08)
data$Y09 <- factor(data$Y09)
data$Y10 <- factor(data$Y10)

data$Sequel <- factor(data$Sequel)
data$Re.release <- factor(data$Re.release)
data$RatingE <- factor(data$RatingE)
data$RatingT <- factor(data$RatingT)
data$RatingM <- factor(data$RatingM)
data$Online <- factor(data$Online)
data$Licensed <- factor(data$Licensed)
data$Accessory <- factor(data$Accessory)
data$Multiplatform <- factor(data$Multiplatform)

data$NDS <- factor(data$NDS)
data$Wii <- factor(data$Wii)
data$PS2 <- factor(data$PS2)
data$PS3 <- factor(data$PS3)
data$PSP <- factor(data$PSP)
data$Xbox <- factor(data$Xbox)
data$X360 <- factor(data$X360)

data$Action <- factor(data$Action)
data$Adventure <- factor(data$Adventure)
data$Educational <- factor(data$Educational)
data$Racing <- factor(data$Racing)
data$RPG <- factor(data$RPG)
data$Simulation <- factor(data$Simulation)
data$Sports <- factor(data$Sports)
data$Strategy <- factor(data$Strategy)

data$X2K <- factor(data$X2K)
data$Activision <- factor(data$Activision)
data$Atari <- factor(data$Atari)
data$Capcom <- factor(data$Capcom)
data$Disney <- factor(data$Disney)
data$Eidos <- factor(data$Eidos)
data$EA <- factor(data$EA)
data$Konami <- factor(data$Konami)
data$Microsoft <- factor(data$Microsoft)
data$Midway <- factor(data$Midway)
data$Namco <- factor(data$Namco)
data$Nintendo <- factor(data$Nintendo)
data$Rockstar <- factor(data$Rockstar)
data$Sony <- factor(data$Sony)
data$Sega <- factor(data$Sega)
data$THQ <- factor(data$THQ)
data$SquareEnix <- factor(data$SquareEnix)
data$Ubisoft <- factor(data$Ubisoft)

data$FirstPerson <- factor(data$FirstPerson)
data$Platform <- factor(data$Platform)
data$Isometric <- factor(data$Isometric)
data$SideScrolling <- factor(data$SideScrolling)
data$TopDown <- factor(data$TopDown)
data$ThirdPerson <- factor(data$ThirdPerson)


#undummying, separate data to run ANOVA analysis in pt2
anovata=data
#rating
Ratings=select(anovata, starts_with("Rating"))
anovata$Rating=factor(apply(Ratings, 1, function(x) names(x)[x == 1]),levels=names(Ratings),ordered=FALSE)

Years=select(anovata, c("Y04","Y05", "Y06", "Y07", "Y08", "Y09", "Y10"))
anovata$Year=factor(apply(Years, 1, function(x) names(x)[x == 1]),levels=names(Years),ordered=FALSE)

Consoles=select(anovata, c("NDS","Wii", "PS2", "PS3", "PSP", "Xbox", "X360"))
anovata$Console=factor(apply(Consoles, 1, function(x) names(x)[x == 1]),levels=names(Consoles),ordered=FALSE)

Genres=select(anovata, c("Action","Adventure", "Educational", "Racing", "RPG", "Simulation", "Sports", "Strategy"))
anovata$Genre=factor(apply(Genres, 1, function(x) names(x)[x == 1]),levels=names(Genres),ordered=FALSE)

Publishers=select(anovata, c("X2K", "Activision", "Atari", "Capcom", "Disney", "Eidos", "EA", "Konami",
                             "Microsoft", "Midway", "Namco", "Nintendo", "Rockstar", "Sony", "Sega", "THQ",
                             "SquareEnix", "Ubisoft"))
anovata$Publisher=factor(apply(Publishers, 1, function(x) names(x)[x == 1]),levels=names(Publishers),ordered=FALSE)

Perspectives=select(anovata, c("FirstPerson","Platform", "Isometric", "SideScrolling", "TopDown", "ThirdPerson"))
anovata$Perspective=factor(apply(Perspectives, 1, function(x) names(x)[x == 1]),levels=names(Perspectives),ordered=FALSE)

str(data)

anovata2=anovata
anovata