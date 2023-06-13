#Change system language after every loaded data set to achieve correct months

#Shark 6
Sys.setlocale("LC_TIME", "Danish_Denmark.utf8")
# Read the CSV file into R
file6.2path <- "C:\\Users\\youss\\OneDrive\\Skrivebord\\Shark6-tag2-131976-Series.csv"
data6.2 <- read.csv2(file6.2path, header = TRUE)

file6.3path <- "C:\\Users\\youss\\OneDrive\\Skrivebord\\Shark6-tag3-138258 test.csv"
data6.3 <- read.csv2(file6.3path, header = TRUE)

data6 <- read.csv2(file6.3path, header = TRUE)

# Create a variable that includes both the date and time information
time_and_date6 <- paste(data6$Day, data6$Time)
# Transform the ariable that includes both the date and time information
Ttime6 <- as.POSIXct(strptime(time_and_date6, format = "%d-%b-%y %H:%M:%S"))
# Create a data frame with the transformed time and depth

df6 <- data.frame(
  time = Ttime6,
  Depth = data6$Depth,
  Temperature = data6$Temperature
)

#Shark 17
Sys.setlocale("LC_TIME", "English_United States.utf8")
file17_path <- "C:\\Users\\youss\\OneDrive\\Skrivebord\\Shark17-tag1-158793-Series (1).csv"
data17 <- read.csv(file17_path)
# Create a variable that includes both the date and time information
time_and_date17 <- paste(data17$Day, data17$Time)
# Transform the variable that includes both the date and time information
Ttime17 <- as.POSIXct(time_and_date17, format = "%d-%b-%Y %H:%M:%S")
# Create a data frame with the transformed time and depth
df17 <- data.frame(
  time = Ttime17,
  Depth = data17$Depth,
  Temperature = data17$Temperature
)

#Shark 30
file30_path <- "C:\\Users\\youss\\OneDrive\\Skrivebord\\Shark30-100982-Series.csv"
data30 <- read.csv(file30_path)
# Create a variable that includes both the date and time information
time_and_date30 <- paste(data30$Day, data30$Time)
# Transform the ariable that includes both the date and time information
Ttime30 <- as.POSIXct(strptime(time_and_date30, format = "%d-%b-%y %H:%M:%S"))
# Create a data frame with the transformed time and depth
df30 <- data.frame(
  time = Ttime30,
  Depth = data30$Depth,
  Temperature = data30$Temperature
)

#time of day calculation. The number should range smoothly from 0 : ~23.99999. 
#If it's anything other than that then it's not calculated right. 
df30$TOD <- as.numeric(format(df30$time, format = "%H")) + as.numeric(format(df30$time, format = "%M")) / 60 + 
  as.numeric(format(df30$time, format = "%S")) / 60 / 60

df17$TOD <- as.numeric(format(df17$time, format = "%H")) + as.numeric(format(df17$time, format = "%M")) / 60 + 
  as.numeric(format(df17$time, format = "%S")) / 60 / 60

Sys.setlocale("LC_TIME", "Danish_Denmark.utf8")

df6$TOD <- as.numeric(format(df6$time, format = "%H")) + as.numeric(format(df6$time, format = "%M")) / 60 +
  as.numeric(format(df6$time, format = "%S")) / 60 / 60

Sys.setlocale("LC_TIME", "English_United States.utf8")
df30 <- df30[!is.na(df30$TOD),]



#Estimate initial parameters for all 3 states using hierarchical clustering (this method resultet in the best likelihood)
init_params30 <- initz(na.omit(df30$Depth), ncomp = 3, init.method = "hclust")
init_params17 <- initz(na.omit(df17$Depth), ncomp = 3, init.method = "hclust")
init_params6 <- initz(na.omit(df6$Depth), ncomp = 3, init.method = "hclust")

# Initial values from the estimation
mean30_Depth <- c(220.7267738, 591.5, 831.0199)
sd30_Depth <- c(34.10027, 90.82657, 52.55956)
df30$Depth[1] <- 1

mean17_Depth <- c(79.70845, 281.24234, 421.93630)
sd17_Depth <- c(64.42877, 51.46055, 36.73780)

mean6_Depth <- c(321.6364, 692.8450, 1078.7261)
sd6_Depth <- c(76.50733, 72.92805, 146.37132)
