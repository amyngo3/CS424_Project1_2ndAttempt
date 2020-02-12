#
# Amy Ngo - ango8
# Project 1 - No Time To Waste - Second Attempt
#
library(rsconnect)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(leaflet)
library(lubridate)
library(stringr)
library(splitstackshape)

# ====================================================
# ========= Adjust raw dataframe to single tag litter, separate date and time, and real names =========

# read CSV file, separate by comma and header is TRUE - see environment tab
# data cells in tags column are switched to NA
litterdata <- read.csv(file = "p1excel.csv", sep = ",", header = TRUE,stringsAsFactors = FALSE)
litterdata$tags[litterdata$tags == ""] <- "untagged" 
# remove URL column
litterdata$url <- NULL
# change column names
colnames(litterdata) <- c("ChallengeId", "JoinId", "LitterId", "Hour", "Lat", "Long", "Tags", "UserId", "Username")
# time stamp is called Hour until the correct hour is placed

# ====================================================
# change longitude and latitude to character type
litterdata$Lat <- as.numeric(as.character(litterdata$Lat))
litterdata$Long <- as.numeric(as.character(litterdata$Long))

# ====================================================

# make csv file into data frame
dflitter <- data.frame(litterdata)

# remove rows containing a comma -- results in 3828 out of 8488 single tags of litter
#singleTags <- dflitter[!grepl(",", dflitter$Tags),]
singleTags <- cSplit(dflitter, "Tags", sep = ",", direction = "long")
singleTags <- as.data.frame(singleTags)

# ========= Splitting Data =========

# make separate dataframe after spliting the Timestamp in singleTags
dateAndTimeSplit <- data.frame(str_split_fixed(singleTags$Hour, " ", 2))
#dateAndTimeSplit$X1 <- format(as.Date(dateAndTimeSplit$X1), "%B %d %Y")
timeSplit <- data.frame(str_split_fixed(dateAndTimeSplit$X2, ":", 3)) # split time into Hour, Minute, Seconds

# ========= change Timezone and Date to Chicago's time =========
# referencing to: https://stackoverflow.com/questions/1395117/how-do-you-convert-dates-times-from-one-time-zone-to-another-in-r
#singleTags$Hour <- format(as.POSIXct(singleTags$Hour), format = "%H", tz =  "America/Chicago")

# ========= Order Days of the Week ========= 
# get the date format as Date -> name of day -> table -> data frame
#dowTable <- data.frame(table(weekdays(as.Date(singleTags$Hour, "%Y-%m-%d"))))
# refer to https://stackoverflow.com/questions/10309564/reorder-factor-levels-by-day-of-the-week-in-r
#dowTable$Var1 <- factor(dowTable$Var1, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
#dowTable <- dowTable[order(dowTable$Var1), ] # days of the week table

# ========= Adjust the Time and Date =========
#singleTags$Time <- dateAndTimeSplit$X2 # change Time column with the dataframe's split columns
singleTags$Date <- as.Date(dateAndTimeSplit$X1) # create new column -- DO NOT FORMAT DATE HERE. OTHERWISE IT'LL CRASH THE CHARTS
#singleTags$Date <- dateAndTimeSplit$X1 # insert date column in the new column

# change hour to numeric -- see professor's example
singleTags$Hour <- as.numeric(as.character(timeSplit$X1))

singleTags$Day <- weekdays(as.Date(singleTags$Date)) # day of the week column added into the data frame -- wday (number of day) or weekdays (name of day)
singleTags$Date <- format(as.Date(singleTags$Date), "%B %d %Y") # format date HERE


# split singleTags table into usernames
usernameTable <- split(singleTags, singleTags$Username)

# remove all NA
singleTags <- na.omit(singleTags)

# ========= Varibles of Columns and Usernames Names =========

# take column header as list names
listColNames <- c(colnames(litterdata))
listColNamesGood <- listColNames[listColNames != "URL" & listColNames != "newDate"] # remove specific columns in variable

# find bad user names
badUN <- paste("litterati-", singleTags$UserId, sep = "")
# change usernames with User_# based on their UserId
for(i in 1:length(singleTags$Username)){
    if(badUN[i] == singleTags$Username[i]) {
        singleTags$Username[i] <- paste("User_", singleTags$UserId[i], sep ="")
    }
}

# take unique rows of usernames
listUsernames <- unique(singleTags$Username, incomparables = FALSE)
listUsernamesGood <- na.omit(listUsernames) # remove NA usernames
# tags are turned into character type
singleTags$Tags <- as.character(singleTags$Tags)

# final dataframe
allData <- singleTags

# ====================================================
# Table of Top 10 Litter
freqLitter <- as.data.frame(sort(table(allData$Tags), decreasing = TRUE))
topLitter <- freqLitter[1:10, 1:2]
colnames(topLitter) <- c("Litter Tag", "Count")
mostPickedLit <- (topLitter[1,])

# take unique rows of litter
listLitter <- unique(singleTags$Tags, incomparables = FALSE)

# ====================================================
# Table of Top 10 Users
freqUser <- as.data.frame( sort(table(allData$Username), decreasing = TRUE) ) # frequency of usernames in descending order

# table of top 10 users with second column of frequency
topUsers <- freqUser[1:10, 1:2]

listUsernamesGood <- as.character(topUsers$Var1)

# ====================================================

# descending order of litter count
dfLitterCount <- as.data.frame( sort(table(allData$Tags), decreasing = TRUE) )

# ====================================================

# Create shiny app
ui <- dashboardPage(
    dashboardHeader(title = "CS 424 Spring 2020 No Time To Waste"),
    dashboardSidebar(disable = FALSE, collapsed = FALSE,
                     # URL items so then it's easier to reach on the big screen in classroom -- short people, ya' know?
                     sidebarMenu(
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL)),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     
                     # Added drop down menu
                     selectInput("User", "Select User to visualize", c("summary", listUsernamesGood), selected = "summary"),
                     selectInput("Litter", "Select litter type to visualize", c("summary", listLitter), selected = "summary")
                     # ===================
    ),
    dashboardBody(
        fluidRow(
            column(12, 
                   fluidRow(
                        box(title = "Leaflet Map", solidHeader = TRUE, status = "primary", width = 12,
                            leafletOutput("leaf", height = 250)
                        )
                    ),
                   fluidRow(
                       box( title = "Each Day (April 2018 - January 2020)", solidHeader = TRUE, status = "primary", width = 12,
                            plotOutput("bar1Ed", height = 250)
                       )
                   ),
            ),
            column(2,
                   
                   fluidRow(
                       box(title = "Most Litter Collected", solidHeader = TRUE, status = "primary", width = 12,
                           tableOutput("mLc")
                       )
                   ),
                   fluidRow(
                       box(title = "Top 10 Litter Collected", solidHeader = TRUE, status = "primary", width = 12,
                           tableOutput("top10")
                       )
                   )
                   
            ),
            
            column(10, 
                   fluidRow(
                       box( title = "Days Of The Week", solidHeader = TRUE, status = "primary", width = 12,
                            plotOutput("bar2Dw", height = 250)
                       )
                   ),
                   fluidRow(
                       box( title = "Hourly Litter Collected", solidHeader = TRUE, status = "primary", width = 12,
                            plotOutput("bar3Hr", height = 250)
                       )
                   )
            )
        )
    ))

server <- function(input, output) {
    
    # increase the default font size
    theme_set(theme_grey(base_size = 18)) 
    
    # =======================
    
    # reactive function to return data if user changes the drop down menu selection
    reactiveFunc <- reactive({
        # both summary selection
        if(input$User == "summary" & input$Litter == "summary")
            return (allData)
        # specific user and summary litter type
        else if ( input$Litter == "summary")
            return (allData[allData$Username == input$User,])
        # summary users and specific litter
        else if (input$User == "summary")
            return (allData[allData$Tags == input$Litter,])
        # both specific selection
        else
            return (allData[allData$Username == input$User,]) & (allData[allData&Tags == input$Litter,])
    })
    
    # =======================
    
    # create hourly map
    output$bar3Hr <- renderPlot({
        #justOneYear <- justOneYearReactive()
        dataInfo <- reactiveFunc()
        ggplot(dataInfo, aes(x=dataInfo$Hour)) + 
            # color picker link: https://htmlcolorcodes.com/color-picker/
            labs(x=paste("Hour (24 Hours)")) +  geom_bar(position = "dodge", fill="#E7431B")
    })
    
    
    # display litter collected each day between dates
    output$bar1Ed <- renderPlot({
        #justOneYear <- justOneYearReactive()
        dataInfo <- reactiveFunc()
        ggplot(dataInfo, aes(x=dataInfo$Date)) +
            labs(x=paste("Date")) + geom_bar(position = "dodge", fill="#E7431B") + 
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) # change x-axis text to 45 degrees
    })
    
    
    # display litter on the days of the week
    output$bar2Dw <- renderPlot({
        dataInfo <- reactiveFunc()
        #x = factor(day, weekdays(min(my_data$date) + 0:6))
        ggplot(dataInfo, aes(x= dataInfo$Day ) ) +
            labs(x=paste("Day of the Week")) + geom_bar(fill="#E7431B")
    })
    
    
    # display the top litter collected - never changes
    output$mLc <- renderTable(mostPickedLit[1,])
    
    
    # use DT to help out with the tables - https://datatables.net/reference/option/
    output$top10 <- renderTable(topLitter)
    
    # add a leaflet map and put a marker on it at the location of the lab
    # while not overly useful this can ceratinly be expnded upon
    output$leaf <- renderLeaflet({
        dataInfo <- reactiveFunc()
        map <- leaflet(dataInfo)
        map <- addTiles(map)
        map <- setView(map, lng = dataInfo$Long[1], lat = dataInfo$Lat[1], zoom = 18)
        map <- addMarkers(map, lng = dataInfo$Long[1], lat = dataInfo$Lat[1], popup = paste(dataInfo$Tag[1]))
        map
    })
    
    
}

shinyApp(ui = ui, server = server)