library(dplyr)
library(tidyr)
library(ggplot2)
#install.packages("maps")
library(maps)
install.packages("maptools")
library(maptools)
library(gridExtra)

setwd("/Users/donovan/Documents/Udacity_Data_Analyst_Nanodegree/EDA/P4")

data.raw <- read.csv("adjustedcohortgrdtnratesslsy201112updated.csv")
str(data.raw)

data <- rename(data.raw, ALL_COHORT=ALL_COHORT_1112,
                         ALL_RATE=ALL_RATE_1112,
                         MAM_COHORT=MAM_COHORT_1112,
                         MAM_RATE=MAM_RATE_1112,
                         MAS_COHORT=MAS_COHORT_1112,
                         MAS_RATE=MAS_RATE_1112,
                         MBL_COHORT=MBL_COHORT_1112,
                         MBL_RATE=MBL_RATE_1112,
                         MHI_COHORT=MHI_COHORT_1112,
                         MHI_RATE=MHI_RATE_1112,
                         MTR_COHORT=MTR_COHORT_1112,
                         MTR_RATE=MTR_RATE_1112,
                         MWH_COHORT=MWH_COHORT_1112,
                         MWH_RATE=MWH_RATE_1112,
                         CWD_COHORT=CWD_COHORT_1112,
                         CWD_RATE=CWD_RATE_1112,
                         ECD_COHORT=ECD_COHORT_1112,
                         ECD_RATE=ECD_RATE_1112,
                         LEP_COHORT=LEP_COHORT_1112,
                         LEP_RATE=LEP_RATE_1112)

data[data==""]<-NA

dim(data)
unique(data$leaid11)
data$leaid11
head(data)

# "15" becomes 15
# "15-19" becomes 17
#mean(strtoi(strsplit("15-19", "-")[[1]]))
# "<20" becomes 10
# ">80" becomes 90

#cohort_rate_str_to_point_estimate <- function(rate) {
#  if (substr(rate,1,2)=="GE") {
#    return((strtoi(substr(rate,3,4))+100) / 2)
#  } else if (substr(rate,1,2)=="LT" | substr(rate,1,2)=="LE") {
#    return(strtoi(substr(rate,3,4))/2)
#  } else if (grepl("-", rate)) {
#    return(mean(strtoi(strsplit(rate, "-")[[1]])))
#  } else {
#    return(strtoi(rate))
#  }
#}

#cohort_rate_str_to_point_estimate("80")
#cohort_rate_str_to_point_estimate("GE80")
#cohort_rate_str_to_point_estimate("LT50")
#cohort_rate_str_to_point_estimate("")
#cohort_rate_str_to_point_estimate("40-60")
#cohort_rate_str_to_point_estimate("40-55")
#cohort_rate_str_to_point_estimate(select(head(data, 100), ALL_RATE))

#holy shit, I can't believe I got this function working :O
cohort_rate_str_to_point_estimate <- function(rates) {
  rates <- as.character(rates)
  return(ifelse(rates=="",
    "NA",
  ifelse(substr(rates,1,2)=="GE",
    (strtoi(substr(rates,3,4))+100) / 2,
  ifelse(substr(rates,1,2)=="LT" | substr(rates,1,2)=="LE",
    strtoi(substr(rates,3,4))/2,
  ifelse(grepl("-", rates),
    colMeans(apply(data.frame(strsplit(rates, "-")), 2, strtoi)),
    strtoi(rates)
  )
  )
  )
  ))
}

#mean(strtoi(c(unlist(strsplit("20-40", "-")))))
#strtoi(unlist(strsplit(c("20-40", "30-50"), "-"), recursive=FALSE))
#dim(c(strsplit(c("20-40", "30-50"), "-")))
#ldply(strsplit(c("20-40", "30-50"), "-"), .fun=strtoi)
#rowMeans(apply(data.frame(strsplit(c("20-40", "30-50"), "-")), 1:2, strtoi))
#rowMeans(apply(data.frame(strsplit(as.character(select(head(data, 100), ALL_RATE)), "-")), 1:2, strtoi))
#select(head(data, 100), ALL_RATE)
#colMeans(apply(data.frame(strsplit(as.character(select(head(data, 100), ALL_RATE)$ALL_RATE), "-")), 2, strtoi))


#strtoi(strsplit(c("20-40", "30-50"), "-"))
#rowMeans(strtoi(strsplit(c("20-40", "30-50"), "-")))

#select(head(data, 100), ALL_RATE)  %>% mutate(ALL_RATE2=cohort_rate_str_to_point_estimate(ALL_RATE))
#select(head(data, 100), ALL_RATE)  %>% mutate(ALL_RATE2=ifelse(substr(ALL_RATE,1,2)=="GE",
#                                                               (strtoi(substr(ALL_RATE,3,4))+100) / 2,
#                                                               ifelse(substr(ALL_RATE,1,2)=="LT" | substr(ALL_RATE,1,2)=="LE",
#                                                                      strtoi(substr(ALL_RATE,3,4))/2,
#                                                                      ifelse(grepl("-", ALL_RATE),
#                                                                             mean(strtoi(strsplit(ALL_RATE, "-")[[1]])),
#                                                                             strtoi(ALL_RATE)
#                                                                      ))))


data <- data %>% mutate(ALL_RATE=cohort_rate_str_to_point_estimate(ALL_RATE),
                        MAM_RATE=cohort_rate_str_to_point_estimate(MAM_RATE),
                        MAS_RATE=cohort_rate_str_to_point_estimate(MAS_RATE),
                        MBL_RATE=cohort_rate_str_to_point_estimate(MBL_RATE),
                        MHI_RATE=cohort_rate_str_to_point_estimate(MHI_RATE),
                        MTR_RATE=cohort_rate_str_to_point_estimate(MTR_RATE),
                        MWH_RATE=cohort_rate_str_to_point_estimate(MWH_RATE),
                        CWD_RATE=cohort_rate_str_to_point_estimate(CWD_RATE),
                        ECD_RATE=cohort_rate_str_to_point_estimate(ECD_RATE),
                        LEP_RATE=cohort_rate_str_to_point_estimate(LEP_RATE))

qplot(ALL_RATE, data=data, binwidth=1) + xlab("Graduation Rate")
summary(data$ALL_RATE)

#maybe the peak at 25 is due to a lot of small sample sizes having a particular range that was averaged to 25
table(data.raw$ALL_RATE_1112)

#YEP!, there are 295 schools with LT50 as their graduation rate

#perhaps a better approach would be to randomize these ranges
#for example, a normal distribution centered around the midpoint

#Reinitialize data
data <- rename(data.raw, ALL_COHORT=ALL_COHORT_1112,
               ALL_RATE=ALL_RATE_1112,
               MAM_COHORT=MAM_COHORT_1112,
               MAM_RATE=MAM_RATE_1112,
               MAS_COHORT=MAS_COHORT_1112,
               MAS_RATE=MAS_RATE_1112,
               MBL_COHORT=MBL_COHORT_1112,
               MBL_RATE=MBL_RATE_1112,
               MHI_COHORT=MHI_COHORT_1112,
               MHI_RATE=MHI_RATE_1112,
               MTR_COHORT=MTR_COHORT_1112,
               MTR_RATE=MTR_RATE_1112,
               MWH_COHORT=MWH_COHORT_1112,
               MWH_RATE=MWH_RATE_1112,
               CWD_COHORT=CWD_COHORT_1112,
               CWD_RATE=CWD_RATE_1112,
               ECD_COHORT=ECD_COHORT_1112,
               ECD_RATE=ECD_RATE_1112,
               LEP_COHORT=LEP_COHORT_1112,
               LEP_RATE=LEP_RATE_1112)

#This data set uses empty strings for null values, but NA works better with R
data[data==""]<-NA

norm_from_max <- function(maxn) {
  minn <- 0
  n <- rnorm(1, mean=(maxn + minn)/2, sd=(maxn - minn)/4)
  return(ifelse(n<minn, minn,
                ifelse(n>maxn, maxn,
                       n)))
}
norm_from_min <- function(minn) {
  maxn <- 100
  n <- rnorm(1, mean=(maxn + minn)/2, sd=(maxn - minn)/4)
  return(ifelse(n<minn, minn,
         ifelse(n>maxn, maxn,
                n)))
}
norm_from_min_max <- function(nums) {
  n <- rnorm(1, mean=(max(nums) + min(nums))/2, sd=(max(nums) - min(nums))/4)
  return(ifelse(n<min(nums), min(nums),
         ifelse(n>max(nums), max(nums),
                n)))
}
cohort_rate_str_to_point_estimate <- function(rates) {
  rates <- as.character(rates)
  return(ifelse(rates=="",
                "NA",
                ifelse(substr(rates,1,2)=="GE",
                       apply(data.frame(strtoi(substr(rates,3,4))), 1, norm_from_min),
                       ifelse(substr(rates,1,2)=="LT" | substr(rates,1,2)=="LE",
                              apply(data.frame(strtoi(substr(rates,3,4))), 1, norm_from_max),
                              ifelse(grepl("-", rates),
                                     apply(apply(data.frame(strsplit(rates, "-")), 2, strtoi), 2, norm_from_min_max),
                                     strtoi(rates)
                              )
                       )
                )
  ))
}

data <- data %>% mutate(ALL_RATE=cohort_rate_str_to_point_estimate(ALL_RATE),
                        MAM_RATE=cohort_rate_str_to_point_estimate(MAM_RATE),
                        MAS_RATE=cohort_rate_str_to_point_estimate(MAS_RATE),
                        MBL_RATE=cohort_rate_str_to_point_estimate(MBL_RATE),
                        MHI_RATE=cohort_rate_str_to_point_estimate(MHI_RATE),
                        MTR_RATE=cohort_rate_str_to_point_estimate(MTR_RATE),
                        MWH_RATE=cohort_rate_str_to_point_estimate(MWH_RATE),
                        CWD_RATE=cohort_rate_str_to_point_estimate(CWD_RATE),
                        ECD_RATE=cohort_rate_str_to_point_estimate(ECD_RATE),
                        LEP_RATE=cohort_rate_str_to_point_estimate(LEP_RATE))

head(data$ALL_RATE, 20)
head(data.raw$ALL_RATE_1112, 20)

data <- data %>% rename(region=STNAM)

state_postal_codes <- read.csv("state_postal_codes.csv")
state_postal_codes <- select(state_postal_codes, State, Postal.Code)
state_postal_codes <- rename(state_postal_codes, region=State)
state_postal_codes <- mutate(state_postal_codes, region=toupper(region))
head(state_postal_codes)

data <- left_join(data, state_postal_codes, by="region")

write.csv(data, file="graduation_rate_data.csv")

ggplot(aes(ALL_RATE), data=data) + xlab("Graduation Rate") + geom_histogram(fill="lightblue", color="black", binwidth=1)
ggplot(aes(Pupil.Teacher.Ratio), data=data) + xlab("Pupil Teacher Ratio") + geom_histogram(fill="purple", color="black", binwidth=1) + xlim(0,40)
ggplot(aes(Total.General.Revenue.Per.Student), data=data) + xlab("Revenue Per Student") + geom_histogram(fill="lightgreen", color="black", binwidth=10000)
ggplot(aes(Total.General.Revenue.Per.Student), data=data) + xlab("Revenue Per Student") + geom_histogram(fill="lightgreen", color="black", binwidth=.05) + scale_x_continuous(trans="log10")
summary(data$ALL_RATE)
summary(data$Pupil.Teacher.Ratio)
summary(data$Total.General.Revenue.Per.Student)

ggplot(aes(x=Postal.Code, y=ALL_COHORT), data=data) + geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) 

#Those outliers look really dubious. Over 30,000 in a graduating class? After a quick bout of research,
#I found that the largest graduating class in U.S. history was less than 1,600, so I limited the plot accordingly.

ggplot(aes(x=Postal.Code, y=ALL_COHORT), data=data) + geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  ylim(0, 1600)

#That looks much better, so let's just throw away the outlying records.

data <- data %>% filter(ALL_COHORT<1600)

school_districts <- read.csv("ELSI_csv_export_6358303792966719138695.csv")
head(school_districts)
str(school_districts)
unique(school_districts$County.Name)
school_districts <- rename(school_districts, 
                           State.Name=State.Name..District..Latest.available.year,
                           County.Name=County.Name..District..2011.12,
                           leaid11=Agency.ID...NCES.Assigned..District..Latest.available.year,
                           Full.Time.Teachers=Full.Time.Equivalent..FTE..Teachers..District..2011.12,
                           Pupil.Teacher.Ratio=Pupil.Teacher.Ratio..District..2011.12,
                           Total.General.Revenue=Total.General.Revenue..TOTALREV...District.Finance..2011.12,
                           Number.Public.Schools=Total.Number.of.Public.Schools..Public.School..2011.12,
                           Total.Students=Total.Students..UG.PK.12...District..2011.12)
#str(school_districts)
#as.numeric(substr("=0624630", 2, 8))
#grepl("=", "=0624630")
leaid_as_numeric <- function(dataItem) {
  return(ifelse(grepl("=", dataItem), as.numeric(substr(dataItem, 2, 20)), strtoi(dataItem)))
}
scholastic_data_as_numeric <- function(dataItem) {
  return(ifelse(grepl("=", dataItem), as.numeric(substr(dataItem, 2, 20)), as.numeric(as.character(dataItem))))
}

#scholastic_data_as_numeric <- function(dataItem) {
#  return(ifelse(grepl("=", dataItem), 
#                as.numeric(substr(dataItem, 2, 20)), 
#                ifelse(strtoi(dataItem), 
#                       strtoi(dataItem),
#                       as.numeric(as.character(dataItem)))))
#}


head(school_districts$Full.Time.Teachers, 100)
scholastic_data_as_numeric(head(school_districts$Full.Time.Teachers, 100))

school_districts <- mutate(school_districts,
                           leaid11=leaid_as_numeric(leaid11),
                           Full.Time.Teachers=scholastic_data_as_numeric(Full.Time.Teachers),
                           Pupil.Teacher.Ratio=scholastic_data_as_numeric(Pupil.Teacher.Ratio),
                           Total.General.Revenue=scholastic_data_as_numeric(Total.General.Revenue),
                           Number.Public.Schools=scholastic_data_as_numeric(Number.Public.Schools),
                           Total.Students=scholastic_data_as_numeric(Total.Students))

summary(school_districts$Full.Time.Teachers)
summary(school_districts$Pupil.Teacher.Ratio)
summary(school_districts$Total.General.Revenue)
summary(school_districts$Number.Public.Schools)
summary(school_districts$Total.Students)

#head(subset(school_districts, State.Name=="Mississippi"))
#head(subset(data, region=="MISSISSIPPI"))
#head(subset(data, region=="MISSISSIPPI" & subregion=="MONROE COUNTY"))
#unique(subset(data, region=="MISSISSIPPI")$leaid11)
#unique(subset(school_districts, State.Name=="Mississippi")$leaid11)
#scholastic_data_as_numeric(subset(school_districts, State.Name=="Mississippi")$leaid11)
#unique(subset(data, region=="CALIFORNIA")$leaid11)
#unique(subset(school_districts, State.Name=="California")$leaid11)

write.csv(school_districts, "school_district_data.csv")

data <- left_join(data, school_districts, by="leaid11")
head(data)

#DONE: Look into the district data for Total.General.Revenue. It's getting repeated for each leaid11.
#Divide by number of schools in leaid11 district to get average proportion for each school.

#data_by_leaid <- data %>% group_by(leaid11) %>% 
#  summarise(Total.General.Revenue=mean(Total.General.Revenue),
#            Total.Students=mean(Total.Students))
#data_by_leaid$Total.General.Revenue.Per.Student <- data_by_leaid$Total.General.Revenue/data_by_leaid$Total.Students
#head(data_by_leaid)
summary(data$Total.General.Revenue.Per.Student)

data$Total.General.Revenue.Per.Student <- data$Total.General.Revenue/data$Total.Students

#Remove Infinities
data <- data %>% mutate(Total.General.Revenue.Per.Student=ifelse(Total.General.Revenue.Per.Student==Inf, NA, Total.General.Revenue.Per.Student))

filter(data, data$Total.General.Revenue.Per.Student>quantile(data$Total.General.Revenue.Per.Student, .999, na.rm=TRUE)) %>% select(region, schnam11, Total.General.Revenue.Per.Student)
sum(data$Total.General.Revenue.Per.Student>quantile(data$Total.General.Revenue.Per.Student, .999, na.rm=TRUE), na.rm=TRUE)
quantile(data$Total.General.Revenue.Per.Student, .999, na.rm=TRUE)

qplot(data=data, Total.General.Revenue.Per.Student, binwidth=20000)

#Remove schools that have revenues of over the .999 quantile.
data <- data %>% filter(Total.General.Revenue.Per.Student<quantile(Total.General.Revenue.Per.Student, .999, na.rm=TRUE))
data <- data %>% filter(Total.General.Revenue<quantile(Total.General.Revenue, .998, na.rm=TRUE))

#Remove Schools that have over 100 pupils per teacher. Either these values are mistaken, or these schools have atypical class environments and I don't want them affecting my analysis.
data <- data %>% filter(Pupil.Teacher.Ratio<=100)

#DONE: Get the actual number of schools from database, not all schools are represented here,
#so you can't divide by n like above, but the total number of schools.

#data <- left_join(data, select(data_by_leaid, leaid11, Total.General.Revenue.Per.Student), by="leaid11")
#data$Total.General.Revenue.District.Level <- data$Total.General.Revenue
#data$Total.General.Revenue <- data$Total.General.Revenue.Per.School
#head(select(data, -Total.General.Revenue.Per.School))
#data <- select(data, -Total.General.Revenue.Per.School)

head(data)



ggplot(aes(x=Postal.Code, y=Total.General.Revenue), data=data) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

ggplot(aes(x=Postal.Code, y=ALL_RATE), data=data) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) 

bp1 <- ggplot(aes(x=Postal.Code, y=Total.General.Revenue.Per.Student), data=data) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

bp2 <- ggplot(aes(x=Postal.Code, y=ALL_RATE), data=data) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) 

bp1
grid.arrange(bp1, bp2)

#Q: Why are these so different? Are different states reporting different units?
#I downloaded the state revenues directly to see if my transformations were solid or not.

state_revs <- read.csv("ELSI_csv_export_6358284373060549993001.csv")
state_revs <- rename(state_revs, 
                     region=State.Name,
                     Total.Revenues=Total.Revenues..TR...State.Finance..2011.12, 
                     Total.Students=Total.Students..State..2011.12,
                     Number.Schools=Total.Number.Operational.Schools..Public.School..2011.12)
head(state_revs)
str(state_revs)
#state_revs$Total.Revenues <- as.numeric(as.character(state_revs$Total.Revenues))
#state_revs$Number.Schools <- as.numeric(as.character(state_revs$Number.Schools))
#state_revs <- state_revs %>% filter(!is.na(Total.Revenues))
state_revs$Total.Revenues
state_revs$Number.Schools
str(state_revs)
state_revs <- left_join(state_revs, state_postal_codes, by="region")
#state_postal_codes
#state_revs <- state_revs %>% filter(!(region %in% c("AMERICAN SAMOA", "GUAM", "NORTHERN MARIANAS", "PUERTO RICO", "VIRGIN ISLANDS")))
state_revs$Total.Revenues.Per.School <- state_revs$Total.Revenues/state_revs$Number.Schools
state_revs$Total.Revenues.Per.Student <- state_revs$Total.Revenues/state_revs$Total.Students
state_revs
select(data, region, Total)

state_revs_plot <- ggplot(aes(x=Postal.Code, y=Total.Revenues.Per.Student), data=state_revs) +
  geom_bar(fill="darkgreen", stat="identity") + 
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) 

grid.arrange(bp1, state_revs_plot)

#There are many problems with the top graph (Total.GeneralRevenue/ALL_COHORT). The sheer number of outliers
#that fall over the $10 million per student range is a pretty big red flag. 

#ALL THIS IS FIXED, HALLELUJAH! GOT MY DATA RIGHT!
#=================================================
#Q: Why is this order not matching the bp1 plot?
#data %>% group_by(region) %>% 
#  summarise(rev=mean(Total.General.Revenue/ALL_COHORT)) %>% 
#  arrange(desc(rev))

#summary(subset(data, region=="HAWAII")$Total.General.Revenue)
#Ok, so all of Hawaii's values are the same. Maybe each entry is reporting total state revenue?
#Turns out there's only one school district in Hawaii, lol
#summary(subset(data, region=="NEVADA")$Total.General.Revenue)
#summary(subset(data, region=="NEVADA")$ALL_COHORT)
#summary(subset(data, region=="ALASKA")$Total.General.Revenue)

#I wish I had had more time to find the faulty data and filter it out, but it is out of the scope
#of this project, so I will use the same tool to get statewide revenues to do some state by state
#comparisons. While this approach introduces uncertainty because I don't know how they reconciled 
#the data problems at the finer granularity, it's nice that I have the option to choose the granularity
#and sidestep the messiness of the school-level data.

#select(subset(data, ALL_COHORT>1600), region, leaid11, leanm11, ALL_COHORT)
#dim(data)
#dim(data)
#=================================================

continental_states <- map_data("state")

#head(map_data("world", regions="us"))
#head(map_data("state", add=TRUE))

non_continental_states <- map_data("world", regions="us")
non_continental_states$region <- non_continental_states$subregion
non_continental_states$group <- non_continental_states$group + max(continental_states$group)
non_continental_states$order <- non_continental_states$order + max(continental_states$order)

hawaii <- non_continental_states %>% filter(region=="Hawaii")
alaska <- non_continental_states %>% filter(region == "Alaska")

centerState <- function(.df) { 
  .df$lat <- .df$lat - (diff(range(.df$lat, na.rm = T))/2 + min(.df$lat, na.rm = T)) 
  .df$long <- .df$long - (diff(range(.df$long, na.rm = T))/2 + min(.df$long, na.rm = T)) 
  return(.df) 
} 

scaleState <- function(.df, scale_matrix, scale_factor, lat_shift, long_shift) { 
  .df <- centerState(.df) 
  coords <- t(cbind(.df$lat, .df$long)) 
  scaled_coord <- t(scale_factor*scale_matrix %*% coords) 
  .df$lat <- scaled_coord[,1] + lat_shift 
  .df$long <- scaled_coord[,2] + long_shift 
  return(.df) 
} 

#us50_shp <- readShapePoly("your path to states.shp") 
#us50_df <- as.data.frame(us50_shp) 
#us50_points <- sp2tmap(us50_shp) 
#names(us50_points) <- c("id", "x", "y") 
#us50 <- merge(x = us50_df, y = us50_points, by.x = "DRAWSEQ", by.y = "id") 
#ggplot(data = us50, aes(x=x, y=y, group = DRAWSEQ)) + 
#  geom_polygon(color = "black", fill = "white") 
#cont_us <- us50[us50$STATE_ABBR != "HI" & us50$STATE_ABBR != "AK", ] 
#ak <- us50[us50$STATE_ABBR == "AK", ] 
#hi <- us50[us50$STATE_ABBR == "HI", ] 
scale_mat <- matrix(c(1,0,0,1.25), ncol = 2, byrow = T) 
ak_scale <- scaleState(alaska, scale_mat, 0.4, lat_shift = 25, long_shift = -45) 
hi_scale <- scaleState(hawaii, scale_mat, 1.5, lat_shift = 25, long_shift = -107) 

head(alaska)
head(ak_scale)

head(hawaii)
head(hi_scale)
#all_us <- rbind(cont_us, ak_scale, hi_scale) 
#proj_type <- "azequalarea" 
#projected <- mapproject(x = all_us$x, y = all_us$y, projection=proj_type) 
#all_us$x_proj <- projected[["x"]] 
#all_us$y_proj <- projected[["y"]] 

#ggplot(data = all_us, aes(x=x_proj, y=y_proj, group = DRAWSEQ)) + 
#  geom_polygon(color = "black", fill = "white")

#head(non_continental_states)
#head(all_states)
#unique(non_continental_states$region)

all_states <- union(continental_states, ak_scale)
all_states <- union(all_states, hi_scale)
all_states <- all_states %>% arrange(order)

all_states <- all_states %>% mutate(region=toupper(region))
all_states <- select(all_states, -subregion)

head(all_states, 100)
unique(all_states$region)
head(data)

write.csv(all_states, "state_coords.csv")

unique(data$region)
unique(data.raw$STNAM)
data.state_means <- data %>% 
  group_by(region) %>% 
  summarise(ALL_RATE = mean(ALL_RATE, na.rm=TRUE),
            ALL_COHORT = mean(ALL_COHORT, na.rm=TRUE),
            MAM_RATE = mean(MAM_RATE, na.rm=TRUE),
            MAM_COHORT = mean(MAM_COHORT, na.rm=TRUE),
            MAS_RATE = mean(MAS_RATE, na.rm=TRUE),
            MAS_COHORT = mean(MAS_COHORT, na.rm=TRUE),
            MBL_RATE = mean(MBL_RATE, na.rm=TRUE),
            MBL_COHORT = mean(MBL_COHORT, na.rm=TRUE),
            MHI_RATE = mean(MHI_RATE, na.rm=TRUE),
            MHI_COHORT = mean(MHI_COHORT, na.rm=TRUE),
            MTR_RATE = mean(MTR_RATE, na.rm=TRUE),
            MTR_COHORT = mean(MTR_COHORT, na.rm=TRUE),
            MWH_RATE = mean(MWH_RATE, na.rm=TRUE),
            MWH_COHORT = mean(MWH_COHORT, na.rm=TRUE),
            CWD_RATE = mean(CWD_RATE, na.rm=TRUE),
            CWD_COHORT = mean(CWD_COHORT, na.rm=TRUE),
            ECD_RATE = mean(ECD_RATE, na.rm=TRUE),
            ECD_COHORT = mean(ECD_COHORT, na.rm=TRUE),
            LEP_RATE = mean(LEP_RATE, na.rm=TRUE),
            LEP_COHORT = mean(LEP_COHORT, na.rm=TRUE),
            Full.Time.Teachers = mean(Full.Time.Teachers, na.rm=TRUE),
            Pupil.Teacher.Ratio = mean(Pupil.Teacher.Ratio, na.rm=TRUE),
            Total.General.Revenue = mean(Total.General.Revenue, na.rm=TRUE),
            Total.General.Revenue.Per.Student = mean(Total.General.Revenue.Per.Student, na.rm=TRUE),
            n = n()) %>% 
  arrange(ALL_RATE)

#Don't need the aggregate data in state_revs anymore now that Total.General.Revenue.Per.Student is working
#data.state_means <- full_join(data.state_means, state_revs, by="region")

head(data.state_means)
dim(data.state_means)
Total <- full_join(all_states, data.state_means, by="region")
head(Total)
Total$Total.General.Revenue.log <- log(Total$Total.General.Revenue)
Total$Total.General.Revenue.Per.Student.log <- log(Total$Total.General.Revenue.Per.Student)

dim(all_states)
dim(Total)
ggplot() + geom_polygon(data=Total, aes(x=long, y=lat, group = group, fill=Total$ALL_RATE),colour="white") + scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar") + xlim(-130, -65)
ggplot() + geom_polygon(data=Total, aes(x=long, y=lat, group = group, fill=Total$ALL_COHORT),colour="white") + scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar") + xlim(-130, -65)
ggplot() + geom_polygon(data=Total, aes(x=long, y=lat, group = group, fill=Total$n),colour="white") + scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar") + xlim(-130, -65)
ggplot() + geom_polygon(data=Total, aes(x=long, y=lat, group = group, fill=Total$Pupil.Teacher.Ratio),colour="white") + scale_fill_continuous(low = "thistle2", high = "darkblue", guide="colorbar") + xlim(-130, -65)
ggplot() + geom_polygon(data=Total, aes(x=long, y=lat, group = group, fill=Total$Total.General.Revenue.log),colour="white") + scale_fill_continuous(low = "thistle2", high = "darkgreen", guide="colorbar") + xlim(-130, -65)
ggplot() + geom_polygon(data=Total, aes(x=long, y=lat, group = group, fill=Total$Total.General.Revenue.Per.Student.log),colour="white") + scale_fill_continuous(low = "thistle2", high = "darkgreen", guide="colorbar") + xlim(-130, -65)
ggplot() + geom_polygon(data=Total, aes(x=long, y=lat, group = group, fill=log(Total$Total.Revenues/Total$Number.Schools)),colour="white") + scale_fill_continuous(low = "thistle2", high = "darkgreen", guide="colorbar") + xlim(-130, -65)
#Why is nevada so freaking high up there??? >:o
#See the list on page 39: https://www.nea.org/assets/docs/NEA-Rankings-and-Estimates-2013-2014.pdf

s1 <- ggplot() + geom_polygon(data=Total, aes(x=long, y=lat, group = group, fill=Total$ALL_RATE),colour="white") + scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar", name="Graduation Rate") + xlim(-130, -65)
s2 <- ggplot() + geom_polygon(data=Total, aes(x=long, y=lat, group = group, fill=Total$ALL_COHORT),colour="white") + scale_fill_continuous(low = "thistle2", high = "orange", guide="colorbar", name="Number of Students") + xlim(-130, -65)
s3 <- ggplot() + geom_polygon(data=Total, aes(x=long, y=lat, group = group, fill=Total$n),colour="white") + scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar") + xlim(-130, -65)
s4 <- ggplot() + geom_polygon(data=Total, aes(x=long, y=lat, group = group, fill=Total$Pupil.Teacher.Ratio),colour="white") + scale_fill_continuous(low = "thistle2", high = "darkblue", guide="colorbar", name="Student Teacher Ratio") + xlim(-130, -65)
s5 <- ggplot() + geom_polygon(data=Total, aes(x=long, y=lat, group = group, fill=Total$Total.General.Revenue.log),colour="white") + scale_fill_continuous(low = "thistle2", high = "darkgreen", guide="colorbar") + xlim(-130, -65)
s6 <- ggplot() + geom_polygon(data=Total, aes(x=long, y=lat, group = group, fill=Total$Total.General.Revenue.Per.Student),colour="white") + scale_fill_gradient(low = "thistle2", high = "darkgreen", guide="colorbar", name="Revenue Per Student", trans="log", breaks=seq(10000, 30000, 5000)) + xlim(-130, -65)
s7 <- ggplot() + geom_polygon(data=Total, aes(x=long, y=lat, group = group, fill=Total$Total.General.Revenue.Per.Student),colour="white") + scale_fill_continuous(low = "thistle2", high = "darkgreen", guide="colorbar") + xlim(-130, -65)
s8 <- ggplot() + geom_polygon(data=Total, aes(x=long, y=lat, group = group, fill=log(Total$Total.General.Revenue.Per.Student)),colour="white") + scale_fill_continuous(low = "thistle2", high = "darkgreen", guide="colorbar") + xlim(-130, -65)

grid.arrange(s1, s6)
grid.arrange(s1, s2, s4, s6)
grid.arrange(s1, s7)
grid.arrange(s7, s8)
s8

ggplot() + geom_polygon(data=all_states, aes(x=long, y=lat, group = group),colour="white")
P1 <- p + theme_bw()  + labs(fill = "Black to White Incarceration Rates \n Weighted by Relative Population" 
                             ,title = "State Incarceration Rates by Race, 2010", x="", y="")
P1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())



ggplot(aes(x=region, y=ALL_RATE), data=data) + geom_point(alpha=0.1) + geom_line(aes(group=1), stat="summary", fun.y=median, color="red") 


head(data.state_means)
data.state_means$region <- factor(data.state_means$region, as.character(data.state_means$region))


data.state_means <- left_join(data.state_means, state_postal_codes, by="region")
head(data.state_means)
data.state_means$region <- factor(data.state_means$region, as.character(data.state_means$region))
data.state_means$Postal.Code <- factor(data.state_means$Postal.Code, as.character(data.state_means$Postal.Code))
tail(data.state_means)


ggplot(aes(x=Postal.Code), data=filter(data.state_means, !is.na(Postal.Code))) + theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  geom_line(aes(y=ALL_RATE, group=2), color="black") +
  geom_point(aes(y=MAM_RATE), color="red") + geom_line(aes(y=MAM_RATE, group=2), color="red", alpha=0.5) + 
  geom_point(aes(y=MAS_RATE), color="orange") + geom_line(aes(y=MAS_RATE, group=2), color="orange", alpha=0.5) + 
  geom_point(aes(y=MBL_RATE), color="yellow") + geom_line(aes(y=MBL_RATE, group=2), color="yellow", alpha=0.5) + 
  geom_point(aes(y=MHI_RATE), color="green") + geom_line(aes(y=MHI_RATE, group=2), color="green", alpha=0.5) + 
  geom_point(aes(y=MTR_RATE), color="lightblue") + geom_line(aes(y=MTR_RATE, group=2), color="lightblue", alpha=0.5) + 
  geom_point(aes(y=MWH_RATE), color="blue") + geom_line(aes(y=MWH_RATE, group=2), color="blue", alpha=0.5) + 
  geom_point(aes(y=CWD_RATE), color="darkblue") + geom_line(aes(y=CWD_RATE, group=2), color="darkblue", alpha=0.5) + 
  geom_point(aes(y=ECD_RATE), color="purple") + geom_line(aes(y=ECD_RATE, group=2), color="purple", alpha=0.5) + 
  geom_point(aes(y=LEP_RATE), color="magenta") + geom_line(aes(y=LEP_RATE, group=2), color="magenta", alpha=0.5) 

ggplot(aes(x=Postal.Code), data=filter(data.state_means, !is.na(Postal.Code))) + theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  geom_line(aes(y=MAM_COHORT/ALL_COHORT, group=2, color="Native American")) + 
  geom_line(aes(y=MAS_COHORT/ALL_COHORT, group=2, color="Asian/Pacific Islander")) + 
  geom_line(aes(y=MBL_COHORT/ALL_COHORT, group=2, color="Black")) + 
  geom_line(aes(y=MHI_COHORT/ALL_COHORT, group=2, color="Hispanic")) + 
  geom_line(aes(y=MTR_COHORT/ALL_COHORT, group=2, color="2+ Races")) + 
  geom_line(aes(y=MWH_COHORT/ALL_COHORT, group=2, color="White")) +
  ylab("") + scale_colour_brewer(palette="Dark2", name="Cohorts")

ggplot(aes(x=Postal.Code), data=filter(data.state_means, !is.na(Postal.Code))) + theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  geom_line(aes(y=CWD_COHORT/ALL_COHORT, group=2, color="Disabled")) + 
  geom_line(aes(y=ECD_COHORT/ALL_COHORT, group=2, color="Economically Disadvantaged")) + 
  geom_line(aes(y=LEP_COHORT/ALL_COHORT, group=2, color="Limited English")) +
  ylab("") + scale_colour_brewer(palette="Dark2", name="Cohorts")

  
#Generate a scatterplot matrix with all rate variables to see which race is the strongest indicator for ALL_RATE
pairs(~ALL_RATE+MAM_RATE+MAS_RATE+MBL_RATE+MHI_RATE+MTR_RATE+MWH_RATE+CWD_RATE+ECD_RATE+LEP_RATE, data=data, main="Graduate Rates Scatterplot Matrix")
  
#install.packages("car")
library(car)
  
scatterplot.matrix(~ALL_RATE+MAM_RATE+MAS_RATE+MBL_RATE+MHI_RATE+MTR_RATE+MWH_RATE+CWD_RATE+ECD_RATE+LEP_RATE, data=data, main="Graduate Rates Scatterplot Matrix")
scatterplot.matrix(~ALL_RATE+ALL_COHORT+MAM_COHORT+MAS_COHORT+MBL_COHORT+MHI_COHORT+MTR_COHORT+MWH_COHORT+CWD_COHORT+ECD_COHORT+LEP_COHORT, data=data, main="Graduate Cohorts Scatterplot Matrix")
cor(select(data, ALL_RATE, MAM_RATE, MAS_RATE, MBL_RATE, MHI_RATE, MTR_RATE, MWH_RATE, CWD_RATE, ECD_RATE, LEP_RATE), use = "na.or.complete")
cor(select(data, ALL_RATE, ALL_COHORT, MAM_COHORT, MAS_COHORT, MBL_COHORT, MHI_COHORT, MTR_COHORT, MWH_COHORT, CWD_COHORT, ECD_COHORT, LEP_COHORT), use = "na.or.complete")
data.state_means %>% select(region, ALL_COHORT, MAM_COHORT, MAS_COHORT, MBL_COHORT, MHI_COHORT, MTR_COHORT, MWH_COHORT, CWD_COHORT, ECD_COHORT, LEP_COHORT)
cor(select(data.state_means, ALL_RATE, ALL_COHORT, Number.Schools, Total.Revenues, Total.Revenues.Per.School, Total.Revenues.Per.Student), use="na.or.complete")  
cor(select(data, ALL_RATE, ALL_COHORT, Pupil.Teacher.Ratio, Total.General.Revenue.Per.Student), use="na.or.complete")

arrange(data.state_means, desc(Total.Revenues.Per.Student))$region
  
data.state_means %>%   
  ggplot(aes(x=Postal.Code, y=ALL_COHORT)) + theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  geom_bar(position="stack", stat="identity")
  
select(data.state_means, c(-ALL_RATE, -ALL_COHORT, -MAM_RATE, -MAS_RATE, -MBL_RATE, -MHI_RATE, -MTR_RATE, -MWH_RATE, -CWD_RATE, -ECD_RATE, -LEP_RATE)) %>% 
  gather(DEMO, COHORT, MAM_COHORT, MAS_COHORT, MBL_COHORT, MHI_COHORT, MTR_COHORT, MWH_COHORT, CWD_COHORT, ECD_COHORT, LEP_COHORT) %>% 
  ggplot(aes(x=Postal.Code, y=COHORT)) + theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  geom_bar(aes(fill=DEMO), position="dodge", stat="identity")

select(data.state_means, c(-ALL_RATE, -ALL_COHORT, -MAM_RATE, -MAS_RATE, -MBL_RATE, -MHI_RATE, -MTR_RATE, -MWH_RATE, -CWD_RATE, -ECD_RATE, -LEP_RATE)) %>% 
  gather(DEMO, COHORT, MAM_COHORT, MAS_COHORT, MBL_COHORT, MHI_COHORT, MTR_COHORT, MWH_COHORT, CWD_COHORT, ECD_COHORT, LEP_COHORT) %>% 
  ggplot(aes(x=Postal.Code, y=COHORT)) + theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  geom_bar(aes(fill=DEMO), position="stack", stat="identity")

sub_cohorts <- select(data.state_means, c(-ALL_RATE, -MAM_RATE, -MAS_RATE, -MBL_RATE, -MHI_RATE, -MTR_RATE, -MWH_RATE, -CWD_RATE, -ECD_RATE, -LEP_RATE, -CWD_COHORT, -ECD_COHORT, -LEP_COHORT)) %>% 
  gather(DEMO, COHORT, MAM_COHORT, MAS_COHORT, MBL_COHORT, MHI_COHORT, MTR_COHORT, MWH_COHORT) %>% 
  filter(!is.na(Postal.Code))

sub_cohorts.group <- sub_cohorts %>% group_by(region) %>% 
  summarise(cohort.sum = sum(COHORT, na.rm=TRUE))

sub_cohorts <- left_join(sub_cohorts, sub_cohorts.group, by="region")

sub_cohorts <- sub_cohorts %>% arrange(cohort.sum, desc(COHORT)) 

sub_cohorts$Postal.Code <- factor(sub_cohorts$Postal.Code, as.character(test$Postal.Code))

ggplot(aes(x=Postal.Code, y=COHORT), data=sub_cohorts) + theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  geom_bar(aes(fill=DEMO), position="stack", stat="identity") 

select(data.state_means, c(-ALL_RATE, -ALL_COHORT, -MAM_RATE, -MAS_RATE, -MBL_RATE, -MHI_RATE, -MTR_RATE, -MWH_RATE, -CWD_RATE, -ECD_RATE, -LEP_RATE)) %>% 
  gather(DEMO, COHORT, MAM_COHORT, MAS_COHORT, MBL_COHORT, MHI_COHORT, MTR_COHORT, MWH_COHORT, CWD_COHORT, ECD_COHORT, LEP_COHORT) %>% 
  ggplot(aes(x=Postal.Code, y=COHORT)) + theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  geom_freqpoly(aes(group=DEMO, color=DEMO), position="stack", stat="identity")
 



all_counties <- map_data("county")
all_counties <- all_counties %>% mutate(region = toupper(region),
                                        subregion = paste(toupper(subregion), "COUNTY"))
head(all_counties, 100)

write.csv(all_counties, "county_coords.csv")

#school_districts$County.Name
#strsplit(as.character(school_districts$County.Name), split=' ', fixed=TRUE)
#school_districts$County.Name <- sapply(strsplit(as.character(school_districts$County.Name), split=' ', fixed=TRUE), function(x) (paste(x[1:length(x)-1], collapse=" ")))
#data <- select(data, c(-Agency.Name.y, -State.Name.y, -Agency.Name..District..2011.12.y, -County.Name.y, -Full.Time.Teachers.y, -Pupil.Teacher.Ratio.y, -Total.General.Revenue.y))
#data <- rename(data, Agency.Name=Agency.Name.x, State.Name=State.Name.x, Agency.Name..District..2011.12=Agency.Name..District..2011.12.x, County.Name=County.Name.x, Full.Time.Teachers=Full.Time.Teachers.x, Pupil.Teacher.Ratio=Pupil.Teacher.Ratio.x, Total.General.Revenue=Total.General.Revenue.x)

#names(data)
#names(all_counties)
data <- rename(data, subregion=County.Name)
#unique(data$region)
#unique(all_counties$region)
#unique(data$subregion)
#unique(all_counties$subregion)
#unique(subset(all_counties, region=="MISSISSIPPI")$subregion)
#unique(data$region)
#unique(school_districts$State.Name)
#unique(subset(data, region=="MISSISSIPPI")$subregion)

names(data)
data.county_means <- data %>% 
  group_by(region, subregion) %>% 
  summarise(ALL_RATE = mean(ALL_RATE, na.rm=TRUE),
            ALL_COHORT = mean(ALL_COHORT, na.rm=TRUE),
            MAM_RATE = mean(MAM_RATE, na.rm=TRUE),
            MAM_COHORT = mean(MAM_COHORT, na.rm=TRUE),
            MAS_RATE = mean(MAS_RATE, na.rm=TRUE),
            MAS_COHORT = mean(MAS_COHORT, na.rm=TRUE),
            MBL_RATE = mean(MBL_RATE, na.rm=TRUE),
            MBL_COHORT = mean(MBL_COHORT, na.rm=TRUE),
            MHI_RATE = mean(MHI_RATE, na.rm=TRUE),
            MHI_COHORT = mean(MHI_COHORT, na.rm=TRUE),
            MTR_RATE = mean(MTR_RATE, na.rm=TRUE),
            MTR_COHORT = mean(MTR_COHORT, na.rm=TRUE),
            MWH_RATE = mean(MWH_RATE, na.rm=TRUE),
            MWH_COHORT = mean(MWH_COHORT, na.rm=TRUE),
            CWD_RATE = mean(CWD_RATE, na.rm=TRUE),
            CWD_COHORT = mean(CWD_COHORT, na.rm=TRUE),
            ECD_RATE = mean(ECD_RATE, na.rm=TRUE),
            ECD_COHORT = mean(ECD_COHORT, na.rm=TRUE),
            LEP_RATE = mean(LEP_RATE, na.rm=TRUE),
            LEP_COHORT = mean(LEP_COHORT, na.rm=TRUE),
            Full.Time.Teachers = mean(Full.Time.Teachers, na.rm=TRUE),
            Pupil.Teacher.Ratio = mean(Pupil.Teacher.Ratio, na.rm=TRUE),
            Total.General.Revenue = mean(Total.General.Revenue, na.rm=TRUE),
            Total.General.Revenue.Per.Student = mean(Total.General.Revenue.Per.Student, na.rm=TRUE),
            n = n()) %>% 
  ungroup() %>% 
  arrange(ALL_RATE)

head(data.county_means)

Total.counties <- left_join(all_counties, data.county_means, by=c("region", "subregion"))
Total.counties$Full.Time.Teachers.log <- log(Total.counties$Full.Time.Teachers+1)
Total.counties$Pupil.Teacher.Ratio.log <- log(Total.counties$Pupil.Teacher.Ratio+1)
Total.counties$Total.General.Revenue.log <- log(Total.counties$Total.General.Revenue+1)
Total.counties$Total.General.Revenue.Per.Student.log <- log(Total.counties$Total.General.Revenue.Per.Student+1)

#(The following lines aren't necesary since we're using Spearman rank for correlation anyway)
#data$Full.Time.Teachers.log <- log(data$Full.Time.Teachers+1)
#data$Pupil.Teacher.Ratio.log <- log(data$Pupil.Teacher.Ratio+1)
#data$Total.General.Revenue.log <- log(data$Total.General.Revenue+1)
#data$Total.General.Revenue.Per.Student.log <- log(data$Total.General.Revenue.Per.Student+1)


summary(Total.counties$n)
Total.counties$Total.General.Revenue.Per.School <- Total.counties$Total.General.Revenue / Total.counties$n
summary(Total.counties$Total.General.Revenue.Per.School)
Total.counties$Total.General.Revenue.Per.School.log <- log(Total.counties$Total.General.Revenue.Per.School+1)

ggplot() + geom_polygon(data=Total.counties, aes(x=long, y=lat, group = group, fill=Total.counties$ALL_RATE),colour="white") + scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar")
ggplot() + geom_polygon(data=Total.counties, aes(x=long, y=lat, group = group, fill=Total.counties$Full.Time.Teachers.log),colour="white") + scale_fill_continuous(low = "thistle2", high = "darkblue", guide="colorbar")
#temp2 <- subset(temp, Full.Time.Teachers < quantile(temp$Full.Time.Teachers, .90, na.rm=TRUE))
#ggplot() + geom_polygon(data=temp2, aes(x=long, y=lat, group = group, fill=temp2$Full.Time.Teachers),colour="white") + scale_fill_continuous(low = "thistle2", high = "darkblue", guide="colorbar")
ggplot() + geom_polygon(data=Total.counties, aes(x=long, y=lat, group = group, fill=Total.counties$Pupil.Teacher.Ratio),colour="white") + scale_fill_continuous(low = "thistle2", high = "purple", guide="colorbar", trans="log")
ggplot() + geom_polygon(data=Total.counties, aes(x=long, y=lat, group = group, fill=Total.counties$Total.General.Revenue.log),colour="white") + scale_fill_continuous(low = "thistle2", high = "darkgreen", guide="colorbar")
ggplot() + geom_polygon(data=Total.counties, aes(x=long, y=lat, group = group, fill=Total.counties$Total.General.Revenue.Per.School.log),colour="white") + scale_fill_continuous(low = "thistle2", high = "darkgreen", guide="colorbar")
ggplot() + geom_polygon(data=Total.counties, aes(x=long, y=lat, group = group, fill=Total.counties$Total.General.Revenue.Per.Student),colour="white") + scale_fill_continuous(low = "thistle2", high = "darkgreen", guide="colorbar", trans="log")

p1 <- ggplot() + geom_polygon(data=Total.counties, aes(x=long, y=lat, group = group, fill=Total.counties$ALL_RATE),colour="white") + scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar")
p2 <- ggplot() + geom_polygon(data=Total.counties, aes(x=long, y=lat, group = group, fill=Total.counties$Full.Time.Teachers.log),colour="white") + scale_fill_continuous(low = "thistle2", high = "darkblue", guide="colorbar")
p3 <- ggplot() + geom_polygon(data=Total.counties, aes(x=long, y=lat, group = group, fill=Total.counties$Pupil.Teacher.Ratio),colour="white") + scale_fill_continuous(low = "thistle2", high = "purple", guide="colorbar", trans="log")
p4 <- ggplot() + geom_polygon(data=Total.counties, aes(x=long, y=lat, group = group, fill=Total.counties$Total.General.Revenue.log),colour="white") + scale_fill_continuous(low = "thistle2", high = "darkgreen", guide="colorbar")
p5 <- ggplot() + geom_polygon(data=Total.counties, aes(x=long, y=lat, group = group, fill=Total.counties$Total.General.Revenue.Per.School.log),colour="white") + scale_fill_continuous(low = "thistle2", high = "darkgreen", guide="colorbar")
p6 <- ggplot() + geom_polygon(data=Total.counties, aes(x=long, y=lat, group = group, fill=Total.counties$Total.General.Revenue.Per.Student),colour="white") + scale_fill_continuous(low = "thistle2", high = "darkgreen", guide="colorbar", trans="log")
p7 <- ggplot() + geom_polygon(data=Total.counties, aes(x=long, y=lat, group = group, fill=Total.counties$Total.General.Revenue.Per.Student),colour="white") + scale_fill_continuous(low = "thistle2", high = "darkgreen", guide="colorbar")

grid.arrange(p1, p2, p3, p4)
grid.arrange(p4, p5)
grid.arrange(p1, p2, p3, p5)
grid.arrange(p5, p6)
grid.arrange(p1, p6)
grid.arrange(p1, p7)

#summary(temp$Full.Time.Teachers)
#summary(data$Full.Time.Teachers)
summary(data$Total.General.Revenue.Per.Student)
str(Total.counties)

cor(select(Total.counties, ALL_RATE, Full.Time.Teachers.log, Pupil.Teacher.Ratio.log, Total.General.Revenue.log), use = "na.or.complete")
cor(select(Total.counties, ALL_RATE, Full.Time.Teachers, Pupil.Teacher.Ratio, Total.General.Revenue), use = "na.or.complete")
cor(select(Total.counties, ALL_RATE, Full.Time.Teachers, Pupil.Teacher.Ratio, Total.General.Revenue.Per.School, n), use = "na.or.complete")
cor(select(Total.counties, ALL_RATE, Full.Time.Teachers.log, Pupil.Teacher.Ratio.log, Total.General.Revenue.Per.School.log), use = "na.or.complete")
cor(select(data, ALL_RATE, ALL_COHORT, Full.Time.Teachers, Pupil.Teacher.Ratio, Total.General.Revenue.Per.Student), use = "na.or.complete", method="spearman")
#cor(select(data, ALL_RATE, Full.Time.Teachers.log, Pupil.Teacher.Ratio.log, Total.General.Revenue.Per.Student.log), use = "na.or.complete", method="spearman")

ggplot(aes(x=ALL_COHORT, y=ALL_RATE), data=data) + 
  geom_point(alpha=0.05)

ggplot(aes(x=Pupil.Teacher.Ratio, y=ALL_RATE), data=data) + 
  geom_point(alpha=0.10) 

ggplot(aes(x=Total.General.Revenue.Per.Student, y=ALL_RATE), data=data) + 
  geom_point(alpha=0.10)

ggplot(aes(x=Total.General.Revenue.Per.Student, y=ALL_RATE), data=data) +
  geom_point(alpha=0.10) +
  xlim(0, quantile(data$Total.General.Revenue.Per.Student, .99, na.rm=TRUE))

ggplot(aes(x=Total.General.Revenue.Per.Student, y=ALL_COHORT), data=data) + 
  geom_point(alpha=0.10)




#ggplot(aes(x=Total.General.Revenue, y=ALL_RATE), data=data) + 
#  geom_point(alpha=0.10) 

#summary(data$Total.General.Revenue)
#qplot(data$Total.General.Revenue) + scale_x_log10()

#ggplot(aes(x=Total.General.Revenue+1, y=ALL_RATE), data=data) + 
#  geom_point(alpha=0.10) + 
#  scale_x_log10(limits=c(100000, 10000000000)) +
#  geom_smooth(color="red")

#ggplot(aes(x=(Total.General.Revenue+1)/ALL_COHORT, y=ALL_RATE), data=data) + 
#  geom_point(alpha=0.10) + 
#  scale_x_log10(limits=c(100000, 10000000000)) +
#  geom_smooth(color="red")

#ggplot(aes(x=Pupil.Teacher.Ratio, y=ALL_RATE), data=data) + 
#  geom_point(alpha=0.10) + 
#  xlim(0, 200) +
#  geom_smooth(method="lm", color="red")

#ggplot(aes(x=Pupil.Teacher.Ratio, y=ALL_RATE), data=data) + 
#  geom_point(alpha=0.10) + 
#  geom_smooth(method="lm", color="red")
ggplot(aes(x=ALL_COHORT, y=ALL_RATE), data=data) + 
  geom_point(alpha=0.10) + 
  xlim(0, quantile(data$ALL_COHORT, .99, na.rm=TRUE)) +
  geom_smooth(method="lm", color="red")

ggplot(aes(x=ALL_COHORT, y=ALL_RATE), data=data) + 
  geom_point(alpha=0.10) + 
  xlim(0, quantile(data$ALL_COHORT, .99, na.rm=TRUE)) +
  geom_smooth(color="red")

ggplot(aes(x=Pupil.Teacher.Ratio, y=ALL_RATE), data=data) + 
  geom_point(alpha=0.10) + 
  xlim(0, 75) +
  geom_smooth(method="lm", color="red")

ggplot(aes(x=Pupil.Teacher.Ratio, y=ALL_RATE), data=data) + 
  geom_point(alpha=0.10) + 
  xlim(0, 75) +
  geom_smooth(color="red")

ggplot(aes(x=Total.General.Revenue.Per.Student, y=ALL_RATE), data=data) +
  geom_point(alpha=0.10) + 
  xlim(0, quantile(data$Total.General.Revenue.Per.Student, .99, na.rm=TRUE)) +
  geom_smooth(method="lm", color="red")

ggplot(aes(x=Total.General.Revenue.Per.Student, y=ALL_RATE), data=data) +
  geom_point(alpha=0.10) + 
  xlim(0, quantile(data$Total.General.Revenue.Per.Student, .99, na.rm=TRUE)) +
  geom_smooth(color="red")

#Multivariate Analysis

ggplot(aes(x=ALL_COHORT, y=ALL_RATE), data=data) + 
  geom_point(aes(color=Total.General.Revenue.Per.Student), alpha=0.05) + 
  xlim(0, 1200)

ggplot(aes(x=Pupil.Teacher.Ratio, y=ALL_RATE), data=data) + 
  geom_point(aes(color=Total.General.Revenue.Per.Student), alpha=0.05) +
  xlim(0, 40)

ggplot(aes(x=Pupil.Teacher.Ratio, y=ALL_RATE), data=data) + 
  geom_point(aes(size=ALL_COHORT), alpha=0.05) + 
  xlim(0, 40)

ggplot(aes(x=Pupil.Teacher.Ratio, y=ALL_RATE), data=data) + 
  geom_point(aes(size=Total.General.Revenue.Per.Student, color=region), alpha=0.15) + 
  xlim(0, 40) + theme(legend.position="none")

print.data.frame(
  data %>% group_by(region) %>% 
    summarize(rev.per.student=mean(Total.General.Revenue.Per.Student, na.rm=TRUE)) %>% 
    arrange(desc(rev.per.student))
)

top1 <- ggplot(aes(x=Pupil.Teacher.Ratio, y=ALL_RATE), data=data) + 
  geom_point(aes(size=Total.General.Revenue.Per.Student, color=(region=="DISTRICT OF COLUMBIA"), alpha=ifelse(region=="DISTRICT OF COLUMBIA", 1, 0.10))) + 
  xlim(0, 75) + ggtitle("DC")

top2 <- ggplot(aes(x=Pupil.Teacher.Ratio, y=ALL_RATE), data=data) + 
  geom_point(aes(size=Total.General.Revenue.Per.Student, color=(region=="ALASKA"), alpha=ifelse(region=="ALASKA", 1, 0.10))) + 
  xlim(0, 75) + ggtitle("ALASKA")

top3 <- ggplot(aes(x=Pupil.Teacher.Ratio, y=ALL_RATE), data=data) + 
  geom_point(aes(size=Total.General.Revenue.Per.Student, color=(region=="NEW YORK"), alpha=ifelse(region=="NEW YORK", 1, 0.10))) + 
  xlim(0, 75) + ggtitle("NEW YORK")

top4 <- ggplot(aes(x=Pupil.Teacher.Ratio, y=ALL_RATE), data=data) + 
  geom_point(aes(size=Total.General.Revenue.Per.Student, color=(region=="NEW JERSEY"), alpha=ifelse(region=="NEW JERSEY", 1, 0.10))) + 
  xlim(0, 75) + ggtitle("NEW JERSEY")

top5 <- ggplot(aes(x=Pupil.Teacher.Ratio, y=ALL_RATE), data=data) + 
  geom_point(aes(size=Total.General.Revenue.Per.Student, color=(region=="WYOMING"), alpha=ifelse(region=="WYOMING", 1, 0.10))) + 
  xlim(0, 75) + ggtitle("WYOMING")

top6 <- ggplot(aes(x=Pupil.Teacher.Ratio, y=ALL_RATE), data=data) + 
  geom_point(aes(size=Total.General.Revenue.Per.Student, color=(region=="CONNECTICUT"), alpha=ifelse(region=="CONNECTICUT", 1, 0.10))) + 
  xlim(0, 75) + ggtitle("CONNECTICUT")

top7 <- ggplot(aes(x=Pupil.Teacher.Ratio, y=ALL_RATE), data=data) + 
  geom_point(aes(size=Total.General.Revenue.Per.Student, color=(region=="VERMONT"), alpha=ifelse(region=="VERMONT", 1, 0.10))) + 
  xlim(0, 75) + ggtitle("VERMONT")

top8 <- ggplot(aes(x=Pupil.Teacher.Ratio, y=ALL_RATE), data=data) + 
  geom_point(aes(size=Total.General.Revenue.Per.Student, color=(region=="MONTANA"), alpha=ifelse(region=="MONTANA", 1, 0.10))) + 
  xlim(0, 75) + ggtitle("MONTANA")

top9 <- ggplot(aes(x=Pupil.Teacher.Ratio, y=ALL_RATE), data=data) + 
  geom_point(aes(size=Total.General.Revenue.Per.Student, color=(region=="MASSACHUSETTS"), alpha=ifelse(region=="MASSACHUSETTS", 1, 0.10))) + 
  xlim(0, 75) + ggtitle("MASSACHUSETTS")

top10 <- ggplot(aes(x=Pupil.Teacher.Ratio, y=ALL_RATE), data=data) + 
  geom_point(aes(size=Total.General.Revenue.Per.Student, color=(region=="NEW HAMPSHIRE"), alpha=ifelse(region=="NEW HAMPSHIRE", 1, 0.10))) + 
  xlim(0, 75) + ggtitle("NEW HAMPSHIRE")

grid.arrange(top1, top2, top3, top4, top5, top6, top7, top8, top9, top10, ncol=2)

bottom1 <- ggplot(aes(x=Pupil.Teacher.Ratio, y=ALL_RATE), data=data) + 
  geom_point(aes(size=Total.General.Revenue.Per.Student, color=(region=="NEVADA"), alpha=ifelse(region=="NEVADA", 1, 0.10))) + 
  xlim(0, 75)

bottom2 <- ggplot(aes(x=Pupil.Teacher.Ratio, y=ALL_RATE), data=data) + 
  geom_point(aes(size=Total.General.Revenue.Per.Student, color=(region=="GEORGIA"), alpha=ifelse(region=="GEORGIA", 1, 0.10))) + 
  xlim(0, 75)

bottom3 <- ggplot(aes(x=Pupil.Teacher.Ratio, y=ALL_RATE), data=data) + 
  geom_point(aes(size=Total.General.Revenue.Per.Student, color=(region=="ARKANSAS"), alpha=ifelse(region=="ARKANSAS", 1, 0.10))) + 
  xlim(0, 75)

bottom4 <- ggplot(aes(x=Pupil.Teacher.Ratio, y=ALL_RATE), data=data) + 
  geom_point(aes(size=Total.General.Revenue.Per.Student, color=(region=="ARIZONA"), alpha=ifelse(region=="ARIZONA", 1, 0.10))) + 
  xlim(0, 75)

bottom5 <- ggplot(aes(x=Pupil.Teacher.Ratio, y=ALL_RATE), data=data) + 
  geom_point(aes(size=Total.General.Revenue.Per.Student, color=(region=="ALABAMA"), alpha=ifelse(region=="ALABAMA", 1, 0.10))) + 
  xlim(0, 75)

bottom6 <- ggplot(aes(x=Pupil.Teacher.Ratio, y=ALL_RATE), data=data) + 
  geom_point(aes(size=Total.General.Revenue.Per.Student, color=(region=="MISSISSIPPI"), alpha=ifelse(region=="MISSISSIPPI", 1, 0.10))) + 
  xlim(0, 75)

bottom7 <- ggplot(aes(x=Pupil.Teacher.Ratio, y=ALL_RATE), data=data) + 
  geom_point(aes(size=Total.General.Revenue.Per.Student, color=(region=="TENNESSEE"), alpha=ifelse(region=="TENNESSEE", 1, 0.10))) + 
  xlim(0, 75)

bottom8 <- ggplot(aes(x=Pupil.Teacher.Ratio, y=ALL_RATE), data=data) + 
  geom_point(aes(size=Total.General.Revenue.Per.Student, color=(region=="FLORIDA"), alpha=ifelse(region=="FLORIDA", 1, 0.10))) + 
  xlim(0, 75)

bottom9 <- ggplot(aes(x=Pupil.Teacher.Ratio, y=ALL_RATE), data=data) + 
  geom_point(aes(size=Total.General.Revenue.Per.Student, color=(region=="NORTH CAROLINA"), alpha=ifelse(region=="NORTH CAROLINA", 1, 0.10))) + 
  xlim(0, 75)

bottom10 <- ggplot(aes(x=Pupil.Teacher.Ratio, y=ALL_RATE), data=data) + 
  geom_point(aes(size=Total.General.Revenue.Per.Student, color=(region=="UTAH"), alpha=ifelse(region=="UTAH", 1, 0.10))) + 
  xlim(0, 75)

data %>% filter(region=="TENNESSEE") %>% arrange(desc(Total.General.Revenue.Per.Student)) %>% head(1)


print.data.frame(
  data %>% group_by(region) %>% 
    summarize(grad.rate=mean(ALL_RATE, na.rm=TRUE)) %>% 
    arrange(desc(grad.rate))
)

#ggplot(aes(x=Pupil.Teacher.Ratio, y=ALL_RATE), data=data) + 
#  geom_point(aes(size=Total.General.Revenue.Per.Student, color=(Total.General.Revenue.Per.Student>1000000), alpha=ifelse(Total.General.Revenue.Per.Student>1000000, 1, 0.10))) + 
#  xlim(0, 75)


summary(data$Total.General.Revenue.Per.Student)

#ggplot(aes(x=Pupil.Teacher.Ratio, y=ALL_RATE), data=data) + 
#  geom_point(aes(size=Total.General.Revenue.Per.Student), alpha=0.15) + 
#  xlim(0, 75)

#ggplot(aes(x=Pupil.Teacher.Ratio, y=ALL_RATE), data=data) + 
#  geom_point(aes(size=log(ALL_COHORT), color=region), alpha=0.15) + 
#  xlim(0, 75)

#growth1994 <- growth %>% filter(Y == "X1994")
#growth2004 <- growth %>% filter(Y == "X2004")
revRanks <- data %>% group_by(Postal.Code) %>% 
  summarize(rev.per.student=mean(Total.General.Revenue.Per.Student, na.rm=TRUE)) %>% 
  arrange(desc(rev.per.student)) %>% 
  mutate(rev.per.student.rank=row_number())
revRanks <- revRanks %>% mutate(rev.per.student.rank=dim(revRanks)[1]-rev.per.student.rank)

sizeRanks <- data %>% group_by(Postal.Code) %>% 
  summarize(size=mean(ALL_COHORT, na.rm=TRUE)) %>% 
  arrange(desc(size)) %>% 
  mutate(size.rank=row_number())
sizeRanks <- sizeRanks %>% mutate(size.rank=dim(sizeRanks)[1]-size.rank)

gradRanks <- data %>% group_by(Postal.Code) %>% 
  summarize(grad.rate=mean(ALL_RATE, na.rm=TRUE)) %>% 
  arrange(desc(grad.rate)) %>% 
  mutate(grad.rate.rank=row_number())
gradRanks <- gradRanks %>% mutate(grad.rate.rank=dim(gradRanks)[1]-grad.rate.rank)


ranks <- inner_join(revRanks, gradRanks, by="Postal.Code")
ranks

ranks2 <- inner_join(sizeRanks, gradRanks, by="Postal.Code")
ranks2

#growth1994$n1994 <- growth1994$n
#growth2004$n2004 <- growth2004$n
#growth1994and2004 <- select(growth1994, -n, -Y, -records) %>% inner_join(select(growth2004, -n, -Y, -records), by = "X")
p<-ggplot(ranks) + geom_segment(aes(x=0,xend=1,y=rev.per.student.rank,yend=grad.rate.rank),size=.75) + scale_x_continuous(breaks = c())
p<-p + theme(panel.background = element_blank())
p<-p + theme(panel.grid=element_blank())
p<-p + theme(axis.ticks=element_blank())
p<-p + theme(axis.text=element_blank())
p<-p + theme(panel.border=element_blank())                                                                                
p<-p + geom_text(label=ranks$Postal.Code, y=ranks$grad.rate.rank, x=1,hjust=-0.2,size=3.5)
p<-p + geom_text(label=ranks$Postal.Code, y=ranks$rev.per.student.rank, x=0,hjust=1.2,size=3.5)
p<-p + geom_text(label="Revenue Per Student Ranks", x=0, y=(1.05*(max(ranks$grad.rate.rank,ranks$rev.per.student.rank))),hjust= 1.2,size=3.5)
p<-p + geom_text(label="Graduation Rate Ranks", x=1, y=(1.05*(max(ranks$grad.rate.rank,ranks$rev.per.student.rank))),hjust=-0.1,size=3.5)
p

number.of.ranks <- 10
top.and.bottom.ranks <- filter(ranks, rev.per.student.rank < number.of.ranks | rev.per.student.rank >= dim(ranks)[1] - number.of.ranks)
p<-ggplot(top.and.bottom.ranks) + geom_segment(aes(x=0,xend=1,y=rev.per.student.rank,yend=grad.rate.rank),size=.75) + scale_x_continuous(breaks = c())
p<-p + theme(panel.background = element_blank())
p<-p + theme(panel.grid=element_blank())
p<-p + theme(axis.ticks=element_blank())
p<-p + theme(axis.text=element_blank())
p<-p + theme(panel.border=element_blank())                                                                                
p<-p + geom_text(label=top.and.bottom.ranks$Postal.Code, y=top.and.bottom.ranks$grad.rate.rank, x=1,hjust=-0.2,size=3.5)
p<-p + geom_text(label=top.and.bottom.ranks$Postal.Code, y=top.and.bottom.ranks$rev.per.student.rank, x=0,hjust=1.2,size=3.5)
p<-p + geom_text(label="Revenue Per Student Ranks", x=0, y=(1.05*(max(top.and.bottom.ranks$grad.rate.rank,top.and.bottom.ranks$rev.per.student.rank))),hjust= 1.2,size=3.5)
p<-p + geom_text(label="Graduation Rate Ranks", x=1, y=(1.05*(max(top.and.bottom.ranks$grad.rate.rank,top.and.bottom.ranks$rev.per.student.rank))),hjust=-0.1,size=3.5)
p

top.and.bottom.ranks <- filter(ranks2, size.rank < number.of.ranks | size.rank >= dim(ranks2)[1] - number.of.ranks)
p<-ggplot(top.and.bottom.ranks) + geom_segment(aes(x=0,xend=1,y=size.rank,yend=grad.rate.rank),size=.75) + scale_x_continuous(breaks = c())
p<-p + theme(panel.background = element_blank())
p<-p + theme(panel.grid=element_blank())
p<-p + theme(axis.ticks=element_blank())
p<-p + theme(axis.text=element_blank())
p<-p + theme(panel.border=element_blank())                                                                                
p<-p + geom_text(label=top.and.bottom.ranks$Postal.Code, y=top.and.bottom.ranks$grad.rate.rank, x=1,hjust=-0.2,size=3.5)
p<-p + geom_text(label=top.and.bottom.ranks$Postal.Code, y=top.and.bottom.ranks$size.rank, x=0,hjust=1.2,size=3.5)
p<-p + geom_text(label="Revenue Per Student Ranks", x=0, y=(1.05*(max(top.and.bottom.ranks$grad.rate.rank,top.and.bottom.ranks$size.rank))),hjust= 1.2,size=3.5)
p<-p + geom_text(label="Graduation Rate Ranks", x=1, y=(1.05*(max(top.and.bottom.ranks$grad.rate.rank,top.and.bottom.ranks$size.rank))),hjust=-0.1,size=3.5)
p



#The need for quantile here made be look at the ALL_COHORT boxplot for each region, which is
#now at the top of this file since it shows the need for removing the extreme outliers which 
#affects many preceding graphs
#ggplot(aes(x=Pupil.Teacher.Ratio, y=ALL_RATE), data=subset(data, ALL_COHORT < quantile(ALL_COHORT, .999))) + 
#  geom_point(aes(size=ALL_COHORT, color=(region=="CALIFORNIA")), alpha=0.15) + 
#  xlim(0, 75)

#ggplot(aes(x=Pupil.Teacher.Ratio, y=ALL_RATE), data=subset(data, Total.General.Revenue/ALL_COHORT > 10000000)) + 
#  geom_point(aes(size=Total.General.Revenue/ALL_COHORT, color=(region=="NEVADA")), alpha=1) + 
#  xlim(0, 75)

#ggplot(aes(x=Pupil.Teacher.Ratio, y=ALL_RATE), data=data) + 
#  geom_point(aes(size=Total.General.Revenue, color=(region=="FLORIDA")), alpha=0.15) + 
#  xlim(0, 75)

#ggplot(aes(x=Pupil.Teacher.Ratio, y=ALL_RATE), data=data) + 
#  geom_point(aes(size=Total.General.Revenue, color=(region=="ILLINOIS")), alpha=0.15) + 
#  xlim(0, 75)
