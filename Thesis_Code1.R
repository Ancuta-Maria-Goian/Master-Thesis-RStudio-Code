

#-------------- DATA PREPARATION and ANALYSES for MAE4191-------------




#------------------ PACKAGES-----------------
#loading packages          #Purpose of the package

library(haven) #            read_sav() to import .sav files
library(dplyr) #            data wrangling: filter(), mutate(), group_by(), summarise(), etc.
library(tidyr) #            pivot_longer(), pivot_wider() in descriptive statistics
library(stringr) #          str_detect(), gsub() pattern matching 
library(igraph) #           network construction and features (graph_from_data_frame(), degree(), etc.)
library(moments) #          skewness(), kurtosis() in descriptive statistics
library(knitr) #            kable() for table rendering
library(factoextra) #       get_clust_tendency() for Hopkins statistic
library(mclust) #           Mclust(), mclustICL(), mclustBootstrapLRT() for Gaussian Mixture Models
library(mirt)   #           function averageMI() -agregate results from multiple imputat
library(purrr) #            Function reduce() combines elements of a list (full_join)
library(scales) #           rescale() for edges plotting in the clusters

        

#----------------------- DATA ------------------

 #  .......setting the working directory.........
setwd("C:/Users/ancam/Desktop/master thesis/R scripts and data")



 #importing dataset from the log-files databases 
         #it is only for the CP007Q02 item 2  (from the traffic task)
  allcountries <- read_sav("CBA_cp007q02_logs12_SPSS.sav") 
  
  #data for the scored item
  scores<-read_sav("Scored_item.sav")
  
  #data for plausible values
  plausible_values <- read_sav("P_values.sav")
  
  
  
#--------------DATA PREPARATION-----------------------

#............LOG FILES INSPECTION and WRAGLING.........
  
  #subset to keep only the three countries we are interested in  (Singapore, Norway, Colombia)
  data1 <- allcountries[allcountries$cnt %in% c("SGP", "NOR", "COL"), ]
  data<-as.data.frame(data1) #transforming to dataframe

  
  # missing values for each column
 (NAs_values <- colSums(is.na(data))) 
  #unique countries in the cnt column
  (unique_countries <- length(unique(data$cnt)))
  #number of students per country
 (stIDs_per_country <- tapply(data$StIDStd, data$cnt, function(x) length(unique(x))))

  #inspecting the identifying metadata (country, school, and student ID)
  length(unique(data$StIDStd)) #unique stIDs in the dataset
  nrow(distinct(data[,1:3])) # unique combinations of  cnt, schoolid, and StIDStd
  nrow(distinct(data[,2:3])) # unique combination of schoolid and StIDStd
  
  
  #missing or empty values for country, school, and studentID
  (missing_cnt <- sum(is.na(data$cnt) | data$cnt == ""))
  (missing_schoolid <- sum(is.na(data$schoolid) | data$schoolid == ""))
  (missing_StIDStd <- sum(is.na(data$StIDStd) | data$StIDStd == ""))
  
  #creating a unique ID by concatenating country, school ID, and student ID
  data$ID <- paste(data$cnt, data$schoolid, data$StIDStd, sep = "_")
  
  # Removing rows with missing or empty values in cnt, schoolid, or StIDStd
  data2 <- data[!(is.na(data$cnt) | data$cnt == "" |
                    is.na(data$schoolid) | data$schoolid == "" |
                    is.na(data$StIDStd) | data$StIDStd == ""), ] # IDs per country left (COL=755, NOR=416, SGP=461)
  #ordering columns so that ID column is first
  data2 <- data2[,c(8, 1:7)]
  # inspect sample size x country after metadata cleaning
  (stIDs_per_country <- tapply(data2$ID, data2$cnt, function(x) length(unique(x))))
  
  
  # summarizing the 'time' column
  summary(data2$time) 

  
  # inspecting the unique values in the event column
  unique(data2$event) 
  
  
  # inspecting the unique values from the 'event_value' column
  uniques <- unique(data2$event_value)
  # converting the unique values into a data frame
  unique_event_values <- data.frame(event_value = uniques)
  # creating two columns: one for words and one for binary data (to have a better look at unique codes for events)
  unique_events <- unique_event_values %>%
    mutate(
      binary = ifelse(grepl("^'[01]+$", event_value), event_value, NA),  # Identify binary data (01000...type string)
      words = ifelse(!grepl("^'[01]+$", event_value), event_value, NA)   # Identify data written in words (hit_DiamondSilver type)
    ) %>%
    select(words, binary)  
 
  # to see better the unique values coded in event_value column, we remove NAs from the words column
   unique_events_words <- unique_events[!is.na(unique_events$words), ]
   #View(unique_events_words)
 
   
   
   # verifying if event_number is strictly increasing for each student ID
   increasing_check <- data2 %>%
     group_by(ID) %>%
     summarise(is_increasing = all(event_number == row_number()))
   any(increasing_check$is_increasing== FALSE) # all students have event_number in increasing order
   
  
   

    #.............INSPECT and TIDY LOG FILES................
   
   
   #Removing students who restarted the item
   
   start_count <- data2 %>%
     filter(event == "START_ITEM") %>%
     group_by(ID) %>%          
     summarise(start_n = n(), .groups = "drop")  %>% # number of "START_ITEM" events per student
     as.data.frame()
   start_count[start_count$start_n > 1,] # 6 students (5 COL;1 NOR)
   #removing the 6 studentd after individual inspection
   data2 <- data2 %>% filter(!ID %in% start_count[start_count$start_n > 1,"ID"])
   #length(unique(data2$ID))   # 1626  COL= 750, NOR= 415, SGP = 461
   
   #inspecting if students have registered more than one END event?
   end_count <- data2 %>%
     filter(event == "END_ITEM") %>%
     group_by(ID) %>%
     summarise(end_n = n(), .groups = "drop")
   end_count[end_count$end_n > 1, ] # all students have just 1 END event
   
   #ispecting students missing the START or END of the item in the 'event' column
   missing_start <- data2 %>%
     group_by(ID) %>%
     summarise(has_start = any(event == "START_ITEM")) %>%
     filter(!has_start)
   
   missing_end <- data2 %>%
     group_by(ID) %>%
     summarise(has_end = any(event == "END_ITEM")) %>%
     filter(!has_end)
   nrow(missing_start)# 0
   nrow(missing_end) #No student has missing start or end event
  
   # checking if first event is START and last event is END for each student
   first_last_check <- data2 %>%
     group_by(ID) %>%
     summarize(
       first_event = first(event),
       last_event = last(event),
       is_first_start = first_event == "START_ITEM",
       is_last_end = last_event == "END_ITEM",
       proper_sequence = is_first_start & is_last_end,
       .groups = "drop"
     )
   
   start_end <- first_last_check %>%
     summarize(
       total_students = n(),
       students_with_correct_start = sum(is_first_start),
       students_with_correct_end = sum(is_last_end),
       students_with_proper_sequence = sum(proper_sequence)
     )
   print(start_end)
   
   # finding students with incorrect sequence just to double check
   students_incorrect <- first_last_check %>%
     filter(!proper_sequence) %>%
     select(ID, first_event, last_event, is_first_start, is_last_end)
   print(students_incorrect) # 3 students 1 COL, 2 NOR are having isssues with end_item
             # 2 students from norway have the entire row of end_item in an incorrect secquence
            # 1 students from Colombia registered a dblclick after end_item. this dblclick will be removed later
   
   
   # creating a function to fix END_ITEM issues for one student
   fix_end_item <- function(df, student_id) {
     student_data <- df %>% filter(ID == student_id)
     
     # removing all END_ITEMs and retain only the last one
     non_end <- student_data %>% filter(event != "END_ITEM")
     end_row <- student_data %>%
       filter(event == "END_ITEM") %>% #select all the END_ITEM rows
       slice_tail(n = 1) # keep the last row of end_item
     
     # recombining and reindexing
     updated <- bind_rows(non_end, end_row) %>%
       arrange(time) %>%
       mutate(event_number = row_number())  
     return(updated)
   }
   
   # applying the function to the two IDs from NOR
   data2 <- data2 %>%
     filter(!(ID %in% c("NOR_0000135_03244", "NOR_0000139_03366"))) %>%
     bind_rows(
       fix_end_item(data2, "NOR_0000135_03244"),
       fix_end_item(data2, "NOR_0000139_03366")
     )
   
   
 
   
   
 #removing ACER_EVENT (0-1 string) rows
   data2<- data2[data2$event != "ACER_EVENT", ]

   

   
#  identifying all (ID, time, event_value) combos that involve a dblclick
   dblclick_combos <- data2 %>%
     filter(event == "dblclick") %>%
     select(ID, time, event_value) %>%
     distinct()
   
# removing both dblclicks AND any clicks with the same ID, time, and event_value
   data2 <- data2 %>%
     anti_join(
       dblclick_combos %>% mutate(event = "dblclick"), by = c("ID", "time", "event_value", "event")
     ) %>%
     anti_join(
       dblclick_combos %>% mutate(event = "click"), by = c("ID", "time", "event_value", "event")
     )
   
  
# identifying if any duplicate (ID, time) pairs left
   same_time <- data2 %>%
     group_by(ID, time) %>%
     filter(n() > 1) %>%
     arrange(ID, time) %>%
     ungroup() # we will hadle these cases in the following sections
               # If the time code is confusing and demonstrates potential software errors we remove cases completely
 
  
   
#removing 	IDs from data2 that have confusing time coding
   #->  COL_0000031_00757 --- this has many rows with 0 as a value for time- indicates a software error
   data2 <- data2 %>%
     filter(ID != "COL_0000031_00757")
   
#Some students have the same event and time stamp duplicated or appearing multiple times (we keep one row of those duplicates)
   #there are 36 distict combinations of duplicated rows (id, time, event and event_value columns)
       #Some others have same time stamp and different event recorded 
   
#removing duplicated time+ event_value rows to keep only one instance
   data2 <- data2 %>%
     distinct(ID, time, event_value, .keep_all = TRUE)

# finding IDs with duplicated time but differing event_values to have an inventory
   conflict_ids <- data2 %>%
     group_by(ID, time) %>%
     summarise(n_unique = n_distinct(event_value), .groups = "drop") %>%
     filter(n_unique > 1) %>%
     pull(ID) %>%
     unique()
   print(conflict_ids) # 30 students ( 3 SGP, 3 COL, 24 NOR)
   
   
   #creating a new column to store renamed event values
   data2$events <- data2$event_value
   #  renaming specific event_values based on the 'event' column
   data2$events[data2$event == "START_ITEM"] <- "start"
   data2$events[data2$event == "END_ITEM"] <- "end"
   data2$events[data2$event_value == "resetButton"] <- "rs"
   
   # renaming remaining event values based on conditions if:
                                    # action hit_ keep as it is
                                    #action start, end, rs, keep as it is
                                    # other actions are insignificant "is"
   data2$events <- with(data2, ifelse(
     grepl("^hit", event_value),  # If event_value starts with "hit", keep it as is
     event_value, 
     ifelse(events %in% c("start", "end", "rs"), 
            events,  # Keep already renamed values
            "is"  # Otherwise, classify as "is"
     )
   ))
   
   
  #collapsing the repeated "is-insignificant" operations to keep only the last one 
   data3 <- data2 %>%
     arrange(ID, event_number) %>%
     mutate(
       is_group = cumsum(events != "is" | lag(ID, default = first(ID)) != ID)
     ) %>% # creating a grouping variable that increments when:
     #  event is NOT "is", OR
     #  student ID changes (so we don't group across students)
     filter(!(events == "is" & lead(is_group, default = max(is_group) + 1) == is_group)) %>% #removing "is" events but not the last one in each group of consecutive "is" entries
     select(-is_group)
   
   #collapsing the repeated "reset"events to keep only the last one
   data3 <- data3 %>%
     arrange(ID, event_number) %>%
     mutate(
       rs_group = cumsum(events != "rs" | lag(ID, default = first(ID)) != ID)
     ) %>%
     filter(!(events == "rs" & lead(rs_group, default = max(rs_group) + 1) == rs_group)) %>%
     select(-rs_group)
   

   #identifying if a "hit" operation represents selection or cancellation
   data3 <- data3 %>%
     group_by(ID) %>%
     mutate(
       # creating a flag that increments on each "start" or "rs" to restart counting
       start_reset_flag = cumsum(events %in% c("start", "rs"))
     ) %>%
     group_by(ID, start_reset_flag, events) %>%  # grouping by ID, reset boundary, and event
     mutate(
       occurrence = row_number(),  # counting hits on the same event within each reset boundary
       Operation_meaning = case_when(
         grepl("^hit", events) & occurrence %% 2 == 1 ~ paste0("Select_", gsub("hit_", "", events)),  # Selection
         grepl("^hit", events) & occurrence %% 2 == 0 ~ paste0("Cancel_", gsub("hit_", "", events)),  # Cancellation
         TRUE ~ events  # for other event types, retain the original event names
       )
     ) %>%
     ungroup() %>%
     select(-start_reset_flag, -occurrence)  # removing additional columns
   
   
   
# simplifying path names 
   # defining the mapping of paths
   path_mapping <- c(
     "Diamondnowhere",  # P1
     "nowhereSakharov", # P2
     "SakharovMarket",  # P3
     "MarketLee",       # P4
     "LeeMandela",      # P5
     "MandelaEinstein", # P6
     "SakharovNobel",   # P7
     "NobelLee",        # P8
     "DiamondSilver",   # P9
     "SilverMarket",    # P10
     "MarketPark",      # P11
     "ParkMandela",     # P12
     "Parknowhere",     # P13
     "nowhereEinstein", # P14
     "Silvernowhere",   # P15
     "nowhereUnity",    # P16
     "UnityPark",       # P17
     "nowhereEmerald",  # P18
     "EmeraldUnity",    # P19
     "UnitySato",       # P20
     "Satonowhere",     # P21
     "EmeraldLincoln",  # P22
     "LincolnSato"      # P23
   )
   
   # simplify path names in the Operation_meaning column
   data3 <- data3 %>%
     mutate(
       Operations = Operation_meaning,  # a copy of Operation_types
       Operations = case_when(
         str_detect(Operation_meaning, "^Select_") ~ {
           # for selections, replace path names with SP followed by their index (1,2,3...23)
           for (i in seq_along(path_mapping)) {
             Operations <- gsub(paste0("Select_", path_mapping[i]), paste0("SP", i), Operations)
           }
           Operations
         },
         str_detect(Operation_meaning, "^Cancel_") ~ {
           # for cancellations, replace path names with CP followed by their index
           for (i in seq_along(path_mapping)) {
             Operations <- gsub(paste0("Cancel_", path_mapping[i]), paste0("CP", i), Operations)
           }
           Operations
         },
         TRUE ~ Operations  # keeping the other events unchanged
       )
     )
   
 
  
   # creating the action_from and action_to columns
   data3 <- data3 %>%
     arrange(ID, event_number) %>%  # rows are sorted by StudentID and event sequence
     group_by(ID) %>% 
     mutate(
       action_from = case_when(
         events == "end" ~ NA_character_,  # removing end from action_from
         lag(events) == "end" ~ ifelse(events == "start", "start", NA_character_),  # start follows end
         TRUE ~ Operations  # otherwise keep Operations
       ),
       action_to = case_when(
         events == "end" ~ Operations,  # end is always in action_to
         TRUE ~ lead(Operations, default = NA)  # otherwise, NA
       )
     ) %>%
     ungroup()
   
   
   # finding students with non-sequential times because PISA mentions some instances where time is non sequential
   #this is useful before creating duration column
   non_sequential_times <- data3 %>%
     group_by(ID) %>%
     arrange(event_number, .by_group = TRUE) %>%
     mutate(time_diff = time - lag(time)) %>%
     filter(!is.na(time_diff) & time_diff < 0) %>%
     distinct(ID)
   
   print(non_sequential_times, n=Inf) #32 students( 6 COL, 6 SGP, 20 NOR)
   # we will handle these cases after computing duration
   
   
   
 # computing the duration column 
   data3 <- data3 %>%
     arrange(ID, event_number) %>%  #  rows are sorted by StudentID and event sequence
     group_by(ID) %>%  
     mutate(
       lag_time = lead(time, default = NA),  # getting the time of the next event
       duration = lag_time - time  # computing the duration 
     ) %>%
     ungroup() %>%  
     select(-lag_time)
   
# removing NAs from the action_from column, as well as from the duration column
   data4 <- data3 %>%
     group_by(ID) %>%
     filter(!(is.na(action_from) & action_to == "end")) %>%  # removing "NA -> end" cases
     filter(!(is.na(action_from) & is.na(action_to))) %>%    # removing complete NA transitions
     filter(!(action_from == action_to & !is.na(action_from))) %>%  # removing self-loops
     ungroup()
   
   
   
summary(data4$duration) # there are some negative durations given that 26 students have non sequential timing


#unique StudentIDs with negative duration give that timing was non-sequential
IDs_negative_duration <- data4 %>%
  filter(duration < 0) %>%
  pull(ID) %>%  
  unique()  
print(IDs_negative_duration) # 32 students, COL= 6, NOR = 20, SGP = 6
# extracting all rows for those IDs to check them individually
negative_duration_data <- data4 %>%
  filter(ID %in% IDs_negative_duration) 
# removing COL_0000345_08928 because the time coding seems in reverse order for many rows
data4 <- data4 %>%
  filter(ID != "COL_0000345_08928")

# removing rows that have negative duration
       # the reason being that negative duration is relatively small 
       # many of the students have typically under less than 1 second ( -0.5, -0.1 and so on)
data5 <- data4 %>%
  filter(duration >= 0)

 
# identifying rows with zero duration
zero_durations <- data5 %>%
  filter(duration == 0) %>%
  mutate(country = substr(ID, 1, 3)) %>%  # extracting country code (first 3 characters)
  group_by(country) %>%
  summarise(unique_students = n_distinct(ID)) %>%
  arrange(desc(unique_students))  # sorting descending
print(zero_durations)     # 29 students, COL= 2, NOR = 24, SGP = 3


#creating a dataset to inspect 0 durations indidually ( we know that there are dupliated times but different event_values rows)
zero_duration_IDs <- data5 %>%
  filter(duration == 0) %>%
  distinct(ID)  
zero_duration_data <- data5 %>%
  filter(ID %in% zero_duration_IDs$ID)


# removing rows with 0 duration data consistent with Zhang & Andersson (2023)
data5 <- data5 %>%
  filter(duration != 0)



#creating the category of operations column consistent with Zhang & Andersson (2023)
data5 <- data5 %>%
  mutate(
    category = case_when(
      action_to %in% c("SP1", "SP2", "SP3", "SP4", "SP5", "SP6") ~ 1,  # Correct Selection (C_S)
      action_to %in% c("CP7", "CP8", "CP9", "CP10", "CP11", "CP12", "CP13", "CP14", "CP15", "CP16", "CP17", "CP18", "CP19", "CP20", "CP21", "CP22", "CP23") ~ 2,  # Correct Cancellation (C_C)
      action_to %in% c("SP7", "SP8", "SP9", "SP10", "SP11", "SP12", "SP13", "SP14", "SP15", "SP16", "SP17", "SP18", "SP19", "SP20", "SP21", "SP22", "SP23", "is") ~ 3,  # Incorrect Selection (I_S) + Insignificant Operation
      action_to %in% c("CP1", "CP2", "CP3", "CP4", "CP5", "CP6") ~ 3,  # Incorrect Cancellation (I_C)
      action_to %in% c("start", "end", "rs") ~ 4,  # Start, End, or Rest
      TRUE ~ NA_real_  # Assign NA if no category is matched
    )
  )


#Creating an inventory of the operations by country
country_operations <- data5 %>%
  mutate(country = str_sub(ID, 1, 3)) %>%              # extracting first 3 letters as country code
  filter(Operation_meaning != "start") %>%            # excluding "start" as we ensure every students started once
  group_by(country, Operations, Operation_meaning) %>%
  summarise(Total_Frequency = n(), .groups = "drop") %>%
  arrange(country, desc(Total_Frequency)) %>%
  group_by(country) %>%
  slice_head(n = 50)                                   # displaying in decreasing order

# dislaying the resulting inventory
print(country_operations, n = Inf)






#--------------------EDGES DATASET-------------------------

# selecting required columns
edges <- data5 %>%
  select(action_from, action_to, category, duration, ID) %>% 
  rename(
    StIDStd = ID  # renaming ID  
  )

edges <- as.data.frame(edges)
#ordering columns for compatibility with later function use
edges <- edges[, c("action_from", "action_to", "category", "duration", "StIDStd")]



# identifying students with only one action: start → end
one_action <- edges %>%
  filter(action_from == "start", action_to == "end") %>%
  group_by(StIDStd) %>%
  filter(n() == 1) %>%
  ungroup()
print(one_action, n = Inf) #38 STDs 1 SGP, 5 NOR, 32 COL


#counting number of transitions per student
student_action_counts <- edges %>%
  group_by(StIDStd) %>%
  filter(!is.na(action_from) & !is.na(action_to)) %>%
  summarise(n_actions = n()) %>%
  filter(n_actions <= 2)

# pulling the matching rows for those students that have one action ( being that START-> end, or Start-action, action-end =2 rows)
min_students <- edges %>%
  filter(StIDStd %in% student_action_counts$StIDStd) %>%
  arrange(StIDStd, action_from)

min_student_ids <- student_action_counts$StIDStd
print(min_student_ids) # 110 students, COL = 93 , NOR = 11, SGP = 6

# deleting the 110 students who have performed one action - either 1 or 2 rows
edges <- edges %>%
  filter(!(StIDStd %in% min_student_ids))

#counting students per country
edges %>%
  mutate(country = substr(StIDStd, 1, 3)) %>%
  distinct(StIDStd, country) %>%
  count(country, name = "n_unique_students") # COL 655, NOR 404, SGP 455


# making sure each student has first action start in action_from, 
                          # last action end in action_to
start_end <- edges %>%
  group_by(StIDStd) %>%
  summarise(
    starting = first(action_from) == "start",
    ending = last(action_to) == "end",
    .groups = "drop"
  ) %>%
  mutate(is_valid = starting & ending)

# viewing students who do NOT satisfy the condition
start_end %>% filter(!is_valid) # all students have met the condition

#summary(edges$duration) # double checking duration 






#-----------------------NODES DATASET---------------------

# extracting unique operations from action_from and action_to
nodes <- edges %>%
  select(action_from) %>%
  rename(operation = action_from) %>%
  bind_rows(edges %>% select(action_to) %>% rename(operation = action_to)) %>%
  distinct(operation)  

# defining categories based on provided mappings in the supplemental material (Zhang & Andersson, 2023)
correct_selection <- paste0("SP", 1:6)  # SP1 - SP6
correct_cancellation <- paste0("CP", 7:23)  # CP7 - CP23
incorrect_selection <- paste0("SP", 7:23)  # SP7 - SP23
incorrect_cancellation <- paste0("CP", 1:6)  # CP1 - CP6

# assigning categories using case_when()
nodes <- nodes %>%
  mutate(
    category = case_when(
      operation %in% correct_selection ~ 1,  # Correct Selection (C_S)
      operation %in% correct_cancellation ~ 2,  # Correct Cancellation (C_C)
      operation %in% incorrect_selection ~ 3,  # Incorrect Selection (I_S)
      operation %in% incorrect_cancellation ~ 3,  # Incorrect Cancellation (I_C)
      operation %in% c("start", "end", "rs") ~ 4,  # start, end, or eeset
      operation == "is" ~ 3,  # insignificant Operation (is) is classified as 3 (Incorrect Selection)
      TRUE ~ NA_real_  # any operation not categorized will be set as NA
    )
  )

nodes<-as.data.frame(nodes)




 #------------------- EXAMINEES DATASET------------------------

 
  scores1<- as.data.frame(scores)
  
  # subsetting the data to keep only the country, ID and scores for the item CP007Q02
scored_item<- scores1[, c("CNT", "SCHOOLID","StIDStd", "CP007Q02")]  
  
#creating student ID by concatenating: CNT, SCHoolID and Student ID
scored_item$ID <- paste(scored_item$CNT, scored_item$SCHOOLID, scored_item$StIDStd, sep = "_")

#selecting relevant columns to create examinees data
examinees <- scored_item %>%
  select(ID, CP007Q02) %>%  
  mutate(
    score = case_when(
      CP007Q02 == "1" ~ 1,  # success
      CP007Q02 == "0" ~ 0,  # sailure
      CP007Q02 == "8" ~ NA_real_,  # not reached → NA
      is.na(CP007Q02) ~ NA_real_  # keep original missing values as NA
    )
  ) %>%
  select(ID, score)

# filtering examinees to keep only IDs that exist in edges
examinees <- examinees %>%
  filter(ID %in% edges$StIDStd)
examinees<-as.data.frame(examinees)


# are there any missing scores?
sum(is.na(examinees$score)) # no missing
      # any other values than 0,1 in score column
any(!examinees$score %in% c(0, 1)) # just 1 and 0 values in the score column
 





#---------------- PLAUSIBLE VALUES DATASET--------------


#GENDER is in this dataset--- 2= female, 1= male - column is coded as: ST04Q01

#selecting only the countries of interest 
PVs <- plausible_values[plausible_values$CNT %in% c("COL", "SGP", "NOR"), ]

#creating new ID for the plausible values in line with edges and examinees
PVs$ID <- paste(PVs$CNT, PVs$SCHOOLID, PVs$StIDStd, sep = "_")

#ordering columns so ID is first
PVs <- PVs[,c(20, 1:19)]

#only keep IDs that appear in edges
PVs <- PVs %>%
  filter(ID %in% edges$StIDStd)

#aligning PV IDs with those from examinees
PVs <- PVs[PVs$ID %in% examinees$ID, ]
PVs<- as.data.frame(PVs)
sum(is.na(PVs))  #any missing values
sum(is.na(PVs$ST04Q01)) # any missing values in gender column?  =NO

#removing metadata identifying columns 
PVs <- PVs %>%
  select(-CNT, -SCHOOLID, -StIDStd)







#------------------- COMPUTING NETWORK FEATURES----------------------



## defining correct and incorrect operations
correct.selection <- paste0("SP",1:6)
correct.cancellation <-  paste0("CP",7:23)
incorrect.selection <- paste0("SP",7:23)
incorrect.cancellation <- paste0("CP",1:6)
correct.action <- c(correct.selection, correct.cancellation)
incorrect.action <- c(incorrect.selection,incorrect.cancellation)


n <- nrow(examinees) #the number of examinees 
features <- matrix(NA, nrow = n, ncol = 7) #creating a matrix to save features for each examinee

for (p in 1:n) {
  id <- examinees$ID[p]
  edges_p <- edges[edges$StIDStd == id,] # creating edges for a particular examinee
  net_p <- graph_from_data_frame(d=edges_p,vertices=nodes,directed = T) # creating an igraph object
  
  ## ploting the network graph
  colors_nodes <- c("gold","tomato","skyblue3","grey50") 
  V(net_p)$colors_nodes <- colors_nodes[as.numeric(V(net_p)$category)] #generate colors for vertices based on their category
  V(net_p)$size <- igraph::degree(net_p, mode = "total") #weight the size of vertices by the total degree
  E(net_p)$width <- E(net_p)$duration #weight the width of edges by duration
  ad_matrix_p <- as.matrix(as_adjacency_matrix(graph_from_data_frame(edges_p), attr = "duration", sparse = TRUE)) #get the adjecent matrix

  
  ## saving the plots for each student (if needed for visualisation)
#set.seed(1369)
#svg(filename = paste0("net_", examinees$ID[p], ".svg")) # save as scalable vector graphics
#plot(net_p, edge.arrow.size = 0.5,edge.arrow.width = 1.2, vertex.color = V(net_p)$colors_nodes, vertext.label.cex = 0.05, rescale = T,vertex.frame.color = V(net_p)$colors_nodes, edge.curved = 0.1)
#dev.off()
  
  ## computing network features
  n.vertex <- nrow(ad_matrix_p) #the number of unique operations taken by the examinee
  N.vertex <- 50 #the total number of operations defined in the task 
  features[p,1] <- n.vertex/N.vertex #operation diversity
  n.edge = ecount(net_p) #the total number of edges presented in the network
  features[p,2] <- n.edge/(n.vertex*(n.vertex-1)) #edge density
  features[p,3] <- reciprocity(net_p, mode = "ratio") #reciprocity
  features[p,4] <- transitivity(net_p, type = "global") #transitivity
  co_co <- length( E(net_p)[correct.action %->% correct.action] ) #the number of edges from correct actions to correct actions
  co_in <- length( E(net_p)[correct.action %->% incorrect.action] ) #the number of edges from correct actions to incorrect actions
  features[p,5] <- (co_co - co_in) / (co_co + co_in) #the E-I index
  features[p,6] <- mean(edges_p$duration, na.rm = T) #average time
  features[p,7] <- sd(edges_p$duration, na.rm = T) #standard deviation of time
  
  ## removing the edges, network object, and adjecent matrix to save memory
  rm(edges_p,net_p,ad_matrix_p)
}  

colnames(features) <- c("operation.diversity","edges.density","reciprocity","transitivity","EI","average.time","sd.time")
features[is.nan(features[,5]),5] <- 0 #replace NA with 0
features.score <- cbind(examinees[,c("ID","score")], features) #combining student information with network features
features.score <- merge(features.score,PVs,
  by = "ID",
  all.x = TRUE) #combining performance indicators with network features




#.......Descriptive statistics for the features.............

# Split dataset into three subsets based on country
features_COL <- features.score %>% filter(grepl("^COL", ID)) #COLOMBIA
features_NOR <- features.score %>% filter(grepl("^NOR", ID)) #NORWAY
features_SGP <- features.score %>% filter(grepl("^SGP", ID)) #SINGAPORE




#Descriptives function
desc_stats <- function(df) {
  df %>%
    summarise_all(list(
      mean = ~ mean(., na.rm = TRUE),
      sd = ~ sd(., na.rm = TRUE),
      se = ~ sd(., na.rm = TRUE) / sqrt(sum(!is.na(.))), # Correct SE calculation
      min = ~ min(., na.rm = TRUE),
      max = ~ max(., na.rm = TRUE),
      skewness = ~ skewness(., na.rm = TRUE),
      kurtosis = ~ kurtosis(., na.rm = TRUE)
    )) %>%
    pivot_longer(cols = everything(), 
                 names_to = c("Variable", "Statistic"), 
                 names_sep = "_") %>%
    pivot_wider(names_from = "Statistic", values_from = "value")
}


# Descriptive statistics for Colombian Students

#with score = 0
desc.COL_0 <- features_COL %>%
  filter(score == 0) %>%
  select(3:9) %>%
  desc_stats()
desc.COL_0 %>%
  kable()

      


#  with score = 1
desc.COL_1 <- features_COL %>%
  filter(score == 1) %>%
  select(3:9) %>%
  desc_stats()
desc.COL_1 %>%
  kable()

              
            


# Descriptive statistics for Norwegian Students

#with score = 0
desc.NOR_0 <- features_NOR %>%
  filter(score == 0) %>%
  select(3:9) %>%
  desc_stats()
desc.NOR_0 %>%
  kable()



#  with score = 1
desc.NOR_1 <- features_NOR %>%
  filter(score == 1) %>%
  select(3:9) %>%
  desc_stats()
desc.NOR_1 %>%
  kable()




# Descriptive statistics for Singapore Students

#with score = 0
desc.SGP_0 <- features_SGP %>%
  filter(score == 0) %>%
  select(3:9) %>%
  desc_stats()
desc.SGP_0 %>%
  kable()


#  with score = 1
desc.SGP_1 <- features_SGP %>%
  filter(score == 1) %>%
  select(3:9) %>%
  desc_stats()
desc.SGP_1 %>%
  kable()



#Distribution of students according to gender per country (1= female, 2=male)
# Colombia
(table(features_COL$ST04Q01)) #385 girls, 323 boys
round(100 * prop.table(table(features_COL$ST04Q01)), 1) #percentage 

#Norway
(table(features_NOR$ST04Q01)) #177 girls, 188 boys
round(100 * prop.table(table(features_NOR$ST04Q01)), 1)

#Singapore
(table(features_SGP$ST04Q01)) #227 girls, 227 boys
round(100 * prop.table(table(features_SGP$ST04Q01)), 1)

# score distribution per country
score_by_country <- examinees %>%
  mutate(country = substr(ID, 1, 3)) %>%
  filter(country %in% c("COL", "NOR", "SGP")) %>%
  group_by(country, score) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(country) %>%
  mutate(percent = round(100 * n / sum(n), 1))
print(score_by_country)




#______________________ANALYSES_________________________


#......................Hopkins Statistics..............

#before applying hopkins statistics and GMMs log-transform sd of time
features_logSD <- features.score
features_logSD[,"sd.time"] <- log(features_logSD[,"sd.time"]) #normalizing the values in sd.time to account for outliers


# spliting the main data based on country
features_COL1 <- features_logSD %>% filter(grepl("^COL", ID))
features_NOR1 <- features_logSD %>% filter(grepl("^NOR", ID))
features_SGP1<- features_logSD%>% filter(grepl("^SGP", ID))


# function to get the Hopkins statistic for every group across countries
hopkins <- function(data, label, seed = 123) {
  set.seed(seed)
  result <- get_clust_tendency(
    data,
    n = nrow(data) - 1,  
    graph = TRUE
  )
  cat(label, "Hopkins statistic:", round(result$hopkins_stat, 3), "\n")
  return(result$hopkins_stat)
}

# Colombia
hopkins_fail_COL    <- hopkins(features_COL1[features_COL1$score == 0, 3:9], "COL - Fail", 123)
hopkins_success_COL <- hopkins(features_COL1[features_COL1$score == 1, 3:9], "COL - Success", 134)

# Norway
hopkins_fail_NOR    <- hopkins(features_NOR1[features_NOR1$score == 0, 3:9], "NOR - Fail", 145)
hopkins_success_NOR <- hopkins(features_NOR1[features_NOR1$score == 1, 3:9], "NOR - Success", 157)

# Singapore
hopkins_fail_SGP    <- hopkins(features_SGP1[features_SGP1$score == 0, 3:9], "SGP - Fail", 156)
hopkins_success_SGP <- hopkins(features_SGP1[features_SGP1$score == 1, 3:9], "SGP - Success", 167)

#organizing an overview of hopkins estimates
hopkins_summary <- data.frame(
  Country = rep(c("COL", "NOR", "SGP"), each = 2),
  Group = rep(c("Fail", "Success"), 3),
  Hopkins = c(hopkins_fail_COL, hopkins_success_COL,
              hopkins_fail_NOR, hopkins_success_NOR,
              hopkins_fail_SGP, hopkins_success_SGP)
)

print(hopkins_summary)






#................ GAUSSIAN MIXTURE MODELS...................


# ................For Colombia.............

#Failure
fail.mclust_COL <- Mclust(features_COL1[features_COL1$score == 0, 3:9])
summary(fail.mclust_COL)
print(summary(mclustICL(features_COL1[features_COL1$score == 0, 3:9])))
print(mclustBootstrapLRT(features_COL1[features_COL1$score == 0, 3:9], modelName = "VEV"))

#Success
success.mclust_COL <- Mclust(features_COL1[features_COL1$score == 1, 3:9])
summary(success.mclust_COL)
print(summary(mclustICL(features_COL1[features_COL1$score == 1, 3:9])))
print(mclustBootstrapLRT(features_COL1[features_COL1$score == 1, 3:9], modelName = "EVV"))


#....................For Norway.....................
# Failure Group 
fail.mclust_NOR <- Mclust(features_NOR1[features_NOR1$score == 0, 3:9])
summary(fail.mclust_NOR)
print(summary(mclustICL(features_NOR1[features_NOR1$score == 0, 3:9])))
print(mclustBootstrapLRT(features_NOR1[features_NOR1$score == 0, 3:9], modelName = "VVV"))

# Success Group
success.mclust_NOR <- Mclust(features_NOR1[features_NOR1$score == 1, 3:9])
summary(success.mclust_NOR)
print(summary(mclustICL(features_NOR1[features_NOR1$score == 1, 3:9])))
print(mclustBootstrapLRT(features_NOR1[features_NOR1$score == 1, 3:9], modelName = "EVV"))


#..................For Singapore...................
# Failure Group (score = 0)
fail.mclust_SGP <- Mclust(features_SGP1[features_SGP1$score == 0, 3:9])
summary(fail.mclust_SGP)
print(summary(mclustICL(features_SGP1[features_SGP1$score == 0, 3:9])))
print(mclustBootstrapLRT(features_SGP1[features_SGP1$score == 0, 3:9], modelName = "VEI"))

# Success Group (score = 1)
success.mclust_SGP <- Mclust(features_SGP1[features_SGP1$score == 1, 3:9])
summary(success.mclust_SGP)
print(summary(mclustICL(features_SGP1[features_SGP1$score == 1, 3:9])))
print(mclustBootstrapLRT(features_SGP1[features_SGP1$score == 1, 3:9], modelName = "VEV"))




#.............adding cluster classification to each performance group/ country......
 
#the cluster classification is assigned back to the features data with non-log transformed sd.time

# Success Group for Colombia
success.group.COL <- features_COL[features_COL$score == 1,] 
success.group.COL$classification <- success.mclust_COL$classification
success.size.COL <- as.numeric(table(success.mclust_COL$classification))

# Failure Group for Colombia
fail.group.COL <- features_COL[features_COL$score == 0,]
fail.group.COL$classification <- fail.mclust_COL$classification
fail.size.COL <- as.numeric(table(fail.mclust_COL$classification))


# Success Group for Norway
success.group.NOR <- features_NOR[features_NOR$score == 1,] 
success.group.NOR$classification <- success.mclust_NOR$classification
success.size.NOR <- as.numeric(table(success.mclust_NOR$classification))

# Failure Group for Norway
fail.group.NOR <- features_NOR[features_NOR$score == 0,]
fail.group.NOR$classification <- fail.mclust_NOR$classification
fail.size.NOR <- as.numeric(table(fail.mclust_NOR$classification))


# Success Group for Singapore
success.group.SGP <- features_SGP[features_SGP$score == 1,] 
success.group.SGP$classification <- success.mclust_SGP$classification
success.size.SGP <- as.numeric(table(success.mclust_SGP$classification))

# Failure Group for Singapore
fail.group.SGP <- features_SGP[features_SGP$score == 0,]
fail.group.SGP$classification <- fail.mclust_SGP$classification
fail.size.SGP <- as.numeric(table(fail.mclust_SGP$classification))





#............................Descriptive statistics for each cluster...........................


# ----------function to compute means and SEs for features within clusters---------

feature_summary <- function(df, cluster_col, feature_cols) {  # defining a function with arguments: df for the success or fail groups; classification; features columns
  df$Cluster <- df[[cluster_col]]  #extracting classification and assigning it to Cluster column
  
  #  Converting numeric column indices to names if needed later
  if (is.numeric(feature_cols)) {
    feature_cols <- names(df)[feature_cols]
  }
  
  cluster_sizes <- df %>%
    group_by(Cluster) %>% #grouping data by cluster
    summarise(Size = n(), .groups = "drop") # counting students in the cluster
  
  # Means of each feature by cluster
  means <- df %>%
    group_by(Cluster) %>%
    summarise(across(all_of(feature_cols), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
  
  # Standard Errors of each feature mean by cluster
  ses <- df %>%
    group_by(Cluster) %>%
    summarise(across(all_of(feature_cols), ~ sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x)))), .groups = "drop")
  
  # Combining means and standard errors into future format: "mean (SE)"
  summary <- means
  for (col in feature_cols) {
    summary[[col]] <- paste0(
      round(means[[col]], 3), " (", round(ses[[col]], 3), ")" # expressing numeric values with 3 decimals
    )
  }
  
  summary <- summary %>%
    left_join(cluster_sizes, by = "Cluster") # joining the size of the cluster column
  
  return(summary) # returning final summary table
}


# -------------Colombia clusters--------------

# applying the feature_summary function to sucess and failure groups in Colombia
# columns of interest are the 3:9 features and classification column
fail_features_COL <- feature_summary(fail.group.COL, "classification", 3:9)
success_features_COL <- feature_summary(success.group.COL, "classification", 3:9)

# adding a group label
fail_features_COL$Group <- "Fail"
success_features_COL$Group <- "Success"

# Combining the two df for fail and success for one country; row-wise (Colombia)
table_features_COL <- rbind(fail_features_COL, success_features_COL)

# reordering columns so group is first
table_features_COL <- table_features_COL %>%
  select(Group, everything())

# displaying the table for Colombia clusters
kable(table_features_COL, caption = "Colombia – Cluster Feature Summary")


# --------------Norway----------------
fail_features_NOR <- feature_summary(fail.group.NOR, "classification", 3:9)
success_features_NOR <- feature_summary(success.group.NOR, "classification", 3:9)
fail_features_NOR$Group <- "Fail"
success_features_NOR$Group <- "Success"
table_features_NOR <- rbind(fail_features_NOR, success_features_NOR)
table_features_NOR <- table_features_NOR %>%
  select(Group, everything())
kable(table_features_NOR, caption = "Norway – Cluster Feature Summary")




#---------------Singapore---------------
fail_features_SGP <- feature_summary(fail.group.SGP, "classification", 3:9)
success_features_SGP <- feature_summary(success.group.SGP, "classification", 3:9)
fail_features_SGP$Group <- "Fail"
success_features_SGP$Group <- "Success"
table_features_SGP <- rbind(fail_features_SGP, success_features_SGP)
table_features_SGP <- table_features_SGP %>%
  select(Group, everything())
kable(table_features_SGP, caption = "Singapore – Cluster Feature Summary")




#---------------function to compute cluster level summary of PVs-------------
PVs_summary <- function(df, cluster_col, domain_prefix) { #argumets: df with the PVs, classification (cluster assignment); PVs prefix ^CPRO

  
  df$Cluster <- df[[cluster_col]] # creating cluster column
  cluster_ids <- sort(unique(df$Cluster)) # getting a list of uniquues cluster numbers
  
  # geting the PVs columns
  pv_cols <- grep(paste0("^PV[1-5]", domain_prefix), names(df), value = TRUE) #finding all PVs columns for the given domain
  if (length(pv_cols) != 5) stop(paste("Missing plausible values for", domain_prefix))# ensuring all columns are present
  
  # creating two empty lists to store PV means and SEs per cluster
  cluster_means_list <- list()
  cluster_ses_list <- list()
  
  #looping over all 5 PVS in a domain, for each PV column compute mean and SE of that PV per cluster
  for (pv in pv_cols) {
    stats <- df %>%
      group_by(Cluster) %>%
      summarise(
        mean_score = mean(.data[[pv]], na.rm = TRUE),
        se_score = sd(.data[[pv]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[pv]]))),
        .groups = "drop"
      ) %>%
      arrange(Cluster)
    
    #appending the means and SEs vector to their respective lists
    cluster_means_list[[length(cluster_means_list) + 1]] <- stats$mean_score
    cluster_ses_list[[length(cluster_ses_list) + 1]] <- stats$se_score
  }
  
  # converting everything to lists of vectors for averageMI computation
  par_list <- lapply(cluster_means_list, as.numeric)
  se_list <- lapply(cluster_ses_list, as.numeric)
  
  # Using averageMI to combine means and SEs from multiple PVs to obtain a final mean and SE x cluster
  mi <- averageMI(par = par_list, SEpar = se_list)
  
  #converting results to dataframe
  mi_df <- as.data.frame(mi)
  names(mi_df) <- c("Mean", "SE")
  mi_df$Cluster <- cluster_ids #adding cluster column
  mi_df$Label <- paste0(round(mi_df$Mean, 3), " (", round(mi_df$SE, 3), ")") # display format mean (SE)
  
  return(mi_df) # return Mean (SE) in Label
}


#-----Function to apply PVs_summary to one group (e.g., fail.group_COL)
domain_summary <- function(df, group_label, country_code, domain) {
  result <- PVs_summary(df, "classification", domain) %>%
    select(Cluster, Label) # for this group return mean(SE)
  colnames(result)[2] <- paste0(domain, "_PV") # Label becomes domain_PV like MATH_PV
  result$Group <- group_label # column for group
  result$Country <- country_code #column for country
  return(result)
}

# -----------------------------
# Applying domain_summary to each group and domain, starting with problem-solving
# -----------------------------
pro_fail_COL     <- domain_summary(fail.group.COL, "Fail", "COL", "CPRO")
pro_success_COL  <- domain_summary(success.group.COL, "Success", "COL", "CPRO")
pro_fail_NOR     <- domain_summary(fail.group.NOR, "Fail", "NOR", "CPRO")
pro_success_NOR  <- domain_summary(success.group.NOR, "Success", "NOR", "CPRO")
pro_fail_SGP     <- domain_summary(fail.group.SGP, "Fail", "SGP", "CPRO")
pro_success_SGP  <- domain_summary(success.group.SGP, "Success", "SGP", "CPRO")

# -----------------------------
# MATH
# -----------------------------
math_fail_COL    <- domain_summary(fail.group.COL, "Fail", "COL", "MATH")
math_success_COL <- domain_summary(success.group.COL, "Success", "COL", "MATH")
math_fail_NOR    <- domain_summary(fail.group.NOR, "Fail", "NOR", "MATH")
math_success_NOR <- domain_summary(success.group.NOR, "Success", "NOR", "MATH")
math_fail_SGP    <- domain_summary(fail.group.SGP, "Fail", "SGP", "MATH")
math_success_SGP <- domain_summary(success.group.SGP, "Success", "SGP", "MATH")

# -----------------------------
# READ
# -----------------------------
read_fail_COL    <- domain_summary(fail.group.COL, "Fail", "COL", "READ")
read_success_COL <- domain_summary(success.group.COL, "Success", "COL", "READ")
read_fail_NOR    <- domain_summary(fail.group.NOR, "Fail", "NOR", "READ")
read_success_NOR <- domain_summary(success.group.NOR, "Success", "NOR", "READ")
read_fail_SGP    <- domain_summary(fail.group.SGP, "Fail", "SGP", "READ")
read_success_SGP <- domain_summary(success.group.SGP, "Success", "SGP", "READ")



#----------funtion to merge all 3 domains into one dataframe-----------
combine_country_pv <- function(pro, math, read) {
  reduce(list(pro, math, read), full_join, by = c("Country", "Group", "Cluster")) %>%
    select(Country, Group, Cluster, CPRO_PV, MATH_PV, READ_PV)
} #keeping all clusters, country, group, and three domain results: format "mean (SE)"

#applying the above function to each country
#Colombia
table_COL <- combine_country_pv(pro_fail_COL, math_fail_COL, read_fail_COL) %>%
  bind_rows(combine_country_pv(pro_success_COL, math_success_COL, read_success_COL)) #bind success and failure group vertically into one table

#Norway
table_NOR <- combine_country_pv(pro_fail_NOR, math_fail_NOR, read_fail_NOR) %>%
  bind_rows(combine_country_pv(pro_success_NOR, math_success_NOR, read_success_NOR))

#Singapore
table_SGP <- combine_country_pv(pro_fail_SGP, math_fail_SGP, read_fail_SGP) %>%
  bind_rows(combine_country_pv(pro_success_SGP, math_success_SGP, read_success_SGP))

#displaying PVs means and SEs in a neat table
kable(table_COL, caption = "Colombia – Cluster-Level PV Summary")
kable(table_NOR, caption = "Norway – Cluster-Level PV Summary")
kable(table_SGP, caption = "Singapore – Cluster-Level PV Summary")





#-------------------PLOTTING THE CLUSTERS-----------------------

# Function to create simplified network plot for a specific cluster
plot_cluster_network <- function(cluster_data, edges, nodes, country, group, cluster_num, save_plot = TRUE) {
  ids_cluster <- cluster_data$ID[cluster_data$classification == cluster_num]  # getting IDs for the specific cluster
  edges_cluster <- edges[edges$StIDStd %in% ids_cluster, ] # subseting the edges for those IDs
  
  edges_cluster_mean <- aggregate(duration ~ action_from + action_to + category,
                                  data = edges_cluster,
                                  mean) # initial network with mean duration
  edges_cluster_mean <- edges_cluster_mean[order(edges_cluster_mean$action_from,
                                                 edges_cluster_mean$action_to), ]
  net_cluster <- graph_from_data_frame(d = edges_cluster_mean,
                                       vertices = nodes,
                                       directed = TRUE) # creating initial network
  colors_nodes <- c("gold", "tomato", "skyblue3", "grey50") # color codes for nodes
  edge_freq <- data.frame(table(edges_cluster$action_from, edges_cluster$action_to))
  names(edge_freq) <- c("action_from", "action_to", "freq")  #frequency of each edge
  cluster_size <- length(ids_cluster)
  min_freq <- ceiling(0.10 * cluster_size)# keeping edges that occur more than 10% of cluster size
  edges_to_remove <- edge_freq %>%  #edges to remove (low frequency)
    filter(freq < min_freq & freq > 0) %>%
    mutate(edge_key = paste(action_from, action_to, sep = "|"))
  
  # converting to igraph edge format and remove low-frequency edges
  if (nrow(edges_to_remove) > 0) {
    edges_to_remove_list <- strsplit(edges_to_remove$edge_key, split = "|", fixed = TRUE)
    del_edges <- do.call(rbind, edges_to_remove_list)
    edges_to_delete <- apply(del_edges, 1, function(x) get_edge_ids(net_cluster, vp = x, directed = TRUE))
    net_cluster_simplified <- delete_edges(net_cluster, edges_to_delete)
  } else {
    net_cluster_simplified <- net_cluster  # assign original graph when no edges removed
  }
  
  layout_clean <- layout_with_fr(net_cluster_simplified)  # final layout
  
  # getting the nodes used in actual paths, touched by students
  used_nodes <- unique(c(edges_cluster$action_from, edges_cluster$action_to))
  
  # styling of nodes
  V(net_cluster_simplified)$color <- ifelse(
    V(net_cluster_simplified)$name %in% used_nodes,
    colors_nodes[as.numeric(V(net_cluster_simplified)$category)],
    NA  # no fill color for untouched nodes
  )
  
  V(net_cluster_simplified)$frame.color <- NA  # no framing for the colors
  V(net_cluster_simplified)$label.color <- ifelse(
    V(net_cluster_simplified)$name %in% used_nodes,
    "black", "grey60"
  ) # labelling color of the nodes nodes names (SP1, SP22): darker if used, light gray if untouched
  
  V(net_cluster_simplified)$label.cex <- 0.7 #font size of nodes
  
  #  used nodes small, untouched even smaller
  V(net_cluster_simplified)$size <- ifelse(
    V(net_cluster_simplified)$name %in% used_nodes,
    6,  # smaller than before
    1   # tiny for untouched
  )
  
  #  styling of edges
  E(net_cluster_simplified)$width <- rescale(E(net_cluster_simplified)$duration, to = c(1.5, 6))
  E(net_cluster_simplified)$arrow.size <- 0.3
  E(net_cluster_simplified)$color <- alpha("grey30", 0.5)
  
  # plotting
  par(mar = c(1,1,2,1))  
  set.seed(1020)
  plot(net_cluster_simplified,
       layout = layout_clean,
       vertex.label.cex = V(net_cluster_simplified)$label.cex,
       vertex.label.color = V(net_cluster_simplified)$label.color,
       vertex.color = V(net_cluster_simplified)$color,
       vertex.size = V(net_cluster_simplified)$size,
       edge.arrow.size = E(net_cluster_simplified)$arrow.size,
       edge.width = E(net_cluster_simplified)$width,
       edge.color = E(net_cluster_simplified)$color,
       edge.curved = 0.2,
       main = paste(country, group, "Cluster", cluster_num))
  
  # Save plot if necessary  
  #if (save_plot) {
    #svg(filename = paste0(country, "_", group, "_Cluster_", cluster_num, ".svg"), 
   #     width = 8, height = 6)
  # plot(net_cluster_simplified,
    # layout = layout_clean,
      # vertex.label.cex = V(net_cluster_simplified)$label.cex,
       # vertex.label.color = V(net_cluster_simplified)$label.color,
      #  vertex.color = V(net_cluster_simplified)$color,
       #  vertex.size = V(net_cluster_simplified)$size,
      # edge.arrow.size = E(net_cluster_simplified)$arrow.size,
      #  edge.width = E(net_cluster_simplified)$width,
       #  edge.color = E(net_cluster_simplified)$color,
       #  edge.curved = 0.2,
      #   main = NULL)
   # dev.off()
 #}
}




# function to generate all cluster plots for a specific country 
generate_plots <- function(fail_data, success_data, edges, nodes, country) {
  fail_clusters <- length(unique(fail_data$classification))
  success_clusters <- length(unique(success_data$classification)) # getting the number of clusters in each group
  
  dev.new()
  
  # failure clusters
  for(i in 1:fail_clusters) {
    plot_cluster_network(fail_data, edges, nodes, country, "Failure", i, save_plot = TRUE)
  }
  
  # success clusters
  for(i in 1:success_clusters) {
    plot_cluster_network(success_data, edges, nodes, country, "Success", i, save_plot = TRUE)
  }
}





# Generate plots for all countries
generate_plots(fail.group.COL, success.group.COL, edges, nodes, "COL")
generate_plots(fail.group.NOR, success.group.NOR, edges, nodes, "NOR")
generate_plots(fail.group.SGP, success.group.SGP, edges, nodes, "SGP")









