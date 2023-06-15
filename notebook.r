
# Load required packages
library(tidyverse, quietly = TRUE)
library(lubridate, quietly = TRUE)

# Read in incidents dataset
incidents <- read_csv("datasets/downsample_police-department-incidents.csv")

# Read in calls dataset
calls <- read_csv("datasets/downsample_police-department-calls-for-service.csv")

print('Done!')

# These packages need to be loaded in the first @tests cell. 
library(testthat) 
library(IRkernel.testthat)

# Then follows one or more tests of the student's code. 
# The @solution should pass the tests.
# The purpose of the tests is to try to catch common errors and to 
# give the student a hint on how to resolve these errors.

run_tests({
    test_that("required packages are loaded", {
        for(pkg in c("tidyverse", "lubridate")) {
            expect_true(pkg %in% .packages(),
            info = sprintf("Did you load %s?", pkg))
        } 
    })
    
    test_that("incidents is loaded correctly", {
        expect_is(incidents, "tbl_df", 
            info = "Did you read the incidents dataset with read_csv (not read.csv)?")
        expect_equal(nrow(incidents), 84000,
            info = "Did you assign the correct dataset to the incidents variable?")
    })
              
    test_that("calls is loaded correctly", {
        expect_is(calls, "tbl_df", 
            info = "Did you read the incidents dataset with read_csv (not read.csv)?")
        expect_equal(nrow(calls), 100000,
            info = "Did you assign the correct dataset to the calls variable?")
    })
              
})

# Glimpse the structure of both datasets
glimpse(incidents)
glimpse(calls)


# Aggregate the number of reported incidents by Date
daily_incidents <- incidents %>%
    count(Date, sort = TRUE) %>%
    rename(n_incidents = n)

# Aggregate the number of calls for police service by Date
daily_calls <- calls %>%
    count(Date, sort = TRUE) %>%
    rename(n_calls = n)

# One or more tests of the student's code. 
# The @solution should pass the tests.
# The purpose of the tests is to try to catch common errors and to 
# give the student a hint on how to resolve these errors.
run_tests({
    test_that("daily_incidents correct", {
        for(colx in c("Date", "n_incidents")) {
            expect_true(colx %in% colnames(daily_incidents),
            info = "Did you name your daily_incidents columns correctly?")
        }
        expect_equal(nrow(daily_incidents), 5525,
            info = "Did you count() the Date column?")    
    })
    
    test_that("daily_calls correct", {
        for(col in c("Date", "n_calls")) {
            expect_true(col %in% colnames(daily_calls),
            info = "Did you name your daily_calls columns correctly?")
        }
        expect_equal(nrow(daily_calls), 947,
            info = "Did you count() the Date column?") 
    })
})

# Join data frames to create a new "mutated" set of information
shared_dates <- daily_calls %>% 
inner_join(daily_incidents, by = "Date")

# Take a glimpse of this new data frame
glimpse(shared_dates)

# One or more tests of the student's code. 
# The @solution should pass the tests.
# The purpose of the tests is to try to catch common errors and to 
# give the student a hint on how to resolve these errors.
run_tests({
    test_that("shared_dates is correct", {
        expect_true(c("Date") %in% colnames(shared_dates),
            info = "Did you include the `by =` parameter of `inner_join()` to specify correct columns to join by?")
        for(col in c("Date", "n_incidents", "n_calls")) {
            expect_true(col %in% colnames(shared_dates),
            info = "Check that you called the correct data frames in the correct order in the `inner_join()`.")
        }
        expect_equal(nrow(shared_dates), 776,
            info = "Is the correct mutating join called?")
    })

})

# Gather into long format using the "Date" column to define observations
plot_shared_dates <- shared_dates %>%
  gather(key = "report", value = "count", -Date)

# Plot points and regression trend lines
ggplot(plot_shared_dates, aes(x = Date, y = count, color = report)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x)

# One or more tests of the student's code. 
# The @solution should pass the tests.
# The purpose of the tests is to try to catch common errors and to 
# give the student a hint on how to resolve these errors.
plot_test <- last_plot()

run_tests({
    test_that("plot_shared_dates is correct", {
        expect_true(exists("plot_shared_dates"), 
            info = "Have you assigned gathered data to plot_shared_dates?")
        for(col in c("Date", "report", "count")) {
            expect_true(col %in% colnames(plot_shared_dates),
            info = sprintf("Do you have the appropriate column `%s` defined?", col))
        }
        expect_equal(nrow(plot_shared_dates), 1552,
            info = "Did you define which variables to gather() correctly?")
    
    })

    test_that("graph is correct", {
        expect_true(plot_test$labels$x == 'Date',
            info = "Is your x-axis assigned to the `Date` column?")
        expect_true(plot_test$labels$y == 'count',
            info = "Is your y-axis assigned to the `count` column?")
        expect_true(plot_test$labels$colour == 'report',
            info = "Is the color aesthetic assigned to the `report` column?")
    })
})

# Calculate correlation coefficient between daily frequencies
daily_cor <- with(shared_dates, cor(n_calls, n_incidents))
daily_cor

# Summarize frequencies by month
correlation_df <- shared_dates %>% 
  mutate(month = month(Date)) %>%
  group_by(month) %>% 
  summarize(n_incidents = sum(n_incidents),
            n_calls = sum(n_calls),
           .groups = "keep")

# Calculate correlation coefficient between monthly frequencies
monthly_cor <- with(correlation_df, cor(n_incidents, n_calls))
monthly_cor

# One or more tests of the student's code.  
# The @solution should pass the tests.
# The purpose of the tests is to try to catch common errors and to 
# give the student a hint on how to resolve these errors.
run_tests({
    test_that("Daily frequency correlation coefficient is correct", {
        expect_equal(round(daily_cor, 3), 0.147, 
            info = "Have you used the shared_dates data frame to calculate the correlation coefficient between n_incidents and n_calls?")
    })
    
    test_that("correlation_df summarised correctly", {
        expect_equal(correlation_df$month, 1:12,
            info = "Did you create a new column `month` from `Date`?")
        expect_true(correlation_df[1,"month"] == 1 & correlation_df[1,"n_incidents"] == 5078 & correlation_df[1,"n_calls"] == 7210,
            info = "Did you call sum() on the columns you wish to summarize?")
    })
    
    test_that("Monthly frequencly correlation coefficient is correct", {
        expect_equal(round(monthly_cor, 3), 0.971,
            info = "Have you used the correlation_df data frame to calculate the correlation coefficient between n_incidents and n_calls? 
                    The result should be a scalar.")
    })
    
})

# Subset calls to police by shared_dates
calls_shared_dates <- calls %>% 
semi_join(shared_dates, by = "Date")

# Perform a sanity check that we are using this filtering join function appropriately
identical(sort(unique(shared_dates$Date)), sort(unique(calls_shared_dates$Date)))

# Filter recorded incidents by shared_dates
incidents_shared_dates <- incidents %>% 
semi_join(shared_dates, by = "Date")

# One or more tests of the student's code. 
# The @solution should pass the tests.
# The purpose of the tests is to try to catch common errors and to 
# give the student a hint on how to resolve these errors.
run_tests({
    test_that("calls_shared_dates is correct", {
        expect_true(identical(colnames(calls), colnames(calls_shared_dates)), 
            info = "Check that you called the correct data frames in the correct order in the semi_join().")
        expect_equal(nrow(calls_shared_dates), 94720,
            info = "Did you use semi_join() to filter the calls data frame?")
    })
    
    test_that("sanity check is true", {
        expect_true(identical(sort(unique(shared_dates$Date)), sort(unique(calls_shared_dates$Date))),
            info = "Did you use the correct filtering join function?")
    })
    
    test_that("incidents_shared_dates is correct", {
        expect_true(identical(colnames(incidents), colnames(incidents_shared_dates)), 
            info = "Check that you called the correct data frames in the correct order in the semi_join().")
        expect_equal(nrow(incidents_shared_dates), 63587,
            info = "Did you use semi_join() to filter the incidents data frame?")
    })
})

# Create a bar chart of the number of calls for each crime
plot_calls_freq <- calls_shared_dates %>% 
  count(Descript) %>% 
  top_n(15, n) %>% 
  ggplot(aes(x = reorder(Descript, n), y = n)) +
  geom_bar(stat = 'identity') +
  ylab("Count") +
  xlab("Crime Description") +
  ggtitle("Calls Reported Crimes") +
  coord_flip()
  

# Create a bar chart of the number of reported incidents for each crime
plot_incidents_freq <- incidents_shared_dates %>% 
  count(Descript) %>% 
  top_n(15, n)  %>% 
  ggplot(aes(x = reorder(Descript, n), y = n)) +
  geom_bar(stat = 'identity') +
  ylab("Count") +
  xlab("Crime Description") +
  ggtitle("Incidents Reported Crimes") +
  coord_flip()

# Output the plots
plot_calls_freq
plot_incidents_freq

# One or more tests of the student's code.
# The @solution should pass the tests.
# The purpose of the tests is to try to catch common errors and to 
# give the student a hint on how to resolve these errors.
run_tests({
    test_that("plot_calls_freq is correct", {
        expect_true(exists("plot_calls_freq"), 
            info = "Have you saved your plot as the variable `plot_calls_freq`?")
        expect_true(colnames(plot_calls_freq$data[,1]) == "Descript",
            info = "Did you count `calls_shared_dates` by `Descript`?")
        expect_equal(dim(plot_calls_freq$data), c(15,2),
            info = "Did you select the top 15 crime types of `calls_shared_dates`?")
        expect_true(plot_calls_freq$mapping$x[2] == "reorder(Descript, n)()",
            info = "Did you reorder() `Descript` by `n`?")
    })
    
    test_that("plot_incidents_freq is correct", {
        expect_true(exists("plot_incidents_freq"), 
            info = "Have you saved your plot as the variable `plot_incidents_freq`?")
        expect_true(colnames(plot_incidents_freq$data[,1]) == "Descript",
            info = "Did you count `incidents_shared_dates` by `Descript`?")
        expect_equal(dim(plot_incidents_freq$data), c(15,2),
            info = "Did you select the top 15 crime types of `incidents_shared_dates`?")
        expect_true(plot_incidents_freq$mapping$x[2] == "reorder(Descript, n)()",
            info = "Did you reorder() `Descript` by `n`?")
    })
})


# Arrange the top 10 locations of called in crimes in a new variable
location_calls <- calls_shared_dates %>%
  filter(Descript == "Auto Boost / Strip") %>% 
  count(Address) %>% 
  arrange(desc(n))%>% 
  top_n(10, n)

# Arrange the top 10 locations of reported incidents in a new variable
location_incidents <- incidents_shared_dates %>%
  filter(Descript == "GRAND THEFT FROM LOCKED AUTO") %>% 
  count(Address) %>% 
  arrange(desc(n))%>% 
  top_n(10, n)

# Print the top locations of each dataset for comparison
location_calls
location_incidents

# One or more tests of the student's code. 
# The @solution should pass the tests.
# The purpose of the tests is to try to catch common errors and to 
# give the student a hint on how to resolve these errors.
run_tests({
    test_that("location_calls is correct", {
        expect_true(exists("location_calls"), 
            info = "Have you saved your plot as the variable `location_calls`?")
        expect_true(colnames(location_calls[,1]) == 'Address',
            info = "Did you count `calls_shared_dates` by `Address`?")
        expect_equal(dim(location_calls), c(11,2),
            info = "Did you select the top 10 crime types of `calls_shared_dates`?")
        expect_true(identical(as.numeric(location_calls$n), c(21,20,18,12,12,10,9,7,7,7,7)),
            info = "Did you filter `calls_shared_dates` for 'Auto Boost / Strip' appropriately?")
    })
    
    test_that("location_incidents is correct", {
        expect_true(exists("location_incidents"), 
            info = "Have you saved your plot as the variable `location_incidents`?")
        expect_true(colnames(location_incidents[,1]) == 'Address',
            info = "Did you count `incidents_shared_dates` by `Address`?")
        expect_equal(dim(location_incidents), c(10,2),
            info = "Did you select the top 10 crime types of `incidents_shared_dates`?")
        expect_true(identical(as.numeric(location_incidents$n), c(441,89,84,61,38,36,35,34,33,30)),
            info = "Did you filter `incidents_shared_dates` for 'GRAND THEFT FROM LOCKED AUTO' appropriately?")
    })
})

# Load ggmap
library(ggmap)

# Read in a static map of San Francisco 
sf_map <- readRDS("datasets/sf_map.RDS")

# Filter grand theft auto incidents
auto_incidents <- incidents_shared_dates %>% 
    filter(Descript == "GRAND THEFT FROM LOCKED AUTO")

# Overlay a density plot of auto incidents on the map
ggmap(sf_map) +
  stat_density_2d(
    aes(x = X, y = Y, fill = ..level..), alpha = 0.15,
    size = 0.01, bins = 30, data = auto_incidents,
    geom = "polygon")

# One or more tests of the student's code. 
# The @solution should pass the tests.
# The purpose of the tests is to try to catch common errors and to 
# give the student a hint on how to resolve these errors.
plot_test <- last_plot()

run_tests({
    test_that("map read correctly", {
        expect_true("ggmap" %in% .packages(), info = "Don't forget to load ggmap!")
        expect_true(exists("sf_map"), 
            info = "Have you read the map of San Francisco in and assigned to the variable `sf_map`?")
    })
    
    test_that("auto_incidents is correct", {
        expect_true(exists("auto_incidents"), 
            info = "Did you assign the filtered `incidents_shared_dates` to a new variable `auto_incidents`?")
        expect_equal(dim(auto_incidents), c(8685,13),
            info = "Did you filter `incidents_shared_dates` for 'GRAND THEFT FROM LOCKED AUTO' appropriately?")
    })
    
    test_that("graph is correct", {
        expect_true(plot_test$labels$x == 'lon',
            info = "Did you pass the `sf_map` object to `ggmap()`?")
        expect_true(plot_test$layers[[1]]$stat$retransform,
            info = "Did you pass `auto_incidents` to the `data` parameter of `stat_2d_density()`?")
    })
})
