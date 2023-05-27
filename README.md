# Animal Movement Summary and Stationarity Analysis 

MoveApps

Github repository: https://github.com/alexvmt/animal-movement-summary-and-stationarity-analysis

## Description
Exploratory app, which should help as a starting point to detect stationarity in data sets. A descriptive statistic summarizes the given tracking data to spot less movement, visualizes the tracks on a map, and shows for one individial a zoomable time series plot of moved distances from one day to another. The interface allows to filter for individials and specific date ranges (such as last week / 30 days / 90 days / half a year/ ...).
 


## Documentation
The app processes the given data set and fills a dropdown with the given individials (tag.local.identifier as unique ID). This dropdown can be used to analyse a single animal of a tracked group of animals. Next to the dropdown a date range filter helps to focus on more recent periods of time. The filter is always cutting of the last n days of each given track. The processing itself contains the following steps:
- drop rows with missing or duplicated values
- filter on given date range and potentially individial
- calculate distance between coordinates in meters
- aggregate data per day (not used for the map)

The analysis consists of three components:
a) Statistics table: summarizing some key figures of the single animals
b) Tracker Map: showing the tracks on a map
c) Timeseries plot: showing the moved distance from one day to another of a single animal

a) Statistics table
This sortable table (only one column can be used for this) shows the following stats for each individial (the individial filter does not influence the table):
- number of observations
- days without observations (are there fewer observations than days)
- was today's total movement below the 1.5x standard deviation of current date range
- total distance covered (in meters)
- average distance (in meters)

b) Tracker map
Show the movement of all or single animals on a map. Identify different behavior patterns for different date ranges and differences between single animals. Both date range and individials influence this view.

c) Timeseries plot
Shows per default the time series for the first found indivial. The dropdown can then be used to show different animals in this plot. the timeseries is interactive an the user can zoom into specific regions of interest.


Potential workflow:
1. Use the summary table to identify individials of interest, which have been moving a little or not throughout the last week for example (use the date range filter to decide on a suitable time frame).

2. Use the dropdown to filter for the animal of interest to explore its behavior on the map. This could help to answer questions such as: is the animal in a location of potential nesting place? are there any pecularities in the recent movements?

3. Use the timeseries to spot a decrease of movement (as compared to the previous days/weeks). Is it moving significantly less? Is this usual for the season as compared to the previous year? Is there data missing or is there no movement at all?

This exemplary workflow might be helpful to spot unusual animal/tracker behavior.



### Input data
MoveStack in Movebank format

### Output data
Shiny user interface (UI) moveStack in Movebank format

### Artefacts
None.

### Settings
No predefined settings needed. The app allows to apply two filters on individials and date ranges.

### Most common errors
None known yet.

### Null or error handling
Data is taken as is and only aggregated for the visualization.
