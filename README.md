# Animal Movement Summary and Stationarity Analysis 

MoveApps

Github repository: https://github.com/alexvmt/animal-movement-summary-and-stationarity-analysis

## Description
Exploratory app that should help as a starting point to detect stationarity in animal tracking datasets.
A descriptive statistics table (movement summary) summarizes the given tracking data to spot anomalous behavior,
visualizes animal tracks and last coordinates on a map
and shows for a selected individual a zoomable time series plot of moved distances from one day to another.
The interface allows to filter for individuals and specific date ranges (such as last week / 30 days / 90 days / ...).
 
## Documentation
The app processes a given dataset and fills a dropdown with the given individuals (`tag.local.identifier` is used as unique id).
This dropdown can be used to analyze a single animal of a tracked group of animals.
In addition, a date range filter helps to focus on more recent periods of time.
The filter is always cutting off the last n days of each given track.
The processing itself contains the following steps:
- drop rows with missing or duplicated values
- filter on given date range and potentially individual
- calculate distance between coordinates in meters
- aggregate data per day (not used for the map, only for the time series plot)

The analysis consists of three components:
a) Statistics table: summarizing some key figures per animal
b) Tracker map: showing tracks and last coordinates
c) Time series plot: showing moved distance from one day to another for a single animal

a) Statistics table
This sortable table (only one column can be used for this) shows the following stats for each individual for the selected date range (the individual filter does not influence the table):
- number of observations
- days without observations (Are there fewer observations than days in the selected date range?)
- was today's total movement below the 1.5x standard deviation of the current date range
- total distance moved (in meters)
- average distance moved (in meters)
- average and variance of measurements per day

b) Tracker map
Show the movement of all animals or a single animal and their/its last coordinates on a map.
Identify different behavior patterns for different date ranges and differences between animals.
Both date range and individual filters influence this view.

c) Time series plot
Shows per default the time series for the first individual in the dataset.
The dropdown can then be used to show different animals in this plot.
The time series is interactive and the user can zoom into specific regions of interest.


Potential workflow:
1. Use the movement summary table to identify individuals of interest that have been moving only a little or not at all throughout the last week for example
(use the date range filter to decide on a suitable time frame).

2. Use the dropdown to filter for the animal of interest to explore its behavior on the map.
This could help to answer questions such as: Is the animal in the area of a potential nesting place? Are there any peculiarities in the recent movements?

3. Use the time series plot to spot a decrease of movement (as compared to the previous days/weeks).
Is it moving significantly less? Is this usual for the season as compared to the previous year? Is there data missing or is there no movement at all?

This exemplary workflow might be helpful to spot unusual animal behavior.

### Input data
MoveStack in Movebank format

### Output data
Shiny user interface (UI)

### Artefacts
None.

### Settings
No predefined settings needed. The app allows to apply two filters on individuals and date ranges.

### Most common errors
None known yet.

### Null or error handling
Data is taken as is and only processed and aggregated for the visualizations.
