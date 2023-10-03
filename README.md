# Animal Movement Summary and Stationarity Analysis 

MoveApps

Github repository: https://github.com/alexvmt/animal-movement-summary-and-stationarity-analysis

## Description
This is an exploratory app that aims to help as a starting point to visually detect stationarity in animal tracking datasets.
A descriptive statistics table (movement summary) summarizes the given data to spot anomalous behavior,
a tracker map visualizes animal tracks and last coordinates
and a zoomable time series plot  shows, for a selected individual, moved distances from one day to another.
The interface allows to filter for individuals and specific date ranges (such as last day / week / 30 days / 60 days / ... / last year).
 
## Documentation
The app processes a given dataset and fills a dropdown with the given individuals (`tag.local.identifier` is used as unique id).
This dropdown can be used to analyze a single animal of a tracked group of animals.
In addition, a date range filter allows to focus on different periods of time.
The date range filter reduces the data to the last n most recent days of observations.
You can go back in time as much as 365 days.

After the data is loaded, the following steps are executed to process the last 365 days per individual (the resulting data forms the basis for the app's content):
- run several data transformation steps
- drop rows with missing or duplicated values
- filter data for last 365 days per individual
- order data and create lag variables
- calculate distance between coordinates in meters for each timestamp (using Haversine great circle distance)

Loading and processing the data may take a moment, depending on the number of individuals, the total date range and the frequency of measurements.
This processed data can then be filtered according to the selected date range and aggregated at a daily resolution.
The aggregated data is not used for the map but only for the statistics table and the time series plot.

The app consists of the following three components:
1. Statistics table: summarizing some key figures
2. Tracker map: showing tracks and last coordinates
3. Time series plot: showing moved distance from one day to another (for a single selected animal)

**Statistics table**

This sortable (only one column can be used for this) table shows the following stats for each individual for the selected date range (the individual filter does not influence the table):
- #days w measures: number of days with measures
- #days w/o measures: number of days without measures (Are there fewer observations than days in the selected date range?)
- last below avg.: was the last day's total movement below the 1.5x standard deviation of the current date range
- total distance (m): total distance moved in the selected date range (in meters)
- avg. distance (m): average distance moved per day in the selected date trange (in meters)
- avg. measures: average number of measurements per day in the selected date range
- var. measures: variance of number of measurements per day in the selected date range

![statistics_table](screenshots/statistics_table.png 'statistics_table')

**Tracker map**

This map shows the movement of all animals or a single animal and their/its last coordinates on a map.
The map can be used to identify different behavior patterns for different date ranges and differences between animals.
Both date range and individual filters influence this view.
If the check box to limit the number of tracks on the map is checked, tracks are shown only for the first 10 individuals (selected from the tag ids in ascending order).

![tracker_map](screenshots/tracker_map.png 'tracker_map')

**Time series plot**

This time series plot shows per default the daily distance moved (in meters) for the first individual in the dataset.
The dropdown can then be used to analyze different animals in this plot.
The plot is interactive and the user can zoom into specific regions of interest.

![time_series_plot](screenshots/time_series_plot.png 'time_series_plot')

Potential workflow:

1. Use the statistics table to identify individuals of interest that have been moving only a little or not at all throughout the last week for example
(use the date range filter to decide on a suitable date range).

2. Use the dropdown to filter for the animal of interest to explore its behavior on the map.
This could help to answer questions such as: Is the animal in the area of a potential nesting place? Are there any peculiarities in the recent movements?

3. Use the time series plot to spot any anomalous movement (as compared to the previous days/weeks/months):
Is it moving significantly less? Is this usual for the current season? Is there data missing or is there no movement at all?

This exemplary workflow might be helpful to spot stationary tags or other unusual animal behavior.

### Input data
MoveStack in Movebank format

### Output data
moveStack

### Artefacts
None.

### Settings
No predefined settings needed. The app allows to apply two filters on individuals and date ranges.

### Most common errors
None known yet.

### Null or error handling
Data is taken as is and only processed and aggregated for the visualizations.
