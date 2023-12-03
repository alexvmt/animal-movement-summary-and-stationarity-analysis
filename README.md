# Animal Movement Summary and Stationarity Analysis

MoveApps

Github repository: https://github.com/alexvmt/animal-movement-summary-and-stationarity-analysis

## Description

This app helps identify stationary animals in movement data.
A downloadable statistical movement summary table summarizes the given data to spot anomalous behavior,
a tracker map visualizes animal tracks, including clustering of locations,
and a zoomable time series plot  shows, for a selected individual, moved distances from one day to another.
The interface allows to filter for individuals and specific date ranges (such as last day / week / 30 days / 60 days / ... / last year).
It also allows to dynamically set maximum distance and minimum duration inputs for identifiying stationary individuals.
 
## Documentation

The app processes a given dataset and fills a dropdown with the given individuals (based on the dataset's respective id column).
This dropdown can be used to analyze a single animal of a tracked group of animals.
In addition, a date range filter allows to focus on different periods of time.
The date range filter reduces the data to the last n most recent days of observations.
You can go back in time as much as 365 days.

After the data is loaded, the following steps are executed to process the last 365 days per individual (the resulting data forms the basis for the app's content):
- construct dataframe from move object
- ensure CRS is EPSG:4326
- run several data transformation steps
- drop rows with missing or duplicated values
- filter data for last 365 days per individual
- order data and create lag variables
- calculate distance between coordinates in meters for each timestamp (using Vincenty ellipsoid great circle distance)
- calculate distance to last coordinates in meters for the timestamps within the last 10 days (to minimize computations) of available timestamps (using Vincenty ellipsoid great circle distance)
- trigger automatic data reduction if number of observations exceeds limit of 100000 observations

Loading and processing the data may take a moment, depending on the number of individuals, the total date range and the frequency of measurements.
This processed data is then filtered according to the selected date range and aggregated at a daily resolution.
Filtering and again aggregating data if the date range filter is changed may take a moment.
The aggregated data is not used for the tracker map but only for the statistics table and the time series plot.

**Stationarity detection**

The processed data in combination with the max. distance and min. duration inputs is used to detect stationary individuals.
The default is 100 meters for max. distance while the minimum is 1 meter and the maximum is 100000 meters.
The default is 24 hours for min. duration while the minimum is 1 hour and the maximum is 240 hours.
The tags' usual location error and users' experience can be used to set these inputs.
Given an individual's known locations,
it is considered stationary if it has not moved past the set max. distance within the set min. duration from its last known location.

**Automatic data recuction**

An automatic data reduction is triggered if the number of observations in the loaded dataset exceeds the limit of 100000 observations
in order to maintain the app's performance.
In case the automatic data reduction is triggered (a warning message is shown in the app),
each individual's first location is retained as well as all its locations within the last 10 days of its time series
because this period is the most interesting regarding end-of-track stationarity.
The data inbetween are reduced to one location per day by calculating the daily mean longitude and latitude.

**Further notes**

- Please note that the app performs best with rather small datasets, containing not too many individuals.
- This is mainly because calculating distances between coordinates is computation-intensive
when there are frequent location measurements (e. g. a fix every minute).
- Plotting many locations for many individuals on the map also slows the app down.
- Rendering the map and time series plot for the first time after starting the app may take some time.
- Sometimes there is no basemap data to be shown due to data availability issues.

### Detailed app description

The app consists of the following three components:
1. Statistics table: summarizing some key figures
2. Tracker map: showing tracks
3. Time series plot: showing moved distance in kilometers from one day to another (for a single selected animal)

**Statistics table**

This sortable (only one column can be used for this) table shows the following stats for each individual for the selected date range
(the individual filter does not influence the table):
- individual: animal id/name as of the loaded data
- first timestamp: first timestamp of the selected date range
- first location: first longitude and latitude of  the selected date range
- last timestamp: last timestamp of the selected date range
- last location: last longitude and latitude of  the selected date range
- #days w measures: number of days with measurements in the selected date range
- #days w/o measures: number of days without measurements in the selected date range (Are there fewer observations than days in the selected date range?)
- total distance (km): total distance moved in the selected date range (in kilometers)
- median distance (km): median distance moved per day in the selected date trange (in kilometers)
- avg. measures: average number of measurements per day in the selected date range
- var. measures: variance of number of measurements per day in the selected date range
- stationary: Is the animal stationary, considering the set max. diameter and min. duration inputs (yes or no)?

![statistics_table](screenshots/statistics_table.png 'statistics_table')

**Tracker map**

This map shows the movement of all animals or a single animal and their/its first/last coordinates on a map.
Last locations of stationary individuals have a black marker when all individuals are selected.
The start location has a white marker and the end location has a red marker when only a single individual is selected.
The user can activate/deactivate the plotted lines and points and can also change the type of the background map.
Locations are clustered dependent on the zoom level when only a single individual is selected.
The map can be used to identify different behavior patterns for different date ranges and differences between animals.
Both date range and individual filters influence this view.
If the check box to limit the number of tracks on the map is checked,
tracks are shown only for the first 10 individuals (selected from the dataset's respective id column in ascending order).

![tracker_map](screenshots/tracker_map.png 'tracker_map')

**Time series plot**

This time series plot shows per default the daily distance moved (in kilometers) for the first individual in the dataset.
The individual dropdown can then be used to analyze different animals in this plot.
The plot is interactive and the user can zoom into specific regions of interest.

![time_series_plot](screenshots/time_series_plot.png 'time_series_plot')

**Potential workflow**

1. After setting the max. distance and min. duration inputs, use the statistics table to identify individuals of interest that have been moving only a little or not at all throughout the last week for example
(use the date range filter to decide on a suitable date range). Are any individuals actually classified as stationary?

2. Use the dropdown to filter for the animal of interest to explore its behavior on the tracker map.
This could help to answer questions such as: Is the animal in the area of a potential nesting place? Are there any peculiarities in the recent movements?

3. Use the time series plot to spot any anomalous movement (as compared to the previous days/weeks/months):
Is it moving significantly less? Is this usual for the current season? Is there data missing or is there no movement at all?

This exemplary workflow might be helpful to spot stationary animals or other unusual animal behavior.

### Input data
MoveStack in Movebank format

### Output data
MoveStack

### Artefacts
None.

### Settings
No predefined settings needed.
The app allows to apply two filters - on individuals and date ranges.
The app also allows to dynamically set maximum distance and minimum duration inputs for the stationarity detection.

### Most common errors
None known yet.

### Null or error handling
Data is taken as is and only processed and aggregated for the visualizations.
