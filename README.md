# HW5_Brenimer_Suzanne
STAA566 HW5 - Shiny App

For this assignment I wanted to continue exploring ways to present the traffic incidents data available from Denver OpenData.
In essence, my goal is to offer the user ways of visualizing where the traffic accident hotspots are in Denver, and provide sort of a (admittedly crude) measure of neighborhood and/or roadway safety.  The data set is quite rich, and contains data for all reported incidents going back to 2013 at the time of this writing, and is updated regularly with current information. It contains many features, such as lighting conditions and roadway conditions, which I was not able to include in this dashboard, but would be useful for expanding this in the future.

The user input in this dashboard is limited to selection of one or more Denver neighborhoods. Originally I wanted to enable the user to select a custom date range, but given the large size of the original data set I found it more expedient for this assignment to subset the data prior to user input, so that the visualizations would not be too slow.

Additional features for future expansion (i.e. that I was not able to get working prior to submission) are plots showing number of traffic incidents (and/or fatalities) by month of year, and day of week.