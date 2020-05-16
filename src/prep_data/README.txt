This file describes the process used to clean, transform and combine the LIDAR observations and the Meterological Observations and to output the final data set used in the analysis. 

The output files are produced for a series of time-averaged windows consisting of the following.  

    "1min",
    "5min",
    "10min",
    "30min",
    "1hour",
    "1day"

The following files should be contained in a single directory and executed in order. The two transform functions are not required to be run independently. 

main_data_clean.r -- Used to combine the .nc file data from multiple observation days. 
primary_clean.r -- Cleans the meterological data in the "primary" files
main_primary_combine.r-- Combines the LIDAR observation data and the meteorological data from the primary files
after the cleaning process. 
nc_transform.r -- provides support functions to the above files
ts_transform.r -- provides support functions to the above files