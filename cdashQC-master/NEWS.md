
# cdashQC Version 0.1.2 (Date: 2016-11-21)
--------------
1. Every raw data need to go through a `create_` step for data preparation
      -  `create_dem` for demographics output
      -  `create_aet` for adverse events
      -  `create_lab` for lab test
      -  `create_vs` for vital signs
      -  `create_eg` for ECGs
2. a sequence of functions use to deal with replicates for EG and VS.
      -  `guess_reps` guess the number of replicates there should have for each time point. 
      -  `guess_test` automatically decide whether the input data is EG or VS.
      -  `replicate_check` report by-subject rechecks/missing values for each time point 
      -  `replicate_data`  Split the replicates into "clean" (having correct replicates) and "dirty" (having wrong number of replicates)
      -  `replicate_clean` clean the "dirty" data and combine it with "clean" data 
      
3. Table summaries
      -  `summary_dem` summary statistics for demographics 
      -  `summary_lab` summary statistics (mean, SD, cv, min, Q1, median, Q3, max) for lab test
      -  `summary_labshift` lab shift table 
      -  `summary_vs_eg` summary statistics (mean, SD, cv, min, Q1, median, Q3, max) for vs and eg
      -  `replicate_average` get the averages if there are replicate for the time point.
      
4. baselines
      -  `create_phour` create protocol hour if it does not exist 
      -  `guess_base_phour`  guess the baseline time point 
      -  `create_baseline`   for the input data, create an extra column for baseline indicator
      

# cdashQC version 0.1.1
-------------

1.  re-structured `labshift.R` .
2.  rewrote `cdash_ae` function:
    -   `ae1()` argument `bytrt=` is removed.
    -   `ae2()` argument `bytrt=` is removed.
    -   `ae3()` argument `bytrt=` is removed.
    -   fixed an issue in `create_aet()` function that causes "ERROR:
        You are losing observations (Duplicates Maybe?)"

3.  rewrote `cdash_dem.R` :
    -   `dem_summary()` argument `bytrt=` is removed.
    -   `dem_listing()` argument `bytrt=` is removed.

4.  complete `new_create_included.R`.

version 0.1.0
-------------

-   Create the TFLs for QC of cdash format data.
-   currently supports the following tables (or listings):
    -   demographics listing
    -   demographics summary
    -   Adverse Event (AE1, AE2 and AE3)
    -   Lab shift table
    -   Lab out of range values
    -   Vital Signs listing
    -   ECG listing
    -   Concomitant medication listing
    -   Other useful functions
