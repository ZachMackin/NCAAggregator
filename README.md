This package is designed so that you can take in college basketball data from [HoopR](https://github.com/sportsdataverse/hoopR-data/tree/main) and use that to construct aggregated predictions. The reason aggregated predictions are important, is because in many forecasting domains it is evident that aggregating many predicitions often performs better than even the best predictors. It lays the groundwork for this with a function that cleans and wrangles that data to get in a usuable format with all the desired metrics. It then has a variety of models that take in a row of that data to predict the score of the game, and aggregation functions to aggregate a list of predictions. The main workhorse aggregation function can take in both models and predictions and aggregate them with a variety of these aggregation functions. 

I encourage the user to implement their own functions, using the framework shown in functions.R, for use within the aggregator. 

For a couple examples of workflow within this project I highly reccomend reading the Vignette before using the package. 

```r
# Installation Methodology:
# Run the command below to install the package from GitHub
devtools::install_github("ZachMackin/NCAAggregator")
```

