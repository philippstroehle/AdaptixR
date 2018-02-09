

# AdaptixR
A package of R functions to interact with the [Adaptix](https://www.sensewaves.io/technology/) platform API by [Sensewaves](https://www.sensewaves.io). 

##### Requirements

You will need an Adaptix account to use this R package. To get one, [get in touch](mailto:contact@sensewaves.com). 

### install

Currently only available through this repo.


```r
devtools::install_github("Sensewaves/AdaptixR")
```


### usage


```r
library(AdaptixR)
```

##### Connection


```r
conn <- AdaptixConnection(url = "https://alpha.adaptix.io/api", api.key = "your_adaptix_key")
```

##### Push data


```r
# Create a stream
my.new.stream <- AdaptixCreateStream(conn = conn, 
				     name = "my_new_stream")
# Create a data frame with some time series data - timestamps must be ISO8601 compliant
points.df <- data.frame(at = ConvertDateToISO8601(c("2017-01-01", "2017-01-02", "2017-01-03", "2017-01-04", "2017-01-05")), 
                        value = c(11.5, 43.0, 15.4, 41.1, 12.2))						 
# Publish the points 	in the stream
AdaptixPublishPoints(conn = conn, stream = my.new.stream$id, points = points.df) 
# [1] "https://alpha.adaptix.io/api/streams/{stream id}/points"
```


##### Pull data

```r
AdaptixGetPoints(conn = conn, stream = my.new.stream$id)
#   value         at
# 1   1.0 2017-01-01
# 2   1.1 2017-01-02
# 3   1.2 2017-01-03
```

##### Analyze data
```r
# Request a forecast for the next 48h on our stream
forecast.response <- AdaptixForecast(conn = conn, 
				     stream = my.new.stream$id, 
				     span = "48h")
# Parse forecasted scenarios
AdaptixForecastPointsToDataFrame(AdaptixGetForecastScenarios(forecast.response))
#       1         at
# 1 43.00 2017-01-05
# 2 28.25 2017-01-06
# 	...
```

