# Maximum Urban Heat

The maximum urban heat island calculations are based on the publication of [Theeuwes (2017)](https://rmets.onlinelibrary.wiley.com/doi/full/10.1002/joc.4717). 

The relationship holds for calm, fair weather conditions. Or for the days without disturbances of weather phenomena such as frontal systems or fog. Classified according to the rules used in Theeuwes(2017). The following rules are checked:

* If the hourly rainfall exceeds 0.3mm
* Or if the relative humidity is above 80%
* Or if the changes is the wind are more than 2m/s

In addition Temperatures below 17 degrees celcius are also excluded since heating of buildings could also introduce disturbances.

## Required input data

The equation requires:

* Sky view factor (value betwene 0-1) in the urban area
* Vegetation fraction (value betwene 0-1) in the urban area
* Temperature observations within the city for verification purposes
* Obervations outside of the city: rural reference station

From the autmaomatic weather stations (AWS) in the rural area the following data is required:

* 10 minute global radiation
* 10 minute wind speed at 10m
* 10 minute temperature at 1.5m
* 10 minute pressure derived to MSL
* 1 hour relative humidity
* 1 hour precipitation 
* 1 hour mean wind

The 10 minute data from global radiation, wind speed, temperature and pressure is used to calculate the UHImax. The hourly relative humidity, precipitation and mean wind are used to filter out synoptic situations with frontal system and fog.

