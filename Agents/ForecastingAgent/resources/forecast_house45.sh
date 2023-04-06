#!/bin/bash

# 1) Execute the following sparql query in the Blazegraph GUI and 
#    place result IRIs in list below
: '
prefix ts: <https://www.theworldavatar.com/kg/ontotimeseries/>
select ?dataIRI
where { ?dataIRI ts:hasTimeSeries ?tsIRI 
        FILTER CONTAINS(str(?dataIRI), "Monthly")
}
'

# 2) Place time series IRIs here to forecast all of them
iris=(
"<https://www.theworldavatar.com/kg/ontotimeseries/45utility/Attic_MonthlyElectricityConsumption_4b8d456e-2883-4714-a9b3-ab53e98987f0>"
"<https://www.theworldavatar.com/kg/ontotimeseries/45utility/FirstFloor_MonthlyElectricityConsumption_75eb0f9b-44f7-42fc-aa5a-e85c1ec5a893>"
"<https://www.theworldavatar.com/kg/ontotimeseries/45utility/GroundFloor_MonthlyElectricityConsumption_733372ef-de6d-42b1-bda4-06208924a664>"
"<https://www.theworldavatar.com/kg/ontotimeseries/45utility/MonthlyOilConsumption_837cf4af-000e-42bc-a82b-f507f262575c>"
"<https://www.theworldavatar.com/kg/ontotimeseries/45utility/MonthlyWaterConsumption_231a341b-17b2-4a40-9de8-0d38fe841ae2>"
)

for iri in ${iris[@]}
do
    echo Forecasting $iri
    # Forecast historical data (to compare with actual consumption)
    #curl -X POST -H 'Content-Type:application/json' -d '{"query":{"iri":"'$iri'", "use_model_configuration":"PIRMASENS", "forecast_start_date":"2018-01-31T00:00:00Z", "horizon":12}}' localhost:3838/forecastingAgent/forecast
    # Forecast into the future (as of last time step)
    curl -X POST -H 'Content-Type:application/json' -d '{"query":{"iri":"'$iri'", "use_model_configuration":"PIRMASENS", "horizon":12}}' localhost:3838/forecastingAgent/forecast
done

# 3) Execute the following sparql query in the Blazegraph GUI to
#    retrieve all forecasted time series
: '
prefix ts: <https://www.theworldavatar.com/kg/ontotimeseries/>
select ?dataIRI ?forecastIRI
where { ?dataIRI ts:hasForecast ?forecastIRI . 
        ?forecastIRI a ts:Forecast .
}
ORDER BY ?dataIRI
'