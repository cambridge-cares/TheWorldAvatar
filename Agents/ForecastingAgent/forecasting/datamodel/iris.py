################################################
# Authors: Magnus Mueller (mm2692@cam.ac.uk)   #
# Date: 30 Nov 2022                            #
################################################
# the purpose of this module is to provide iris for the KG, which can be used to query and update the KG
### Datamodel ###

 # Namespaces #
OHN = "https://www.theworldavatar.com/kg/ontoheatnetwork/"
ONTOCAPE = "http://www.theworldavatar.com/ontology/ontocape/"
METAMODEL = "http://www.theworldavatar.com/ontology/meta_model/"
OM = "http://www.ontology-of-units-of-measure.org/resource/om-2/"
ONTOEMS = "https://www.theworldavatar.com/kg/ontoems/"
ONTOPOWSYS = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#"
ONTOCHEMPLANT = "http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#"
TS = "https://www.theworldavatar.com/kg/ontotimeseries/"
RDF = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
RDFS = "http://www.w3.org/2000/01/rdf-schema#"
XSD = "http://www.w3.org/2001/XMLSchema#"
GEO = "http://www.bigdata.com/rdf/geospatial#"
GEOLIT = "http://www.bigdata.com/rdf/geospatial/literals/v1#"
OWL = "http://www.w3.org/2002/07/owl#"
WEATHER = "https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#"
M3L = "http://purl.org/iot/vocab/m3-lite#"
SIO = "http://semanticscience.org/resource/"
UOM = "http://theworldavatar.com/resource/ontouom/"
KB = "https://www.theworldavatar.com/kg/pms_dh/"
TIME = "http://www.w3.org/2006/time#"

### OHN ###
OHN_HASCO2FACTOR = OHN + "hasCO2Factor"
OHN_HASCOGENELECTRICITYAMOUNT = OHN + "hasCoGenElectricityAmount"
OHN_HASCONSUMEDGASAMOUNT = OHN + "hasConsumedGasAmount"
OHN_HASCUMULATIVEENERGYCAP = OHN + "hasCumulativeEnergyCap"
OHN_HASCURRENTUNITPRICE = OHN + "hasCurrentUnitPrice"
OHN_HASDOWNSTREAMGRIDCONNECTION = OHN + "hasDownstreamGridConnection"
OHN_HASGENERATEDHEATAMOUNT = OHN + "hasGeneratedHeatAmount"
OHN_HASHEATDEMAND = OHN + "hasHeatDemand"
OHN_HASHIGHERCALORIFICVALUE = OHN + "hasHigherCalorificValue"
OHN_HASLOWERCALORIFICVALUE = OHN + "hasLowerCalorificValue"
OHN_HASMAXANNUALPURCHASEVOLUME = OHN + "hasMaxAnnualPurchaseVolume"
OHN_HASMAXHOURLYSUPPLY = OHN + "hasMaxHourlySupply"
OHN_HASMINANNUALPURCHASEVOLUME = OHN + "hasMinAnnualPurchaseVolume"
OHN_HASMINFLOWRATE = OHN + "hasMinFlowRate"
OHN_HASMINHOURLYSUPPLY = OHN + "hasMinHourlySupply"
OHN_HASMINIMUMIDLETIME = OHN + "hasMinimumIdleTime"
OHN_HASMINIMUMTHERMALLOAD = OHN + "hasMinimumThermalLoad"
OHN_HASOBSERVABLEPROPERTY = OHN + "hasObservableProperty"
OHN_HASOPERATINGAVAILABILITY = OHN + "hasOperatingAvailability"
OHN_HASOPERATINGTIME = OHN + "hasOperatingTime"
OHN_HASPROVIDEDHEATAMOUNT = OHN + "hasProvidedHeatAmount"
OHN_HASPURCHASEAGREEMENT = OHN + "hasPurchaseAgreement"
OHN_HASRATEDELECTRICALPOWER = OHN + "hasRatedElectricalPower"
OHN_HASRATEDTHERMALPOWER = OHN + "hasRatedThermalPower"
OHN_HASTIER = OHN + "hasTier"
OHN_HASTIEREDUNITPRICE = OHN + "hasTieredUnitPrice"
OHN_HASUNITPRICE = OHN + "hasUnitPrice"
OHN_HASUPSTREAMGRIDCONNECTION = OHN + "hasUpstreamGridConnection"
OHN_ISFULFILLEDBY = OHN + "isFulfilledBy"
OHN_OPERATES = OHN + "operates"
OHN_PROVIDESHEATTO = OHN + "providesHeatTo"
OHN_SUPPLIESHEATTO = OHN + "suppliesHeatTo"
OHN_ISPUBLICHOLIDAY = OHN + "isPublicHoliday"
OHN_ISVACATION = OHN + "isVacation"
OHN_AVAILABILITY = OHN + "Availability"
OHN_CHPBONUS = OHN + "CHPBonus"
OHN_CO2CERTIFICATEPRICE = OHN + "CO2CertificatePrice"
OHN_CO2EMISSIONINTIMEINTERVAL = OHN + "CO2EmissionInTimeInterval"
OHN_CO2FACTOR = OHN + "CO2Factor"
OHN_CALENDAREFFECT = OHN + "CalendarEffect"
OHN_CALORIFICVALUE = OHN + "CalorificValue"
OHN_COGENREVENUEINTIMEINTERVAL = OHN + "CoGenRevenueInTimeInterval"
OHN_CONSUMER = OHN + "Consumer"
OHN_COSTINTIMEINTERVAL = OHN + "CostInTimeInterval"
OHN_DEMANDDRIVENWEARCOST = OHN + "DemandDrivenWearCost"
OHN_DURATIONINTIMEINTERVAL = OHN + "DurationInTimeInterval"
OHN_ELECTRICITYSPOTPRICE = OHN + "ElectricitySpotPrice"
OHN_EMISSIONCOST = OHN + "EmissionCost"
#OHN_ENERGYINTIMEINTERVAL = OHN + "EnergyInTimeInterval"
OHN_PROVIDEDHEATAMOUNT = OHN + "ProvidedHeatAmount"
OHN_HEATDEMAND = OHN + "HeatDemand"
OHN_CONSUMEDGASAMOUNT = OHN + "ConsumedGasAmount" 
OHN_GENERATEDHEATAMOUNT = OHN + "GeneratedHeatAmount"
OHN_COGENELECTRICITYAMOUNT = OHN + "CoGenElectricityAmount"

OHN_FIXEDCOST = OHN + "FixedCost"
OHN_FIXEDWEARCOST = OHN + "FixedWearCost"
OHN_FUELCOST = OHN + "FuelCost"
OHN_FUELUNITCOST = OHN + "FuelUnitCost"
OHN_GASTURBINE = OHN + "GasTurbine"
OHN_GASUNITCOST = OHN + "GasUnitCost"
OHN_GRIDCHARGES = OHN + "GridCharges"
OHN_GRIDCONNECTION = OHN + "GridConnection"
OHN_HEATBOILER = OHN + "HeatBoiler"
OHN_HEATGENERATOR = OHN + "HeatGenerator"
OHN_HEATPROVIDER = OHN + "HeatProvider"
OHN_HEATUNITPRICE = OHN + "HeatUnitPrice"
OHN_HEATINGNETWORK = OHN + "HeatingNetwork"
OHN_HIGHERCALORIFICVALUE = OHN + "HigherCalorificValue"
OHN_HOURLYLABOURCOST = OHN + "HourlyLabourCost"
OHN_HOURLYWEARCOST = OHN + "HourlyWearCost"
OHN_INCINERATIONPLANT = OHN + "IncinerationPlant"
OHN_LABOURCOST = OHN + "LabourCost"
OHN_LOWERCALORIFICVALUE = OHN + "LowerCalorificValue"
OHN_MUNICIPALUTILITY = OHN + "MunicipalUtility"
OHN_NATURALGAS = OHN + "NaturalGas"
OHN_SHUTDOWNCOST = OHN + "ShutDownCost"
OHN_STARTUPCOST = OHN + "StartUpCost"
OHN_SWITCHINGCOST = OHN + "SwitchingCost"
OHN_TIER = OHN + "Tier"
OHN_TIEREDUNITPRICE = OHN + "TieredUnitPrice"
OHN_UNITRATE = OHN + "UnitRate"
OHN_VARIABLECOST = OHN + "VariableCost"
OHN_VARIABLEWEARCOST = OHN + "VariableWearCost"
OHN_HASDATE = "http://nomisma.org/ontology#hasDate"
OHN_CONTRACT = "https://spec.edmcouncil.org/fibo/ontology/FND/Agreements/Contracts/Contract"

### ONTOCAPE ###
ONTOCAPE_HASCOST = ONTOCAPE + "chemical_process_system/CPS_performance/economic_performance.owl#hasCost"
ONTOCAPE_HASREVENUE = ONTOCAPE + "chemical_process_system/CPS_performance/economic_performance.owl#hasRevenue"
ONTOCAPE_ISOWNEROF = ONTOCAPE + "upper_level/system.owl#isOwnerOf"
ONTOCAPE_COSTS = ONTOCAPE + "chemical_process_system/CPS_performance/economic_performance.owl#Costs"
ONTOCAPE_THERMODYNAMICSTATEPROPERTY = ONTOCAPE + "material/phase_system/phase_system.owl#ThermodynamicStateProperty"
ONTOCAPE_PROPERTY = ONTOCAPE + "upper_level/system.owl#Property"

### METAMODEL ###
METAMODEL_HASPART = METAMODEL + "mereology/mereology.owl#hasPart"

### OM ###
OM_HASUNIT = OM + "hasUnit"
OM_HASVALUE = OM + "hasValue"
OM_HASNUMERICALVALUE = OM + "hasNumericalValue"
OM_SYMBOL = OM + "symbol"
OM_AMOUNTOFMONEY = OM + "AmountOfMoney"
OM_COST = OM + "Cost"
OM_DENSITY = OM + "Density"
OM_DURATION = OM + "Duration"
OM_ENERGY = OM + "Energy"
OM_MEASURE = OM + "Measure"
OM_POWER = OM + "Power"
OM_PRESSURE = OM + "Pressure"
OM_QUANTITY = OM + "Quantity"
OM_SPECIFICHEATCAPACITY = OM + "SpecificHeatCapacity"
OM_TEMPERATURE = OM + "Temperature"
OM_UNIT = OM + "Unit"
OM_VOLUMETRICFLOWRATE = OM + "VolumetricFlowRate"
OM_CUBICMETREPERSECOND = OM + 'cubicMetrePerSecond-Time'
OM_TONNE = OM + 'tonne'
OM_HOUR = OM + 'hour-Sidereal'
OM_MEGAWATT = OM + 'megawatt'
OM_DEGREE = OM + 'degreeCelsius'
OM_EURO = OM + 'euro'
OM_BAR = OM + 'bar'
OM_MEGAWATTHOUR = OM + 'megawattHour'
OM_KGPERM3 = OM + 'kilogramPerCubicMetre'
OM_KJPERKGPERK = OM + 'kilojoulePerKilogramPerKelvin'
OM_EUROPERMEGAWATTHOUR = OM + 'euroPerMegawattHour' 

### ONTOEMS ###
ONTOEMS_AIRTEMPERATURE = ONTOEMS + "AirTemperature"

### ONTOPOWSYS ###
ONTOPOWSYS_THERMALENERGYSTORAGE = ONTOPOWSYS + "ThermalEnergyStorage"

### ONTOCHEMPLANT ###
ONTOCHEMPLANT_HASFUELTYPE = ONTOCHEMPLANT + "hasFuelType"
ONTOCHEMPLANT_HASINDIVIDUALCO2EMISSION = ONTOCHEMPLANT + "hasIndividualCO2Emission"
ONTOCHEMPLANT_FUELTYPE = ONTOCHEMPLANT + "FuelType"

### TS ###
TS_HASTIMESERIES = TS + "hasTimeSeries"
TS_HASINPUTTIMEINTERVAL = TS + "hasInputTimeInterval"
TS_HASOUTPUTTIMEINTERVAL = TS + "hasOutputTimeInterval"
TS_HASFORECAST = TS + "hasForecast"
TS_FORECAST = TS + "Forecast"
TS_HASRDB = TS + "hasRDB"
TS_HASTIMEUNIT = TS + "hasTimeUnit"
TS_TIMESERIES = TS + "TimeSeries"
TS_CREATEDAT = TS + "createdAt"

TS_HASURL = TS + "hasURL"
TS_FORECASTINGMODEL = TS + "ForecastingModel"
TS_HASCOVARIATE = TS + "hasCovariate"
TS_HASFORECASTINGMODEL = TS + "hasForecastingModel"
TS_HASTRAININGTIMESERIES = TS + "hasTrainingTimeSeries"

# Data types
RDF_TYPE = RDF + 'type'
RDFS_SUBCLASSOF = RDFS + 'subClassOf'
RDFS_COMMENT = RDFS + 'comment'
RDFS_LABEL = RDFS + 'label'
XSD_STRING = XSD + 'string'
XSD_FLOAT = XSD + 'float'
XSD_DATETIME = XSD + 'dateTime'
XSD_INTEGER = XSD + 'integer'
XSD_DOUBLE = XSD + 'double'
XSD_BOOLEAN = XSD + 'boolean'
XSD_DATE = XSD + "date"
XSD_DATETIMESTAMP = XSD + "dateTimeStamp"

### time interval ###
TIME_HASBEGINNING = TIME + 'hasBeginning'
TIME_INTERVAL = TIME + 'Interval'
TIME_INSTANT = TIME + 'Instant'
TIME_HASEND = TIME + 'hasEnd'
TIME_HASTIME = TIME + 'hasTime'
TIME_HASTRS = TIME + 'hasTRS'
TIME_INTIMEPOSITION = TIME + 'inTimePosition'
TIME_NUMERICPOSITION = TIME + 'numericPosition'
TIME_TIMEPOSITION = TIME + 'TimePosition'
UNIX_TIME = "http://dbpedia.org/resource/Unix_time"

TIME_TRS = TIME + 'TRS'
TIME_TEMPORALPOSITION = TIME + 'TemporalPosition'
TIME_TEMPORALUNIT = TIME + 'TemporalUnit'
TIME_NUMERICDURATION = TIME + 'numericDuration'
TIME_HASAVERAGINGPERIOD = TIME + 'hasAveragingPeriod'
TIME_UNITTYPE = TIME + 'unitType'
