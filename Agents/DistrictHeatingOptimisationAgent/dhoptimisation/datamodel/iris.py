# Provide Constants/IRIs, which can be used to query and update the KG

from pyderivationagent.data_model.iris import ONTODERIVATION_ISDERIVEDFROM, \
                                              ONTODERIVATION_BELONGSTO, \
                                              ONTODERIVATION_DERIVATIONWITHTIMESERIES

# Namespaces
# External ontologies
OM =   'http://www.ontology-of-units-of-measure.org/resource/om-2/'
OWL =  'http://www.w3.org/2002/07/owl#'
RDF =  'http://www.w3.org/1999/02/22-rdf-syntax-ns#'
RDFS = 'http://www.w3.org/2000/01/rdf-schema#'
TIME = 'http://www.w3.org/2006/time#'
XSD =  'http://www.w3.org/2001/XMLSchema#'
# CoMo / CARES ontologies
ONTOCHEMPLANT = 'http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#'
ONTOCAPE =      'http://www.theworldavatar.com/ontology/ontocape/'
METAMODEL =     'http://www.theworldavatar.com/ontology/meta_model/'
ONTODERIV =     'https://www.theworldavatar.com/kg/ontoderivation/'
EMS =           'https://www.theworldavatar.com/kg/ontoems/'
TS =            'https://www.theworldavatar.com/kg/ontotimeseries/'
OHN =           'https://www.theworldavatar.com/kg/ontoheatnetwork/'
# Knowledge base
KB =            'https://www.theworldavatar.com/kg/pms_dh/'
# Derivation markup
DERIVATION_INSTANCE_BASE_URL = 'https://www.theworldavatar.com/kg/derivation/'


### OHN ###
# concepts
OHN_FIXED_COST = OHN + "FixedCost"
OHN_FIXED_WEAR_COST = OHN + "FixedWearCost"
OHN_FUEL_COST = OHN + "FuelCost"
OHN_FUEL_UNIT_COST = OHN + "FuelUnitCost"
OHN_GASTURBINE = OHN + "GasTurbine"
OHN_GAS_UNIT_COST = OHN + "GasUnitCost"
OHN_GRID_CHARGES = OHN + "GridCharges"
OHN_GRID_CONNECTION = OHN + "GridConnection"
OHN_HEATBOILER = OHN + "HeatBoiler"
OHN_HEATGENERATOR = OHN + "HeatGenerator"
OHN_HEATPROVIDER = OHN + "HeatProvider"
OHN_UNIT_PRICE = OHN + "UnitPrice"
OHN_HEATINGNETWORK = OHN + "HeatingNetwork"
OHN_HIGHER_CALORIFICVALUE = OHN + "HigherCalorificValue"
OHN_HOURLY_LABOUR_COST = OHN + "HourlyLabourCost"
OHN_HOURLY_WEAR_COST = OHN + "HourlyWearCost"
OHN_INCINERATIONPLANT = OHN + "IncinerationPlant"
OHN_LABOUR_COST = OHN + "LabourCost"
OHN_LOWER_CALORIFICVALUE = OHN + "LowerCalorificValue"
OHN_MUNICIPAL_UTILITY = OHN + "MunicipalUtility"
OHN_NATURAL_GAS = OHN + "NaturalGas"
OHN_SHUTDOWN_COST = OHN + "ShutDownCost"
OHN_STARTUP_COST = OHN + "StartUpCost"
OHN_SWITCHING_COST = OHN + "SwitchingCost"
OHN_TIER = OHN + "Tier"
OHN_TIERED_UNIT_PRICE = OHN + "TieredUnitPrice"
OHN_UNIT_RATE = OHN + "UnitRate"
OHN_VARIABLE_COST = OHN + "VariableCost"
OHN_VARIABLE_WEAR_COST = OHN + "VariableWearCost"
OHN_APPLICABLE_LOCATION = OHN + "applicableLocation"
OHN_APPLICABLE_OPEX_COMPONENT = OHN + "applicableOPEXComponent"
OHN_CONTRACT = "https://spec.edmcouncil.org/fibo/ontology/FND/Agreements/Contracts/Contract"
OHN_AVAILABILITY = OHN + "Availability"
OHN_CHP_BONUS = OHN + "CHPBonus"
OHN_CO2_CERTIFICATE_PRICE = OHN + "CO2CertificatePrice"
OHN_CO2_EMISSION_IN_TIME_INTERVAL = OHN + "CO2EmissionInTimeInterval"
OHN_CO2_FACTOR = OHN + "CO2Factor"
OHN_CALENDAR_EFFECT = OHN + "CalendarEffect"
OHN_CALORIFICVALUE = OHN + "CalorificValue"
OHN_COGEN_REVENUE_IN_TIME_INTERVAL = OHN + "CoGenRevenueInTimeInterval"
OHN_CONSUMER = OHN + "Consumer"
OHN_COST_IN_TIME_INTERVAL = OHN + "CostInTimeInterval"
OHN_DEMAND_DRIVEN_WEAR_COST = OHN + "DemandDrivenWearCost"
OHN_DURATION_IN_TIME_INTERVAL = OHN + "DurationInTimeInterval"
OHN_ELECTRICITY_SPOT_PRICE = OHN + "ElectricitySpotPrice"
OHN_EMISSION_COST = OHN + "EmissionCost"
OHN_PROVIDED_HEAT_AMOUNT = OHN + "ProvidedHeatAmount"
OHN_HEAT_DEMAND = OHN + "HeatDemand"
OHN_CONSUMED_GAS_AMOUNT = OHN + "ConsumedGasAmount" 
OHN_GENERATED_HEAT_AMOUNT = OHN + "GeneratedHeatAmount"
OHN_COGEN_ELECTRICITY_AMOUNT = OHN + "CoGenElectricityAmount"
# relationships
OHN_HAS_CO2_FACTOR = OHN + "hasCO2Factor"
OHN_HAS_COGEN_ELECTRICITY_AMOUNT = OHN + "hasCoGenElectricityAmount"
OHN_HAS_CONSUMED_GAS_AMOUNT = OHN + "hasConsumedGasAmount"
OHN_HAS_CUMULATIVE_ENERGYCAP = OHN + "hasCumulativeEnergyCap"
OHN_HAS_CURRENT_UNIT_PRICE = OHN + "hasCurrentUnitPrice"
OHN_HAS_DOWNSTREAM_GRIDCONNECTION = OHN + "hasDownstreamGridConnection"
OHN_HAS_GENERATED_HEAT_AMOUNT = OHN + "hasGeneratedHeatAmount"
OHN_HAS_HEATDEMAND = OHN + "hasHeatDemand"
OHN_HAS_HIGHER_CALORIFICVALUE = OHN + "hasHigherCalorificValue"
OHN_HAS_LOWER_CALORIFICVALUE = OHN + "hasLowerCalorificValue"
OHN_HAS_MAX_ANNUAL_PURCHASE_VOLUME = OHN + "hasMaxAnnualPurchaseVolume"
OHN_HAS_MAX_HOURLY_SUPPLY = OHN + "hasMaxHourlySupply"
OHN_HAS_MIN_ANNUAL_PURCHASE_VOLUME = OHN + "hasMinAnnualPurchaseVolume"
OHN_HAS_MIN_FLOWRATE = OHN + "hasMinFlowRate"
OHN_HAS_MIN_HOURLY_SUPPLY = OHN + "hasMinHourlySupply"
OHN_HAS_MIN_IDLE_TIME = OHN + "hasMinimumIdleTime"
OHN_HAS_MIN_THERMAL_LOAD = OHN + "hasMinimumThermalLoad"
OHN_HAS_OBSERVABLE_PROPERTY = OHN + "hasObservableProperty"
OHN_HAS_OPERATING_AVAILABILITY = OHN + "hasOperatingAvailability"
OHN_HAS_OPERATING_TIME = OHN + "hasOperatingTime"
OHN_HAS_PROVIDED_HEAT_AMOUNT = OHN + "hasProvidedHeatAmount"
OHN_HAS_PURCHASE_AGREEMENT = OHN + "hasPurchaseAgreement"
OHN_HAS_RATED_ELECTRICAL_POWER = OHN + "hasRatedElectricalPower"
OHN_HAS_RATED_THERMAL_POWER = OHN + "hasRatedThermalPower"
OHN_HAS_TIER = OHN + "hasTier"
OHN_HAS_TIERED_UNIT_PRICE = OHN + "hasTieredUnitPrice"
OHN_HAS_UNIT_PRICE = OHN + "hasUnitPrice"
OHN_HAS_UPSTREAM_GRIDCONNECTION = OHN + "hasUpstreamGridConnection"
OHN_IS_FULFILLED_BY = OHN + "isFulfilledBy"
OHN_OPERATES = OHN + "operates"
OHN_PROVIDES_HEAT_TO = OHN + "providesHeatTo"
OHN_SUPPLIES_HEAT_TO = OHN + "suppliesHeatTo"
OHN_IS_PUBLIC_HOLIDAY = OHN + "isPublicHoliday"
OHN_IS_VACATION = OHN + "isSchoolVacation"


### ONTOCAPE, ONTOCHEMPLANT, METAMODEL ###
ONTOCAPE_COSTS = ONTOCAPE + "chemical_process_system/CPS_performance/economic_performance.owl#Costs"
ONTOCAPE_HASCOST = ONTOCAPE + "chemical_process_system/CPS_performance/economic_performance.owl#hasCost"
ONTOCAPE_HASREVENUE = ONTOCAPE + "chemical_process_system/CPS_performance/economic_performance.owl#hasRevenue"
ONTOCAPE_THERMODYNAMICSTATEPROPERTY = ONTOCAPE + "material/phase_system/phase_system.owl#ThermodynamicStateProperty"
ONTOCAPE_ISOWNEROF = ONTOCAPE + "upper_level/system.owl#isOwnerOf"
ONTOCAPE_PROPERTY = ONTOCAPE + "upper_level/system.owl#Property"
METAMODEL_HASPART = METAMODEL + "mereology/mereology.owl#hasPart"
ONTOCHEMPLANT_HASFUELTYPE = ONTOCHEMPLANT + "hasFuelType"
ONTOCHEMPLANT_HASINDIVIDUALCO2EMISSION = ONTOCHEMPLANT + "hasIndividualCO2Emission"
ONTOCHEMPLANT_FUELTYPE = ONTOCHEMPLANT + "FuelType"

### ONTOEMS ###
EMS_AIRTEMPERATURE = EMS + "AirTemperature"

### TimeSeries ###
TS_TIMESERIES = TS + 'TimeSeries'
TS_FORECAST = TS + 'Forecast'
TS_HASTIMESERIES = TS + 'hasTimeSeries'
TS_HASFORECAST = TS + 'hasForecast'
TS_HASURL = TS + 'hasURL'
TS_HASRDB = TS + 'hasRDB'
TS_HASTIMEUNIT = TS + "hasTimeUnit"

### Time ###
TIME_INTERVAL = TIME + 'Interval'
TIME_HASBEGINNING = TIME + 'hasBeginning'
TIME_HASEND = TIME + 'hasEnd'
TIME_INSTANT = TIME + 'Instant'
TIME_INTIMEPOSITION = TIME + 'inTimePosition'
TIME_TIMEPOSITION = TIME + 'TimePosition'
TIME_HASTRS = TIME + 'hasTRS'
UNIX_TIME = 'http://dbpedia.org/resource/Unix_time'
TIME_NUMERICPOSITION = TIME + 'numericPosition'
TIME_HASTIME = TIME + 'hasTime'

### OM ###
OM_HAS_QUANTITY = OM + 'hasQuantity'
OM_QUANTITY = OM + "Quantity"
OM_AMOUNTOFMONEY = OM + "AmountOfMoney"
OM_COST = OM + "Cost"
OM_DURATION = OM + "Duration"
OM_SPECIFICHEATCAPACITY = OM + "SpecificHeatCapacity"
OM_TEMPERATURE = OM + "Temperature"
OM_PRESSURE = OM + "Pressure"
OM_DENSITY = OM + 'Density'
OM_VOLUMETRICFLOWRATE = OM + "VolumetricFlowRate"
OM_ENERGY = OM + "Energy"
OM_POWER = OM + "Power"
OM_HASVALUE = OM + 'hasValue'
OM_MEASURE = OM + "Measure"
OM_HAS_NUMERICAL_VALUE = OM + 'hasNumericalValue'
OM_HASUNIT = OM + 'hasUnit'
OM_UNIT = OM + "Unit"
OM_SYMBOL = OM + "symbol"
OM_KELVIN = OM + 'kelvin'
OM_TONNE = OM + 'tonne'
OM_HOUR = OM + 'hour-Sidereal'
OM_CELSIUS = OM + 'degreeCelsius'
OM_EURO = OM + 'euro'
OM_BAR = OM + 'bar'
OM_MEGAWATT = OM + 'megawatt'
OM_MEGAWATTHOUR = OM + 'megawattHour'
OM_KG_PER_M3 = OM + 'kilogramPerCubicmetre'
OM_KG_PER_S = OM + 'kilogramPerSecond-Time'
# custom units
OM_GIGAWATTHOUR = OM + 'gigawattHour'
OM_EURO_PER_TONNE = OM + 'euroPerTonne'
OM_EURO_PER_HOUR = OM + "euroPerHour"
OM_EURO_PER_MEGAWATTHOUR = OM + "euroPerMegawattHour"
OM_TONNE_PER_MEGAWATTHOUR = OM + "tonnePerMegawattHour"
OM_KILOWATTHOUR_PER_M3 = OM + "kilowattHourPerCubicMetre"
OM_KILOJOULE_PER_KELVIN_KILOGRAM = OM + "kilojoulePerKelvinKilogram"
OM_M3_PER_HOUR = OM + "cubicMetrePerHour"

### RDF, RDFS ###
RDF_TYPE = RDF + 'type'
RDFS_SUBCLASSOF = RDFS + 'subClassOf'
RDFS_LABEL = RDFS + 'label'

### Data types ###
XSD_STRING = XSD + 'string'
XSD_FLOAT = XSD + 'float'
XSD_DECIMAL = XSD + 'decimal'
XSD_DOUBLE = XSD + 'double'
