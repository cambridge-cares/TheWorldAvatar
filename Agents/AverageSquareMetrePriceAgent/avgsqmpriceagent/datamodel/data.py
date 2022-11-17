################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 23 Oct 2022                            #
################################################

# The purpose of this module is to provide necessary data/constants for the 
# `avgsqmprice` agent (i.e. its TimeSeriesClient)
# Please note: The TimeSeries data has been instantiated using the PropertySalesInstantiationAgent;
#              hence, the settings here need to match the ones used by that agent
# For details on HM Land Registry's Price Paid Data PPD data, see:
# https://www.gov.uk/guidance/about-the-price-paid-data#explanations-of-column-headers-in-the-ppd

from avgsqmpriceagent.kg_operations.javagateway import jpsBaseLibGW


# Endpoint of Office for National Statistics (ONS) public SPARQL API
# (required to potentially retrieve nearby postcodes)
ONS_ENDPOINT = 'http://statistics.data.gov.uk/sparql'

# Dates from HM Land Registry are reported in xsd:gYearMonth, i.e. ISO 8601 YYYY-MM
# However, YearMonth not supported by TimeSeriesClient RDB implementation
# --> Used xsd:date, i.e. ISO 8601 YYYY-MM-DD, during instantiation (via PropertySalesInstantiationAgent)
TIME_FORMAT_LONG = '%Y-%m-%d'
TIME_FORMAT_SHORT = '%Y-%m'

# OM / UOM unit symbols
GBP_PER_SM = '£ m-2'


### Create required JAVA classes ###

# Create ONE JVM module view
jpsBaseLibView = jpsBaseLibGW.createModuleView()

# Create data class for time entries (LocalDate)
# PostgreSQL supported data types: https://www.jooq.org/javadoc/dev/org.jooq/org/jooq/impl/SQLDataType.html
LocalDate = jpsBaseLibView.java.time.LocalDate
TIMECLASS = LocalDate.now().getClass()

# Create data class for all time series data (i.e. all data as double)
DATACLASS = jpsBaseLibView.java.lang.Double.TYPE
