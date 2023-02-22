################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 14 Nov 2022                            #
################################################

# The purpose of this module is to provide necessary data/constants for the 
# `propertyvalueestimation` agent (i.e. its TimeSeriesClient)
# Please note: The TimeSeries data has been instantiated using the PropertySalesInstantiationAgent;
#              hence, the settings here need to match the ones used by that agent
# For details on HM Land Registry's Price Paid Data PPD data, see:
# https://www.gov.uk/guidance/about-the-price-paid-data#explanations-of-column-headers-in-the-ppd

from toyagent.kg_operations.javagateway import jpsBaseLibGW


# Dates from HM Land Registry are reported in xsd:gYearMonth, i.e. ISO 8601 YYYY-MM
# However, YearMonth not supported by TimeSeriesClient RDB implementation
# --> Used xsd:date, i.e. ISO 8601 YYYY-MM-DD, during instantiation (via PropertySalesInstantiationAgent)
TIME_FORMAT_LONG = '%Y-%m-%d'
TIME_FORMAT_SHORT = '%Y-%m'

# OM / UOM unit symbols
# NOTE: There are reported issues with encoding of special characters, i.e. Blazegraph
#       claiming to use utf-8 encoding while actually using iso-8859-1
#       --> PoundSterling displayed wrongly in GUI but corrected when retrieved in code
# Details: https://github.com/blazegraph/database/issues/224
GBP_SYMBOL = '£'


### Create required JAVA classes ###

# Create ONE JVM module view
jpsBaseLibView = jpsBaseLibGW.createModuleView()

# Create data class for time entries (LocalDate)
# PostgreSQL supported data types: https://www.jooq.org/javadoc/dev/org.jooq/org/jooq/impl/SQLDataType.html
LocalDate = jpsBaseLibView.java.time.LocalDate
TIMECLASS = LocalDate.now().getClass()

# Create data class for all time series data (i.e. all data as double)
DATACLASS = jpsBaseLibView.java.lang.Double.TYPE
