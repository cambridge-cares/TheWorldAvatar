################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 19 Oct 2022                            #
################################################

# The purpose of this module is to provide necessary constants for the TimeSeriesClient
# Please note: The TimeSeries data has been instantiated using the PropertySalesInstantiationAgent;
#              hence, the settings here need to match the one used by that agent
# For details on HM Land Registry's Price Paid Data PPD data, see:
# https://www.gov.uk/guidance/about-the-price-paid-data#explanations-of-column-headers-in-the-ppd

from avgsmprice.kgutils.javagateway import jpsBaseLibGW


# Dates from HM Land Registry are reported in xsd:gYearMonth, i.e. ISO 8601 YYYY-MM
# However, YearMonth not supported by TimeSeriesCLient RDB implementation
# --> Use to xsd:date, i.e. ISO 8601 YYYY-MM-DD
# TODO: only use as fall-back and query from KG in first place
TIME_FORMAT = 'YYYY-MM-DD'

### Create required JAVA classes ###

# Create data class for time entries (LocalDate)
# PostgreSQL supported data types: https://www.jooq.org/javadoc/dev/org.jooq/org/jooq/impl/SQLDataType.html
jpsBaseLibView = jpsBaseLibGW.createModuleView()
LocalDate = jpsBaseLibView.java.time.LocalDate
TIMECLASS = LocalDate.now().getClass()

# Create data class for all time series data (i.e. all data as double)
jpsBaseLibView = jpsBaseLibGW.createModuleView()
DATACLASS = jpsBaseLibView.java.lang.Double.TYPE

# OM / UOM unit symbols
GBP_PER_SM = '£ m-2'
