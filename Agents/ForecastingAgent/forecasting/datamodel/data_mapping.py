################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 16 Oct 2022                            #
################################################

# The purpose of this module is to provide a mapping between retrieved 
# HM Land Registry's Price Paid Data property types and instantiated property
# types according to OntoBuiltEnv

# For details on PPD data, see:
# https://www.gov.uk/guidance/about-the-price-paid-data#explanations-of-column-headers-in-the-ppd

from forecasting.datamodel.iris import *
from forecasting.utils.useful_queries import get_df_for_heat_supply, get_df_no_covariates

from forecasting.kgutils.javagateway import jpsBaseLibGW

mapping_type_data_function = {
    'Default': get_df_no_covariates,
    OHN_CONSUMER : get_df_for_heat_supply}


# Dates from HM Land Registry are reported in xsd:gYearMonth, i.e. ISO 8601 YYYY-MM
# However, YearMonth not supported by TimeSeriesCLient RDB implementation
# --> Use to xsd:date, i.e. ISO 8601 YYYY-MM-DD
TIME_FORMAT = 'YYYY-MM-DD'
TIME_FORMAT_TS = "YYYY-MM-DDThh:mm:ssZ"

### Create required JAVA classes ###

# Create data class for time entries (LocalDate)
# PostgreSQL supported data types: https://www.jooq.org/javadoc/dev/org.jooq/org/jooq/impl/SQLDataType.html
jpsBaseLibView = jpsBaseLibGW.createModuleView()
Instant = jpsBaseLibView.java.time.Instant
TIMECLASS = Instant.now().getClass()

# Create data class for all time series data (i.e. all data as double)
jpsBaseLibView = jpsBaseLibGW.createModuleView()
DATACLASS = jpsBaseLibView.java.lang.Double.TYPE
DOUBLE = jpsBaseLibView.java.lang.Double.TYPE
INTEGER = jpsBaseLibView.java.lang.Integer.TYPE
BOOLEAN = jpsBaseLibView.java.lang.Boolean.TYPE