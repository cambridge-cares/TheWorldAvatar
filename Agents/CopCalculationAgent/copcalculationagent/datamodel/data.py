################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk)        #    
# Date: 17 May 2023                            #
################################################


from copcalculationagent.kg_operations.javagateway import jpsBaseLibGW

#TIME_FORMAT = '%Y-%m-%dT%H:%M:%S.%sZ'
TIME_FORMAT = '%Y-%m-%dT%H:%M:%SZ'

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
# LocalDate = jpsBaseLibView.java.time.LocalDate
# TIMECLASS = LocalDate.now().getClass()

Instant = jpsBaseLibView.java.time.Instant
TIMECLASS = Instant.now().getClass()

# Create data class for all time series data (i.e. all data as double)
DATACLASS = jpsBaseLibView.java.lang.Double.TYPE
