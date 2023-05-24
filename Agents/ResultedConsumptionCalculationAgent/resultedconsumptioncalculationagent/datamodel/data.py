################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk)        #    
# Date: 17 May 2023                            #
################################################


from resultedconsumptioncalculationagent.kg_operations.javagateway import jpsBaseLibGW

TIME_FORMAT_LONG = '%Y-%m-%d %H:%M:%S'
TIME_FORMAT_SHORT = '%Y-%m'

# OM / UOM unit symbols
# NOTE: There are reported issues with encoding of special characters, i.e. Blazegraph
#       claiming to use utf-8 encoding while actually using iso-8859-1
#       --> PoundSterling displayed wrongly in GUI but corrected when retrieved in code
# Details: https://github.com/blazegraph/database/issues/224
GBP_SYMBOL = '£'

# Dictionary to convert datetime (month) to index in tensor
date_dict = {'2020-01-01T12:00:00.000Z':0,\
             '2020-02-01T12:00:00.000Z':1,\
             '2020-03-01T12:00:00.000Z':2,\
             '2020-04-01T12:00:00.000Z':3,\
             '2020-05-01T12:00:00.000Z':4,\
             '2020-06-01T12:00:00.000Z':5,\
             '2020-07-01T12:00:00.000Z':6,\
             '2020-08-01T12:00:00.000Z':7,\
             '2020-09-01T12:00:00.000Z':8,\
             '2020-10-01T12:00:00.000Z':9,\
             '2020-11-01T12:00:00.000Z':10,\
             '2020-12-01T12:00:00.000Z':11,
             '2020-01-01T12:00:00Z':0,\
             '2020-02-01T12:00:00Z':1,\
             '2020-03-01T12:00:00Z':2,\
             '2020-04-01T12:00:00Z':3,\
             '2020-05-01T12:00:00Z':4,\
             '2020-06-01T12:00:00Z':5,\
             '2020-07-01T12:00:00Z':6,\
             '2020-08-01T12:00:00Z':7,\
             '2020-09-01T12:00:00Z':8,\
             '2020-10-01T12:00:00Z':9,\
             '2020-11-01T12:00:00Z':10,\
             '2020-12-01T12:00:00Z':11}

### Create required JAVA classes ###

# Create ONE JVM module view
jpsBaseLibView = jpsBaseLibGW.createModuleView()

# Create data class for time entries (LocalDate)
# PostgreSQL supported data types: https://www.jooq.org/javadoc/dev/org.jooq/org/jooq/impl/SQLDataType.html
LocalDate = jpsBaseLibView.java.time.LocalDate
TIMECLASS = LocalDate.now().getClass()

# Create data class for all time series data (i.e. all data as double)
DATACLASS = jpsBaseLibView.java.lang.Double.TYPE
