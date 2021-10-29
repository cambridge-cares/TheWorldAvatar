#!/bin/sh

set -e

create_postgres_database() {
  db=${1}
  type=${2}
  
  # Create the db
  echo "SELECT 'CREATE DATABASE ${db}' WHERE NOT EXISTS (SELECT FROM pg_database WHERE datname = '${db}')\gexec" | psql -h $POSTGRES_HOST -U $POSTGRES_USER -w

  if [[ "${type:0:7}" == "postgis" ]] ; then
    # Load PostGIS into database
    echo "Loading base PostGIS extensions into $db"
    psql -h $POSTGRES_HOST -U $POSTGRES_USER -w --dbname="${db}" <<-'EOSQL'
        CREATE EXTENSION IF NOT EXISTS postgis;
        CREATE EXTENSION IF NOT EXISTS postgis_topology;
        CREATE EXTENSION IF NOT EXISTS fuzzystrmatch;
        CREATE EXTENSION IF NOT EXISTS postgis_tiger_geocoder;
EOSQL
  fi
  
  if [[ "$type" = "postgis_raster" ]] ; then
    # Load PostGIS into database
    echo "Loading PostGIS raster extension into $db"
    psql -h $POSTGRES_HOST -U $POSTGRES_USER -w --dbname="${db}" <<-'EOSQL'
        CREATE EXTENSION IF NOT EXISTS postgis_raster;
EOSQL

  fi
}

export PGPASSFILE=/tmp/postgres.pgpass
# Populate .pgpass file
# hostname:port:database:username:password
echo ${POSTGRES_HOST}:${POSTGRES_PORT}:*:${POSTGRES_USER}:$(cat ${POSTGRES_PASSWORD_FILE}) > $PGPASSFILE
chmod 0600 $PGPASSFILE

# Read in the configuration files
for c in ${CONFIG_DIR}/*${c}.conf; do
  
  CSV_ENABLED=0
  SHP_ENABLED=0
  TIF_ENABLED=0

  echo $c
  . $c

  if [ $g_CSV_ENABLED -eq 1 ] && [ $CSV_ENABLED -eq 1 ]; then
    echo loading csv files
    
#    create_postgres_database $CSV_DB 

    if [ -z "$CSV_SQL_STATEMENT_TRANSFORM" ]; then
      CSV_SQL_STATEMENT_TRANSFORM=cat
    else
      CSV_SQL_STATEMENT_TRANSFORM=${CONFIG_DIR}/$CSV_SQL_STATEMENT_TRANSFORM
    fi

    for xls in ${DATA_DIR}/${DATASET_DIR}/*.{xls,xlsx,XLS,XLSX}; do
      echo $xls
      # https://csvkit.readthedocs.io/en/1.0.6/scripts/in2csv.html
 #     in2csv $xls
    done

    #
    for csv in ${DATA_DIR}/${DATASET_DIR}/*.csv; do
      echo $csv
      # https://csvkit.readthedocs.io/en/1.0.6/scripts/csvsql.html
      csvsql --overwrite --insert --db-schema $CSV_SCHEMA --db "postgresql://${POSTGRES_USER}:$(cat ${POSTGRES_PASSWORD_FILE})@${POSTGRES_HOST}/${CSV_DB}" $csv
    done
  fi

  if [ $g_SHP_ENABLED -eq 1 ] && [ $SHP_ENABLED -eq 1 ]; then
    echo loading shp files
    
    create_postgres_database $SHP_DB postgis
    
    if [ -z "$SHP_SQL_STATEMENT_TRANSFORM" ]; then
      SHP_SQL_STATEMENT_TRANSFORM=cat
    else
      SHP_SQL_STATEMENT_TRANSFORM=${CONFIG_DIR}/$SHP_SQL_STATEMENT_TRANSFORM
    fi
    
    # Add shape files to .sql file
    mode=d # For the first shp file drop any existing table with the name defined by "${SHP_SCHEMA}.${SHP_TABLE}"
    for shp in ${DATA_DIR}/${DATASET_DIR}/*.shp; do
      echo $shp
      
      shp2pgsql -${mode} -s ${SHP_EPSG_FROM} -D -I $shp ${SHP_SCHEMA}.${SHP_TABLE} |
      ${SHP_SQL_STATEMENT_TRANSFORM} |
      psql -h $POSTGRES_HOST -d $SHP_DB -U $POSTGRES_USER -w
      
      mode=a # Just append the data from subsequent shp files
    done

  else
    echo not loading shp files
  fi

  if [ $g_TIF_ENABLED -eq 1 ] && [ $TIF_ENABLED -eq 1 ]; then
    echo loading tifs
    
    create_postgres_database $TIF_DB postgis_raster

    # Add geotiff files to database
    mode=d
    raster2pgsql -C -I -F -Y -M -R -${mode} -s ${TIF_EPSG_FROM} -t auto ${DATA_DIR}/${DATASET_DIR}/*.tif ${TIF_SCHEMA}.${TIF_TABLE} | psql -h $POSTGRES_HOST -d $DB -U $POSTGRES_USER -w

  else
    echo not loading tifs
  fi
done
