#!/bin/bash

# Ensure the script exits if one of the sub-processes fails
set -e

ensure_scripts_executable() {
  find "${1}" -name "*.sh" -exec chmod a+x {} \;
}
ensure_unix_line_endings() {
  sed -i 's/\r$//' "${1}"
}

curl_test(){
  HTTP_RESPONSE="$(curl --silent --write-out "HTTPSTATUS:%{http_code}" -u "${ADMIN_USER}:${ADMIN_PASSWORD}" "$@")"

  # extract the status
  HTTP_STATUS=$(echo $HTTP_RESPONSE | tr -d '\n' | sed -e 's/.*HTTPSTATUS://')

  # example using the status
  test $HTTP_STATUS -eq 200
}

curl_wrapper(){
  HTTP_RESPONSE="$(curl --silent --write-out "HTTPSTATUS:%{http_code}" -u "${ADMIN_USER}:${ADMIN_PASSWORD}" "$@")"

  # extract the body
  HTTP_BODY=$(echo $HTTP_RESPONSE | sed -e 's/HTTPSTATUS\:.*//g')

  # extract the status
  HTTP_STATUS=$(echo $HTTP_RESPONSE | tr -d '\n' | sed -e 's/.*HTTPSTATUS://')

  # example using the status
  if [ $HTTP_STATUS -ge 300 ]; then
    >&2 echo "Error [HTTP status: $HTTP_STATUS]"
    # print the body
    >&2 echo "$HTTP_BODY" | tr -d '\r'
    exit 1
  fi
  
  # print the body
  echo "$HTTP_BODY" | tr -d '\r'
}

create_postgres_database() {
  db=${1}
  # Create the db
  echo "SELECT 'CREATE DATABASE ${db}' WHERE NOT EXISTS (SELECT FROM pg_database WHERE datname = '${db}')\gexec" | psql -h "$POSTGRES_HOST" -p "$POSTGRES_PORT" -U "$POSTGRES_USER" -w

  if [ -n ${GEOSERVER_HOST} ]; then
    if ! curl_test "http://${GEOSERVER_HOST}:${GEOSERVER_PORT}/geoserver/rest/workspaces/${WORKSPACE}" ; then
      echo "Adding new workspace ..."
      workspace_name="$(curl_wrapper -X POST -H "content-type: application/json" -d "$(envsubst < geoservertemplates/workspace.json)" "http://${GEOSERVER_HOST}:${GEOSERVER_PORT}/geoserver/rest/workspaces?default=true")"
      echo "Added workspace '${workspace_name}'"
    fi
    if ! curl_test "http://${GEOSERVER_HOST}:${GEOSERVER_PORT}/geoserver/rest/workspaces/${WORKSPACE}/datastores/$DB" ; then
      curl_wrapper -X POST -H "content-type: application/json" -d "$(envsubst < geoservertemplates/stores/postgres_store.json)" "http://${GEOSERVER_HOST}:${GEOSERVER_PORT}/geoserver/rest/workspaces/${WORKSPACE}/datastores"
    fi
  fi

}
install_postgis_extensions() {
  db=${1}
  # Load PostGIS into database
  echo "Loading base PostGIS extensions into $db"
  psql -h "$POSTGRES_HOST" -p "$POSTGRES_PORT" -U "$POSTGRES_USER" -d "$db" -w <<-'EOSQL'
      CREATE EXTENSION IF NOT EXISTS postgis;
      CREATE EXTENSION IF NOT EXISTS postgis_topology;
      CREATE EXTENSION IF NOT EXISTS fuzzystrmatch;
      CREATE EXTENSION IF NOT EXISTS postgis_tiger_geocoder;
EOSQL
}
install_postgis_raster_extensions() {
  db=${1}
  # Load PostGIS into database
  echo "Loading PostGIS raster extension into $db"
  psql -h "$POSTGRES_HOST" -p "$POSTGRES_PORT" -U "$POSTGRES_USER" -w --dbname="${db}" <<-'EOSQL'
      CREATE EXTENSION IF NOT EXISTS postgis_raster;
      SET SESSION postgis.enable_outdb_rasters = True;
      SET SESSION postgis.gdal_enabled_drivers = 'ENABLE_ALL';
EOSQL
}
sanitise_file_name()
{
  # Extract the file name and replace most non-alphanumeric characters with "_"
  # "echo -n" is not POSIX compliant but is widely supported
  echo -n "$(basename -- "$1")" | tr -c '[:alnum:]._' '_'
}
prepare_tabular_files() {
  in_base_dir="$1"
  in_files_map="$2"
  out_dir="$3"

  # Ensure the output directory exists
  mkdir -p "$out_dir"

  # Temporarily set the for-loop delimiter to the newline character
  SAVEIFS1="$IFS"
  IFS="
"
  for in_file_entry in ${in_files_map}; do
    # Extract the relative path of the source file.
    # It's the part before the ":" if sheets are also specified
    # or the whole string if no ":" is present
    in_file_rel_path="${in_file_entry%:*}"
    # Check to see if the entry contains sheets that need extracting
    if [ "$in_file_rel_path" = "${in_file_entry#*:}" ]; then
      # No sheets listed so just create a hard link to the file in the output folder with a sanitised name
      out_filename="$(sanitise_file_name "$in_file_rel_path")"
      ln -f "$in_base_dir/$in_file_rel_path" "$out_dir/$out_filename"
    else
      # Temporarily set the for-loop delimiter to the ";" character
      SAVEIFS2="$IFS"
      IFS=";"
      # Loop over named sheets (after the ":" and separated by ";") and extract them to CSV files with sanitised names
      for sheet in ${in_file_entry#*:}; do
        out_filename="$(sanitise_file_name "${sheet}".csv)"
        in2csv --sheet "$sheet" "$in_base_dir/$in_file_rel_path" > "$out_dir/$out_filename"
      done
      IFS="$SAVEIFS2"
    fi
  done
  IFS="$SAVEIFS1"
}
get_value() {
  map="$1"
  key="$2"

  # Temporarily set the for-loop delimiter to the newline character
  SAVEIFS="$IFS"
  IFS="
"
  # Loop over the "key:value" pairs until one with the matching key is found
  for pair in $map; do
    if [ "$key" = "$(sanitise_file_name "${pair%:*}")" ]; then
      value="${pair#*:}"
      # Echo the value to "return" it
      echo "$value"
      break
    fi
  done

  IFS="$SAVEIFS"
}
upload_csv() {
  # Read in the file and apply transformation to its content, if one is specified
  "$csv_transform" < "$csv" |

  # https://csvkit.readthedoc.io/en/1.0.6/scripts/csvsql.html
  csvsql --overwrite --insert --db-schema "$CSV_SCHEMA" --db postgresql://"${POSTGRES_USER}"@"${POSTGRES_HOST}":"${POSTGRES_PORT}"/"${DB}" --tables "$tablename"

  # Run SQL transformation, if one is specified
  SQL_SPECIFIC_TRANSFORM=$(get_value "$SQL_SPECIFIC_TRANSFORMS" "$tablename")
  if [ -n "$SQL_SPECIFIC_TRANSFORM" ]; then
    psql -h "$POSTGRES_HOST" -p "$POSTGRES_PORT" -U "$POSTGRES_USER" -d"${DB}" -w < "${CONFIG_DIR}/${SQL_SPECIFIC_TRANSFORM}"
  fi
}

transform_rasters_to_geotiffs() {
  first_tiff="TRUE"

  find "${DATA_DIR}/${DATASET_DIR}" -type f \( \( -name '*.asc' -o -name '*.tif' \) -a \( ! -name 'hUx_*' -a ! -name 'hUy_*' -a ! -name 'h_max_*' \) \) |
  while IFS= read -r rast; do
    if [ "TRUE" = "$first_tiff" ]; then
      echo Transforming/copying geo raster files

      # Remove any pre-existing GeoTIFF output
      rm -rf "${GEOTIF_DIR:?}/${DATASET_DIR:?}"

      first_tiff="FALSE"
    fi

    echo "$rast"

    out_tif_path="${GEOTIF_DIR}/$(echo "${rast#"${DATA_DIR}/"}" | sed -e 's/asc$/tif/')"

    echo "$out_tif_path"

    # Ensure that the output directory exists
    mkdir -p "$(dirname "$out_tif_path")"

    gdal_translate -a_srs EPSG:"${TIF_EPSG_FROM}" "${rast}" "$out_tif_path"

  done
}

export PGPASSFILE=/tmp/postgres.pgpass
# Populate .pgpass file
# hostname:port:database:username:password
echo "${POSTGRES_HOST}:${POSTGRES_PORT}:*:${POSTGRES_USER}:${POSTGRES_PASSWORD}" > $PGPASSFILE
chmod 0600 $PGPASSFILE

# Ensure all scripts (.sh) in the config directory are executable
ensure_scripts_executable "${CONFIG_DIR}"

# Read in the configuration files, each one in a new subshell so that the variables from the previous file are cleared
for c in "${CONFIG_DIR}"/*.conf; do (

  # Load variables from config file
  echo "Loading settings from config file at $c"
  ensure_unix_line_endings "$c"
  . "$c"

  echo DATASET="$DATASET_DIR"
  echo SOURCE_DIR="$SOURCE_DIR"

  case "${RASTER_TRANSFORM_MODE}" in
    ONLY)
      # This is just for the DAFNI workflow as the generated GeoTIFFs need to be copied into the pgsql container before it is started
      if [ "${g_TIF_ENABLED-0}" = "1" ] && [ "${TIF_ENABLED-0}" = "1" ]; then
        echo Just transforming rasters to GeoTIFFs
        transform_rasters_to_geotiffs
      fi
      # exit sub-shell and go to next iteration of the loop
      exit 0
      ;;
    NONE|"")
      ;;
    *)
      echo "Error: '${RASTER_TRANSFORM_MODE}' is not a valid value for the variable 'RASTER_TRANSFORM_MODE'."
      echo "If it is set, 'RASTER_TRANSFORM_MODE' can only take the values 'ONLY', 'NONE', or an empty string."
      exit 1
    ;;
  esac

  # Always create the database if it doesn't already exist,
  # even if there aren't any files to upload
  create_postgres_database "$DB"

  echo DATASET="$DATASET_DIR"
  echo SOURCE_DIR="$SOURCE_DIR"

  # If the dataset has been mounted as a zipfile, unzip it now
  zip_name="$SOURCE_DIR.zip"
  if [ -f "$zip_name" ]; then
    echo "Unzipping dataset to $SOURCE_DIR"
    unzip "$zip_name" -d $(dirname $SOURCE_DIR)
  fi

  # If a dataset directory is specified then also install the postgis extentions
  if [ -n "$DATASET_DIR" ]; then
    install_postgis_extensions "$DB"
  fi

  # Uploading of CSV files is disabled globally unless "g_CSV_ENABLED" is set to "1" externally
  if [ "${g_CSV_ENABLED-0}" = "1" ] ; then
    # Uploading of CSV files from this dataset is disabled unless "CSV_ENABLED" is set to "1" in the config file
    if [ "${CSV_ENABLED-0}" = "1" ]; then

      # "TABULAR_FILES" should contain a list of source CSV files and/or Excel files with the required sheets within them
      if test -n "$TABULAR_FILES"; then
        prepare_tabular_files "$SOURCE_DIR" "$TABULAR_FILES" "${DATA_DIR}/${DATASET_DIR}"
      fi

      if test -n "$(find "${DATA_DIR}/${DATASET_DIR}" -maxdepth 1 -type f -name '*.csv' -print -quit)"; then
        echo Loading csv files

        for csv in "${DATA_DIR}/${DATASET_DIR}"/*.csv; do
          echo "$csv"
          filename=$(basename -- "$csv" ".csv")

          # Read in the file specific transformation(s), if specified
          CSV_SPECIFIC_TRANSFORMS=$(get_value "$CSV_TRANSFORMS" "$filename")
          if [ -n "$CSV_SPECIFIC_TRANSFORMS" ]; then
            # Temporarily set the for-loop delimiter to the ";" character
            SAVEIFS="$IFS"
            IFS=";"
            for CSV_SPECIFIC_TRANSFORM in $CSV_SPECIFIC_TRANSFORMS; do
              # Check whether an alternative table name is specifed with the transformation, "transformation(table_name)"
              if [ "${CSV_SPECIFIC_TRANSFORM}" = "${CSV_SPECIFIC_TRANSFORM#*(}" ]; then
                # No alternative table name so just use the sanitised name of the source file
                csv_transform="${CONFIG_DIR}/${CSV_SPECIFIC_TRANSFORM}"
                tablename="$filename"
              else
                # Alternamtive table name specified so use that
                csv_transform="${CONFIG_DIR}/${CSV_SPECIFIC_TRANSFORM%(*}"
                tablename="${CSV_SPECIFIC_TRANSFORM#*(}"
                tablename="${tablename%)}"
              fi
              # Upload the CSV file after applying the current transformation
              upload_csv
            done
            IFS="$SAVEIFS"
          else
            # No transformations specifed so just use the "cat" command as the identity transformation
            csv_transform="cat"
            tablename="$filename"
            upload_csv
          fi
        done

      else
        echo No .csv or Excel files found.
      fi
    else
      echo Skipping loading of csv files
    fi
  fi

  # Uploading of vector geometries is disabled globally unless "g_SHP_ENABLED" is set to "1" externally
  if [ "${g_SHP_ENABLED-0}" = "1" ] ; then
    # Uploading of vector geometries from this dataset is disabled unless "SHP_ENABLED" is set to "1" in the config file
    if [ "${SHP_ENABLED-0}" = "1" ]; then

      if test -n "$(find "${DATA_DIR}/${DATASET_DIR}" -maxdepth 1 -type f -name '*.shp' -print -quit)"; then
        echo Loading shp files

        if [ -z "$SHP_SQL_STATEMENT_TRANSFORM" ]; then
          SHP_SQL_STATEMENT_TRANSFORM="cat"
        else
          SHP_SQL_STATEMENT_TRANSFORM=${CONFIG_DIR}/$SHP_SQL_STATEMENT_TRANSFORM
        fi

        # Add shape files to .sql file
        mode=d # For the first shp file drop any existing table with the name defined by "${SHP_SCHEMA}.${SHP_TABLE}"
        add_index_flag="-I" # Only add an index once so remove this flag after the first file
        for shp in "${DATA_DIR}/${DATASET_DIR}"/*.shp; do
          echo "$shp"
          # If "SHP_EPSG_FROM" is empty or set to 0 then try to get the projection from the .shp file
          # This replicates the same functionality that is built into the 'raster2pgsql' tool
          if test -z "$SHP_EPSG_FROM" || test "0" = "$SHP_EPSG_FROM"; then
            SHP_EPSG_FROM="$(gdalsrsinfo -o epsg "$shp")"
            SHP_EPSG_FROM="${SHP_EPSG_FROM#*:}"
          fi
         shp2pgsql -${mode} -s "${SHP_EPSG_FROM}" -D $add_index_flag -S "$shp" "${SHP_SCHEMA}"."${SHP_TABLE}" |
            ${SHP_SQL_STATEMENT_TRANSFORM} |
            psql -h "$POSTGRES_HOST" -p "$POSTGRES_PORT" -U "$POSTGRES_USER" -d "$DB" -w
          
          mode=a # Just append the data from subsequent shp files
          add_index_flag=
        done

        if [ -e "${DATA_DIR}/${DATASET_DIR}/${SHP_TABLE}.json" ]; then
          if ! curl_test "http://${GEOSERVER_HOST}:${GEOSERVER_PORT}/geoserver/rest/workspaces/${WORKSPACE}/datastores/${DB}/featuretypes/${SHP_TABLE}" ; then
            curl_wrapper -X POST -H "content-type: application/json" -d @"${DATA_DIR}/${DATASET_DIR}/${SHP_TABLE}.json" "http://${GEOSERVER_HOST}:${GEOSERVER_PORT}/geoserver/rest/workspaces/${WORKSPACE}/datastores/${DB}/featuretypes"
          fi
          if [ -e "${DATA_DIR}/${DATASET_DIR}/${SHP_TABLE}.sld" ]; then
            if ! curl_test "http://${GEOSERVER_HOST}:${GEOSERVER_PORT}/geoserver/rest/workspaces/${WORKSPACE}/styles/${SHP_TABLE}.json" ; then
              echo "Adding style '${SHP_TABLE}' to workspace '${WORKSPACE}' ..."
              curl_wrapper -X POST -H "content-type: application/vnd.ogc.sld+xml" -d @"${DATA_DIR}/${DATASET_DIR}/${SHP_TABLE}.sld" "http://${GEOSERVER_HOST}:${GEOSERVER_PORT}/geoserver/rest/workspaces/${WORKSPACE}/styles?name=${SHP_TABLE}"
              echo "Added style '${SHP_TABLE}' to workspace '${WORKSPACE}'"
            fi
            echo "Adding style to layer '${SHP_TABLE}' ..."
            style_name="$(curl_wrapper -X POST -H "content-type: application/json" -d "$(export STYLE="${SHP_TABLE}" && envsubst < geoservertemplates/layers/styleconfig.json)" "http://${GEOSERVER_HOST}:${GEOSERVER_PORT}/geoserver/rest/layers/${WORKSPACE}:${SHP_TABLE}/styles?default=true")"
            echo "Added style '${SHP_TABLE}' to layer '${SHP_TABLE}'"
          fi
        fi

      else
        echo No .shp files found.
      fi
    else
      echo Skipping loading of shp files
    fi
  fi

  # Uploading of raster files is disabled globally unless "g_TIF_ENABLED" is set to "1" externally
  if [ "${g_TIF_ENABLED-0}" = "1" ] ; then
    # Uploading of raster files from this dataset is disabled unless "TIF_ENABLED" is set to "1" in the config file
    if [ "${TIF_ENABLED-0}" = "1" ]; then

      # This skipped here for the DAFNI workflow as the generated GeoTIFFs need to be copied into the pgsql container before it is started
      if [ "NONE" != "${RASTER_TRANSFORM_MODE}" ]; then
        # Remove any previously added table with the same name, this is a prerequsite of deleting the previous GeoTIFFs
        echo "DROP TABLE IF EXISTS ${TIF_TABLE}" | psql -h "$POSTGRES_HOST" -p "$POSTGRES_PORT" -U "$POSTGRES_USER" -d "$DB" -w

        transform_rasters_to_geotiffs
      fi

      geotiffs="$(find "${GEOTIF_DIR}/${DATASET_DIR}" -type f -name '*.tif' -print)"
      if test -n "$geotiffs"; then
        echo Loading geotiff files
        
        install_postgis_raster_extensions "$DB"

        if [ -z "$TIF_SQL_STATEMENT_TRANSFORM" ]; then
          TIF_SQL_STATEMENT_TRANSFORM="cat"
        else
          TIF_SQL_STATEMENT_TRANSFORM=${CONFIG_DIR}/$TIF_SQL_STATEMENT_TRANSFORM
        fi

        # Add geotiff files to database
        mode=d
        raster2pgsql -C -I -F -Y -M -R -${mode} -s "${TIF_EPSG_FROM}" -t auto ${geotiffs} "${TIF_SCHEMA}.${TIF_TABLE}" |
          ${TIF_SQL_STATEMENT_TRANSFORM} |
          psql -h "$POSTGRES_HOST" -p "$POSTGRES_PORT" -U "$POSTGRES_USER" -d "$DB" -w

      else
        echo No .asc or .tif files found.
      fi
   else
      echo Skipping loading of geotiff files
    fi
  fi
)
done
