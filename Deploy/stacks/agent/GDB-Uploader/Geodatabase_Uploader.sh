

#!/bin/bash

# Set your data folder and PostGIS password
data_folder="./Cambridge_Ely"
postgis_password="1111"

# The final table name will be an argument
final_table_name=$1

# Get the Docker containers with the name "3DcityDB-gdal"
gdal_containers=$(docker ps --format "{{.Names}}" --filter name="3DcityDB-gdal")

# Check if there are any "gdal" containers
if [ -z "$gdal_containers" ]; then
    echo "No 'gdal' containers found"
    exit 1
fi

# Create final table if it doesn't exist already
# This assumes you have the psql command line tool installed, if not, this command may be omitted
# and the table created manually in your database.
PGPASSWORD=${postgis_password} psql -h localhost -d postgres -U postgres -c "CREATE TABLE IF NOT EXISTS ${final_table_name} (os_topo_toid VARCHAR(255), polygon GEOMETRY, relh2 FLOAT, abshmin FLOAT);"

# Iterate over all .gdb files in the data folder
for gdb_file in "$data_folder"/*.gdb; do
    # Extract just the name of the file
    gdb_file_name=$(basename "$gdb_file")
    
    # Extract base table name from gdb_file_name
    base_table_name=${gdb_file_name%.*} # remove extension

    # Check if the table exists
    if ! ogrinfo "$gdb_file" "buildings_${base_table_name}" >/dev/null 2>&1; then
        echo "'buildings_${base_table_name}' does not exist in $gdb_file"
        continue
    fi

    echo "'buildings_${base_table_name}' table is detected in $gdb_file"

    # Copy the data to each container and execute the PostgreSQL command
    for container_name in $gdal_containers; do
        docker cp "$gdb_file" "$container_name":"/$gdb_file_name" &&
        echo "Data copied to $container_name" &&
        docker exec "$container_name" sh -c "ogr2ogr -f PostgreSQL -append PG:'host=3DcityDB-postgis user=postgres password=${postgis_password} dbname=postgres' /$gdb_file_name -lco SCHEMA='public' -sql 'SELECT os_topo_toid, polygon, relh2, abshmin FROM buildings_${base_table_name} WHERE bha_conf <> 90' -nln public.${final_table_name} 2>> /error.log" &&
        echo "PostgreSQL command executed in $container_name" || echo "Failed to execute PostgreSQL command in $container_name for $gdb_file"
    done
done

