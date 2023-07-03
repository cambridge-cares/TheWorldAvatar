# #!/bin/bash

# # Set the data folder, PostGIS password, username, and database name
# data_folder="./Test"
# postgis_password="1111"
# postgis_username="postgres"
# postgis_dbname="postgres"

# # Set a fixed table name
# table_name="Geodatabase"

# # Check if the data folder exists and is not empty
# if [ ! -d "$data_folder" ] || [ -z "$(ls -A "$data_folder")" ]; then
#     echo "Data folder does not exist or is empty"
#     exit 1
# fi

# # Get the Docker containers with the name "3DcityDB-gdal"
# gdal_containers=$(docker ps --format "{{.Names}}" --filter name="3DcityDB-gdal")

# # Check if there are any "gdal" containers
# if [ -z "$gdal_containers" ]; then
#     echo "No 'gdal' containers found"
#     exit 1
# fi

# # Iterate over all .gdb files in the data folder
# for gdb_file in "$data_folder"/*.gdb; do
#     # Extract just the name of the file
#     gdb_file_name=$(basename "$gdb_file")
    
#     # Extract base table name from gdb_file_name
#     base_table_name=${gdb_file_name%.*} # remove extension

#     # Check if the table exists
#     if ! ogrinfo "$gdb_file" "buildings_${base_table_name}" >/dev/null 2>&1; then
#         echo "'buildings_${base_table_name}' does not exist in $gdb_file"
#         continue
#     fi

#     echo "'buildings_${base_table_name}' table is detected in $gdb_file"

#     # Copy the data to each container and execute the PostgreSQL command
#     for container_name in $gdal_containers; do
#         docker cp "$gdb_file" "$container_name":"/$gdb_file_name" &&
#         echo "Data copied to $container_name" &&
#         docker exec "$container_name" sh -c "ogr2ogr -f PostgreSQL -append -nln ${table_name} PG:'host=3DcityDB-postgis user=${postgis_username} password=${postgis_password} dbname=${postgis_dbname}' /$gdb_file_name -lco SCHEMA='public' -sql 'SELECT os_topo_toid, polygon, relh2, abshmin FROM buildings_${base_table_name} WHERE bha_conf <> 90' 2>> /error.log" &&
#         echo "PostgreSQL command executed in $container_name" || echo "Failed to execute PostgreSQL command in $container_name for $gdb_file"
#     done
# done

#!/bin/bash

# # Set your data folder, PostGIS password, username, and database name
# data_folder="./Test"
# postgis_password="1111"
# postgis_username="postgres"
# postgis_dbname="postgres"

# # Set a fixed table name
# table_name="Geodatabase"

# # Check if the data folder exists and is not empty
# if [ ! -d "$data_folder" ] || [ -z "$(ls -A "$data_folder")" ]; then
#     echo "Data folder does not exist or is empty"
#     exit 1
# fi

# # Get the Docker containers with the name "3DcityDB-gdal"
# gdal_containers=$(docker ps --format "{{.Names}}" --filter name="3DcityDB-gdal")

# # Check if there are any "gdal" containers
# if [ -z "$gdal_containers" ]; then
#     echo "No 'gdal' containers found"
#     exit 1
# fi

# # Initialize counters and get total number of gdb files
# counter=0
# total=$(find "$data_folder" -type d -name "*.gdb" | wc -l)

# # Iterate over all .gdb directories in the data folder
# for gdb_file in "$data_folder"/*.gdb; do
#     # Ensure directory exists before proceeding
#     if [ ! -d "$gdb_file" ]; then
#         echo "Directory $gdb_file does not exist"
#         continue
#     fi
    
#     # Extract just the name of the directory
#     gdb_file_name=$(basename "$gdb_file")
    
#     # Extract base table name from gdb_file_name
#     base_table_name=${gdb_file_name%.*} # remove extension

#     # Check if the table exists
#     if ! ogrinfo "$gdb_file" "buildings_${base_table_name}" >/dev/null 2>&1; then
#         echo "'buildings_${base_table_name}' does not exist in $gdb_file"
#         continue
#     fi

#     echo "'buildings_${base_table_name}' table is detected in $gdb_file"

#     # Copy the data to each container and execute the PostgreSQL command
#     for container_name in $gdal_containers; do
#         if docker cp "$gdb_file" "$container_name":"/$gdb_file_name"; then
#             echo "Data copied to $container_name"
#         else
#             echo "Failed to copy data to $container_name"
#             continue
#         fi
        
#         if docker exec "$container_name" sh -c "ogr2ogr -f PostgreSQL -append -nln ${table_name} PG:'host=3DcityDB-postgis user=${postgis_username} password=${postgis_password} dbname=${postgis_dbname}' /$gdb_file_name -lco SCHEMA='public' -sql 'SELECT os_topo_toid, polygon, relh2, abshmin FROM buildings_${base_table_name} WHERE bha_conf <> 90' 2>> /error.log"; then
#             echo "PostgreSQL command executed in $container_name"
#         else
#             echo "Failed to execute PostgreSQL command in $container_name for $gdb_file"
#         fi
#     done

#     # Update counter and display progress
#     counter=$(expr $counter + 1)
#     echo "Processed $counter out of $total files."
# done

# #!/bin/bash

# # Set your data folder, PostGIS password, username, and database name
# data_folder="./Test"
# postgis_password="1111"
# postgis_username="postgres"
# postgis_dbname="postgres"

# # Set a fixed table name
# table_name="Geodatabase"

# # Check if the data folder exists and is not empty
# if [ ! -d "$data_folder" ] || [ -z "$(ls -A "$data_folder")" ]; then
#     echo "Data folder does not exist or is empty"
#     exit 1
# fi

# # Get the Docker containers with the name "3DcityDB-gdal"
# gdal_containers=$(docker ps --format "{{.Names}}" --filter name="3DcityDB-gdal")

# # Check if there are any "gdal" containers
# if [ -z "$gdal_containers" ]; then
#     echo "No 'gdal' containers found"
#     exit 1
# fi

# # Initialize counters and get total number of gdb files
# counter=0
# total=$(find "$data_folder" -type d -name "*.gdb" | wc -l)

# # Iterate over all .gdb directories in the data folder
# for gdb_file in "$data_folder"/*.gdb; do
#     # Ensure directory exists before proceeding
#     if [ ! -d "$gdb_file" ]; then
#         echo "Directory $gdb_file does not exist"
#         continue
#     fi

#     # Extract just the name of the directory
#     gdb_file_name=$(basename "$gdb_file")

#     # Extract base table name from gdb_file_name
#     base_table_name=${gdb_file_name%.*} # remove extension

#     # Check if the table exists
#     if ! ogrinfo "$gdb_file" "buildings_${base_table_name}" >/dev/null 2>&1; then
#         echo "'buildings_${base_table_name}' does not exist in $gdb_file"
#         continue
#     fi

#     echo "'buildings_${base_table_name}' table is detected in $gdb_file"

#     # Temporary table name for this iteration
#     temp_table_name="${table_name}_temp_${base_table_name}"

#     # Copy the data to each container and execute the PostgreSQL command
#     for container_name in $gdal_containers; do
#         if docker cp "$gdb_file" "$container_name":"/$gdb_file_name"; then
#             echo "Data copied to $container_name"
#         else
#             echo "Failed to copy data to $container_name"
#             continue
#         fi

#         if docker exec "$container_name" sh -c "ogr2ogr -f PostgreSQL -nln ${temp_table_name} PG:'host=3DcityDB-postgis user=${postgis_username} password=${postgis_password} dbname=${postgis_dbname}' /$gdb_file_name -lco SCHEMA='public' -sql 'SELECT os_topo_toid, polygon, relh2, abshmin FROM buildings_${base_table_name} WHERE bha_conf <> 90' 2>> /error.log"; then
#             echo "PostgreSQL command executed in $container_name"
#         else
#             echo "Failed to execute PostgreSQL command in $container_name for $gdb_file"
#             continue
#         fi
#     done

#     ((counter++))
#     echo "Processed $counter out of $total files."
# done

# # All data is in separate temporary tables.
# # Now we merge them into the final Geodatabase table.
# for container_name in $gdal_containers; do
#     docker exec "$container_name" sh -c "ogr2ogr -f PostgreSQL -append PG:'host=3DcityDB-postgis user=${postgis_username} password=${postgis_password} dbname=${postgis_dbname}' -sql 'SELECT * FROM ${table_name}_temp_*'"
# done

# # Cleanup: remove temporary tables
# for container_name in $gdal_containers; do
#     docker exec "$container_name" sh -c "PGPASSWORD=${postgis_password} psql -U ${postgis_username} -d ${postgis_dbname} -h 3DcityDB-postgis -c 'DROP TABLE IF EXISTS ${table_name}_temp_*;'"
# done

# echo "Finished processing all gdb files into final table: $table_name"


# #!/bin/bash

# # Set your data folder and PostGIS password
# data_folder="./Test"
# postgis_password="1111"
# merge_table_name=$1

# # Get the Docker containers with the name "3DcityDB-gdal"
# gdal_containers=$(docker ps --format "{{.Names}}" --filter name="3DcityDB-gdal")

# # Check if there are any "gdal" containers
# if [ -z "$gdal_containers" ]; then
#     echo "No 'gdal' containers found"
#     exit 1
# fi

# # Create the merge_table_name if not exists
# docker exec 3DcityDB-gdal psql -U postgres -c "CREATE TABLE IF NOT EXISTS ${merge_table_name} AS TABLE buildings_tl WITH NO DATA;"

# # Iterate over all .gdb files in the data folder
# for gdb_file in "$data_folder"/*.gdb; do
#     # Extract just the name of the file
#     gdb_file_name=$(basename "$gdb_file")
    
#     # Extract base table name from gdb_file_name
#     base_table_name=${gdb_file_name%.*} # remove extension

#     # Check if the table exists
#     if ! ogrinfo "$gdb_file" "buildings_${base_table_name}" >/dev/null 2>&1; then
#         echo "'buildings_${base_table_name}' does not exist in $gdb_file"
#         continue
#     fi

#     echo "'buildings_${base_table_name}' table is detected in $gdb_file"

#     # Check if data already exists in merge_table_name
#     result=$(docker exec 3DcityDB-gdal psql -U postgres -t -c "SELECT COUNT(*) FROM ${merge_table_name} WHERE os_topo_toid IN (SELECT os_topo_toid FROM buildings_${base_table_name});")
#     if [ "$result" -gt "0" ]; then
#         echo "Data from 'buildings_${base_table_name}' already exists in '${merge_table_name}', skipping this gdb file."
#         continue
#     fi

#     # Copy the data to each container and execute the PostgreSQL command
#     for container_name in $gdal_containers; do
#         docker cp "$gdb_file" "$container_name":"/$gdb_file_name" &&
#         echo "Data copied to $container_name" &&
#         docker exec "$container_name" sh -c "ogr2ogr -f PostgreSQL -append -nln ${merge_table_name} PG:'host=3DcityDB-postgis user=postgres password=${postgis_password} dbname=postgres' /$gdb_file_name -lco SCHEMA='public' -sql 'SELECT os_topo_toid, polygon, relh2, abshmin FROM buildings_${base_table_name} WHERE bha_conf <> 90' 2>> /error.log" &&
#         echo "PostgreSQL command executed in $container_name" || echo "Failed to execute PostgreSQL command in $container_name for $gdb_file"
#     done
# done

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

