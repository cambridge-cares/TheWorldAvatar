# Using Ontop and GeoServer to serve tabular and geospatial data

## Ontop

### TL;DR
1. Go to the directory containing the files for this stack:

        cd TheWorldAvatar/Deploy/stacks/ontop+geoserver

2. Run this script to generate a Docker-compose `.env` file, just press `Enter` to use the default settings:

        ./scripts/generate_env_file.sh

3. Start the server containers (this will also pull and build the images the first time):

        docker-compose -f docker-compose.servers.yml up -d

4. Copy the required crop map shapefiles (and [accompanying files][shapefiles]) into the `shared_data/cropmap` directory.

5. Start the uploader container:

        docker-compose -f docker-compose.data_upload.yml up

6. Access the data via the graphical web interfaces, the URLs are based on the default options provided in step 2.:
   * [Adminer interface to the PostgreSQL database with default login settings][postgres_web] (the default password can be found in the `postgis/postgres.env` file)
   * [Ontop web endpoint][ontop_web]

[shapefiles]: https://trac.osgeo.org/gdal/wiki/UserDocs/Shapefiles
[postgres_web]: http://localhost:2311/?pgsql=host.docker.internal%3A2317&username=postgres&db=the_world_avatar
[ontop_web]: http://localhost:2316/