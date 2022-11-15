ARG GEOSERVER_VERSION
FROM geosolutionsit/geoserver:${GEOSERVER_VERSION}
# Override config script to set password from secret file, if it exists
COPY geoserver-rest-config.sh /usr/local/bin