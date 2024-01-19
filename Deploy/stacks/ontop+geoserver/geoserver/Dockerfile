ARG GEOSERVER_VERSION

FROM geosolutionsit/geoserver:${GEOSERVER_VERSION}

# Override Tomcat settings to enable COR
COPY webapps/geoserver/WEB-INF/web.xml /usr/local/tomcat/webapps/geoserver/WEB-INF/web.xml

# Used to override Global Geoserver settings over REST API
# Currently used to configure use with reverse proxies
COPY global_settings_overrides.json /global_settings_overrides.json

# Run the upload script, via the entrypoint script
ENTRYPOINT /entrypoint.sh