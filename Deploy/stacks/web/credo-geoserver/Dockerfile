ARG GEOSERVER_VERSION

FROM geosolutionsit/geoserver:${GEOSERVER_VERSION} as base

# Override Tomcat settings to enable COR
COPY webapps/geoserver/WEB-INF/web.xml /usr/local/tomcat/webapps/geoserver/WEB-INF/web.xml

# Used to override Global Geoserver settings over REST API
# Currently used to configure use with reverse proxies
COPY global_settings_overrides.json /global_settings_overrides.json

# Temporarly switch to "root" user to rename original entrypoint script
USER root

# Rename original entrypoint script
RUN mv /usr/local/bin/geoserver-rest-config.sh /usr/local/bin/geoserver-rest-config_orig.sh

# Copy in new entrypoint script
COPY geoserver-rest-config.sh /usr/local/bin/geoserver-rest-config.sh

# Switch back to the user "tomcat" as specified in the parent image
USER tomcat

# Run the upload script, via the entrypoint script
ENTRYPOINT /entrypoint.sh

FROM base as dev

FROM base as prod