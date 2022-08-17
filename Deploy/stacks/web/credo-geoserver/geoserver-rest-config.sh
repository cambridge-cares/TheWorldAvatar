#!/usr/bin/env bash
while [ "$(curl -s --retry-connrefused --retry 100 -I http://localhost:8080/geoserver/web/ 2>&1 |grep 200)" == "" ];do
  echo "Waiting for GeoServer to be Up and running"
done  

# If password has been provided as a Docker secret, use that, else pass a default
if [ -e "$GEOSERVER_PASSWORD_FILE" ]; then
  ADMIN_PASSWORD=$(cat $GEOSERVER_PASSWORD_FILE)
else
  ADMIN_PASSWORD="geopass"
fi

if [ "$ADMIN_PASSWORD" != "" ]; then
    echo "GeoServer password is likely to be default, going to change to new admin password."
    ADMIN_HEADER=$(echo -n "admin:geoserver" | base64)
    curl -H "Authorization: basic $ADMIN_HEADER" -X PUT http://localhost:8080/geoserver/rest/security/self/password -H  "accept: application/json" -H  "content-type: application/json" -d "{  \"newPassword\": \"$ADMIN_PASSWORD\"}"
fi

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

# Override the default global settings
curl_wrapper -X PUT -H "content-type: application/json" -d @"/global_settings_overrides.json" "http://${GEOSERVER_HOST}:${GEOSERVER_PORT}/geoserver/rest/settings"

previous_workspaces="$(curl_wrapper -X GET -H "accept: application/xml" "http://${GEOSERVER_HOST}:${GEOSERVER_PORT}/geoserver/rest/workspaces")"
previous_workspaces="$(echo "${previous_workspaces}" | grep -o '<name>[^<]*' | grep -o '[^><]*$')"
for previous_workspace in $previous_workspaces; do
  echo "Removing previous workspace '${previous_workspace}' ..."
  curl_wrapper -X DELETE "http://${GEOSERVER_HOST}:${GEOSERVER_PORT}/geoserver/rest/workspaces/${previous_workspace}.html?recurse=true"
  echo "Removed previous workspace '${previous_workspace}'"
done

# Remove all of the previous data
rm -rf ${GEOSERVER_DATA_DIR}/data/*

echo "Adding new workspace ..."
workspace_name="$(curl_wrapper -X POST -H "content-type: application/json" -d @"${DATA_IN_DIR}/workspace.json" "http://${GEOSERVER_HOST}:${GEOSERVER_PORT}/geoserver/rest/workspaces?default=true")"
echo "Added workspace '${workspace_name}'"

style_file_glob="${DATA_IN_DIR}/*.sld"
for style_file_path in $style_file_glob; do
  if [ "$style_file_path" != "$style_file_glob" ]; then
    style_name="$(basename "${style_file_path}" .sld)"

    echo "Adding style '${style_name}' to workspace '${workspace_name}' ..."
    curl_wrapper -X POST -H "content-type: application/vnd.ogc.sld+xml" -d @"${style_file_path}" "http://${GEOSERVER_HOST}:${GEOSERVER_PORT}/geoserver/rest/workspaces/${workspace_name}/styles"
    echo "Added style '${style_name}' to workspace '${workspace_name}'"
  fi
done

layer_zip_glob="${DATA_IN_DIR}/*.zip"
for layer_file_path in $layer_zip_glob; do
  if [ "$layer_file_path" = "$layer_zip_glob" ]; then
    echo "No layer .zip files provided in the '${DATA_IN_DIR}' directory"
  else
    layer_name="$(basename "${layer_file_path}" .zip)"

    echo "Uploading store files for ${layer_name} ..."
    curl_wrapper -X PUT -H "Content-type:application/zip" --data-binary @"${layer_file_path}" "http://${GEOSERVER_HOST}:${GEOSERVER_PORT}/geoserver/rest/workspaces/${workspace_name}/coveragestores/${layer_name}/file.imagemosaic?configure=none"
    echo "Uploaded store files for ${layer_name}"

    echo "Uploading layer files for ${layer_name} ..."
    curl_wrapper -X POST -H "Content-type: application/json" -d @"${DATA_IN_DIR}/${layer_name}/coverageconfig.json" "http://${GEOSERVER_HOST}:${GEOSERVER_PORT}/geoserver/rest/workspaces/${workspace_name}/coveragestores/${layer_name}/coverages"
    echo "Uploaded layer files for ${layer_name}"

    style_config_path="${DATA_IN_DIR}/${layer_name}/styleconfig.json"
    if [ -f "$style_config_path" ]; then
      echo "Adding style to layer '${layer_name}' ..."
      style_name="$(curl_wrapper -X POST -H "content-type: application/json" -d @"${style_config_path}" "http://${GEOSERVER_HOST}:${GEOSERVER_PORT}/geoserver/rest/layers/${layer_name}/styles?default=true")"
      echo "Added style '${style_name}' to layer '${layer_name}'"
    fi
  fi
done

