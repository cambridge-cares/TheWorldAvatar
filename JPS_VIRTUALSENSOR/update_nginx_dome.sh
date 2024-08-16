#!/bin/bash

TEXT_TO_ADD="
location /demos/ship-emission/dome-interactor/api/kernels/ {
  proxy_pass http://ship-stack-dome-interactor_ui/demos/ship-emission/dome-interactor/api/kernels/;

proxy_set_header X-Real-IP \$remote_addr;
proxy_set_header Host \$host;
proxy_set_header X-Forwarded-For \$proxy_add_x_forwarded_for;
# WebSocket support
proxy_http_version 1.1;
proxy_set_header Upgrade \"websocket\";
proxy_set_header Connection \"upgrade\";
# CORS headers
add_header \"Access-Control-Allow-Origin\" \"*\";
add_header \"Access-Control-Allow-Methods\" \"GET, POST, OPTIONS\";
add_header \"Access-Control-Allow-Headers\" \"Content-Type, Authorization\";
add_header \"Access-Control-Allow-Credentials\" \"true\";
}
"

# find nginx, add additional websocket for dome interactor

for container_name in $(docker ps --format "{{.Names}}" --filter name="ship-stack-nginx"); do
	if ! docker exec $container_name grep -qF "location /dome-interactor/api/kernels/" "/etc/nginx/conf.d/locations/ship-stack-dome-interactor.conf";
	then
		docker exec $container_name sh -c "echo '$TEXT_TO_ADD' >> /etc/nginx/conf.d/locations/ship-stack-dome-interactor.conf"
		docker exec $container_name nginx -t
		docker exec $container_name nginx -s reload
		echo "NGINX updated and reloaded."
	fi
done