#!/bin/sh

# Build script for the 'db' Docker environment
#
# Most of the actual configuration is in ./docker-compose.yml; this is a wrapper that ensures all
# of the required volumes and secrets exist before running 'docker-compose up'.
#
# Any arguments will be passed on to 'docker-compose up' (along with -d), i.e.:
#   ./build.sh --force-recreate --build
# will run:
#   docker-compose -d --force-recreate --build

compose_opts_default="-d"
compose_opts="$compose_opts_default $*"

printf "\n==========================================================================================\n"
printf "Building 'db' environment\n\n"

# Create required,named volumes, if they don't exist already
echo "Checking/creating required Docker volumes..."
docker volume create blazegraph_data
docker volume create portainer_data
docker volume create rdf4j_data
docker volume create rdf4j_logs
printf "Done\n\n"

# Check that secret files exist, and have exactly one word on one line
echo "Checking required Docker secrets..."
for secret_path in ./blazegraph/secrets/blazegraph_password ./rdf4j/secrets/rdf4j_admin_password ./rdf4j/secrets/rdf4j_user_password; do
    if [ -f $secret_path ]; then
	num_lines=$(wc -l $secret_path |awk '{print $1}')
	num_words=$(wc -w $secret_path |awk '{print $1}')
        if [ $num_lines != "1" ] || [ $num_words != "1" ]; then
            echo "Secret file at $secret_path looks to be invalid (contains $num_words words on $num_lines lines; expected 1 word on 1 line)"
	    exit 2 
        fi
    else	
        echo no secret file at $secret_path
        exit 1
    fi
done
printf "Done\n\n"

# Run docker-compose
echo "Running docker-compose with args [$compose_opts]..."
docker-compose up $compose_opts
echo Done

printf "\nFinished building 'db' environment\n"
printf "==========================================================================================\n"

