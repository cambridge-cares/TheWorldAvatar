#!/bin/bash

# Exit on first error
set -e

get_executables(){
    if [ -z "$EXECUTABLE" ]; then
        # Ensure compatibility with podman
        if command -v podman &> /dev/null; then
            #>&2 echo "ERROR Podman cannot run in swarm mode so will have to move to Kubenetes before this actually works"
            # The "--podman-build-args" argument requires podman compose version 0.1.8

            if [ -n "$VIRTUAL_ENV" ]; then
                #echo "Info: Using Python virtual environment $VIRTUAL_ENV..."

                pip_executable="pip3"
                if command -v "$pip_executable" &> /dev/null; then

                    package_name="podman-compose"
                    desired_version="1.0.3"
                    # Check if the package is installed
                    if $pip_executable show "$package_name" >/dev/null 2>&1; then
                        installed_version=$($pip_executable show "$package_name" | grep Version | awk '{print $2}')
                        # Compare installed version with desired version
                        if [[ "$installed_version" != "$desired_version" ]]; then
                            echo "ERROR: Installed version of $package_name is $installed_version, but the desired version is $desired_version. Please either remove the incompatible version or (better) use a different virtual environment."
                            exit 1
                        else
                            #echo "Info: Found $package_name version $desired_version."
                            :
                        fi
                    else
                        echo "Info: $package_name not found. Installing version $desired_version..."
                        $pip_executable install -q "$package_name==$desired_version"
                    fi

                    # Patch podman-compose
                    # NB One could use the patch command with suitable arguments, e.g.:
                    # patch -u -N $filename -i ...
                    # The -N argument ensures the patch is applied only in forward direction,
                    # i.e. reverse or re-application will be ignored, which makes it idempotent!
                    # However, the patch command is not necessarily installed on all systems,
                    # and we cannot assume that we have sufficient privileges to install it.
                    # We therefore use git apply.
                    patch_executable=("git" "apply")
                    if command -v "${patch_executable[@]}" &> /dev/null; then
                        sitepackage_path=$(python3 -c 'import sysconfig; print(sysconfig.get_paths()["purelib"])')
                        # NB git apply is fussy about where it is being applied, and its
                        # directory argument apparently only takes paths relative to the root
                        # folder of the git repository, so as a work-around we temporarily
                        # switch into the site-packages folder.
                        pushd "$sitepackage_path/" > /dev/null
                        "${patch_executable[@]}" "${SCRIPTS_DIR}/podman/podman_compose_v$desired_version.patch" > /dev/null 2>&1 || true
                        popd > /dev/null
                    else
                        echo "ERROR: '${patch_executable[*]}' command not found. Please install it."
                        exit 1
                    fi

                else
                    echo "ERROR: $pip_executable not found."
                    exit 1
                fi

            else
                echo "ERROR: No active Python virtual environment detected."
                echo "Please create and/or activate a virtual environment first."
                exit 1
            fi

            EXECUTABLE="podman"
            COMPOSE_EXECUTABLE="podman-compose"
            API_SOCK="$XDG_RUNTIME_DIR/podman/podman.sock"
        else
            EXECUTABLE="docker"
            COMPOSE_EXECUTABLE="docker compose"
            API_SOCK="/var/run/docker.sock"
        fi

        STACK_BASE_DIR="$(pwd)"

        export STACK_BASE_DIR
        export EXECUTABLE
        export COMPOSE_EXECUTABLE
        export API_SOCK
    fi
}

init_server(){
    if [ "$EXECUTABLE" == "docker" ]; then
        if [ "$(docker info --format '{{.Swarm.LocalNodeState}}')" != "active" ]; then
            docker swarm init
        fi
    else
        if [ ! -S "$API_SOCK" ] || [ -z "$(pidof podman)" ]; then
            rm -rf "$API_SOCK"
            mkdir -p "$(dirname "$API_SOCK")"
            podman system service -t 0 "unix://$API_SOCK" &
        fi
    fi
}

# Attempt compatibility with podman
get_executables

if [[ -z "$NO_STACK_NAME" ]]; then
    # Read in the stack name as the first argument
    export STACK_NAME="$1"

    if (( $# >= 1 )); then
        shift
    fi
fi

export IMAGE_SUFFIX=
