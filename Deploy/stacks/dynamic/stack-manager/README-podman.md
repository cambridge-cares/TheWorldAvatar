# Running the stack with podman

It is assumed that the reader is familiar with [how to operate the stack using Docker](./README.md). Using [podman](https://podman.io/) is largely similar, and this readme addresses only points where differences arise.

Success has been reported with podman v4.6.1 on Rocky v8.9 and v9.3, podman v4.9.3 on Ubuntu v24.04, and podman v4.9.4-rhel on Rocky 8.9. Running the stack with any podman version <4 is known not to work on any tested platform.

## Installing podman

If you have admin access to your host machine and need to install podman yourself, please follow the instructions [here](https://podman.io/docs/installation).

After installation, you may need to run:
```console
sudo systemctl enable --now podman
sudo systemctl enable --now podman.socket
```

Please note that podman is intended to be used in [rootless mode](https://github.com/containers/podman/blob/main/docs/tutorials/rootless_tutorial.md), which has the advantage that once installed, no administrative privileges are required. In particular, this means there is no need to prefix any `stack.sh` commands with `sudo`.

## Configuration

Your local `~/.config/containers/containers.conf` may need to be customised. Please consult [this file](../common-scripts/podman/containers.conf) for any additions that may be necessary.

## Operating the stack

Whilst using Python virtual environments is not strictly necessary in general, it is _highly_ recommended, which is why the `stack.sh` script will insist on it. Therefore, before running any `stack.sh` commands, please create a virtual environment via
```console
python3 -m venv <venvname>
```
and activate it via
```console
cd <venvname>
source ./bin/activate
```

For background information, the `stack.sh` script will check which version, if any, of the `podman-compose` Python package is installed within the virtual environment. The desired version is currently hard-coded [here](../common-scripts/common_functions.sh). If an incompatible version is already installed, the user will be asked to use a different virtual environment. If `podman-compose` is not installed, the script will install the desired version. Once the correct version of `podman-compose` is in place, the `stack.sh` script will apply a patch to the installed source in order to allow handling of secrets as required by the stack.

## Troubleshooting

Starting podman with debug-level verbosity may provide additional insights:
```console
podman --log-level=debug system service -t0
```

If you end up in a state where the system is unusable, try
```console
podman system reset -f
```
delete the folders
```console
~/.cache/containers
~/.local/share/containers
/run/user/${UID}/containers
```
and restart your machine (if you can).

If you find that logging out of your session kills your containers, try enabling lingering (if you have privileges):
```console
sudo loginctl enable-linger <user>
```
