# The Pirmasens Digital Twin (PSDT)

In order to deploy (on a Linux machine):

1. Spin up the `gateway` by following the instructions in its readme file.

2. From a terminal in the `stack-manager` directory in TWA git repository, start the `stack-manager` by running the following:
    ```console
    sudo ./stack.sh start psdt
    ```

3. Populate the `input` folder of the `stack-data-uploader` directory with what is in the `stack-inputs` folder, following the readme files in each subfolder.

4. From a terminal in the `stack-data-uploader` directory in TWA git repository, start the `stack-data-uploader` container by running the following:
    ```console
    sudo ./stack.sh start psdt
    ```

5. Run `copy_icons_into_geoserver.sh` from within the `stack-inputs` folder.

6. Spin up the visualisation by following the instructions in its readme file.
