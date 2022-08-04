# The Stack Data Uploader

In the commands below placeholders are shown as `<STACK NAME>`, you will need to substitute in the required value when running the command.

## Prerequisites

These are the same as listed in [The Stack Manager](../stack-manager/README.md#prerequisites).

You should also initialise the stack by following the instructions in [Spinning up a Stack](../stack-manager/README.md#spinning-up-a-stack).

## Running the Stack Data Uploader

To load static data files into the stack please follow the instructions below:

1. Open the Workspace in the `Deploy/stacks/dynamic` directory in VSCode (or go to the `stack-data-uploader` subdirectory within it in a `bash` terminal).

2. Either start from an example dataset by following the instructions in the [README.md](../example_datasets/README.md) file in the `example_datasets` directory or use the examples as a guide to write a new .json config file.

3. From a terminal in the `stack-data-uploader` directory, start the `stack-data-uploader` container by running the following:
    ```console
    ./stack.sh start <STACK NAME>
    ```

## Debugging the Stack Data Uploader in VSCode

1. Add the following entry into top level node the JSON file `stack-data-uploader/.vscode/settings.json`, creating the file if it doesn't exist.
    ```json
    "debug.port": "<DEBUG PORT>"
    ```
    A value around `5007` for `<DEBUG PORT>` should be appropriate, this must be different to the one specified for the `stack-manager`.

2. In the `Run and Debug` side panel of VSCode run the `Debug (stack-data-uploader)` configuration.

## Developing the Stack Data Uploader in VSCode

You will need permission to push to the CMCL package repository to be able to build the stack-data-uploader project

1. Follow the instuctions in step 1. of [Debugging the Stack Data Uploader in VSCode](#debugging-the-stack-data-uploader-in-vscode)

2. Create two files called `repo_username.txt` and `repo_password.txt` in the `stack-data-uploader/docker/credentials` directory. Populate the files with your GitHub username and access token (with scope to write packages), respectively.

3. In the `Run and Debug` side panel of VSCode run the `Build and Debug (stack-data-uploader)` configuration.
