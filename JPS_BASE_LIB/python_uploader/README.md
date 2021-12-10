# Description #

The `pyuploader` package provides a simple python API for uploading files into file servers and triples, in form of owl files, into triple stores. The package aims to simplify and unify data uploads while working on the [TheWorldAvatar](https://github.com/cambridge-cares/TheWorldAvatar) project.

# Installation #
These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.

## Virtual environment setup

It is highly recommended to use a [virtual environment](https://docs.python.org/3/tutorial/venv.html) for the `pyuploader` installation. The virtual environment can be created as follows:

`(Windows)`

```cmd
$ python -m venv <venv_name>
$ <venv_name>\Scripts\activate.bat
(<venv_name>) $
```

`(Linux)`
```sh
$ python3 -m venv <venv_name>
$ source <venv_name>/bin/activate
(<venv_name>) $
```

The above commands will create and activate the virtual environment `<venv_name>` in the current directory.

## Installation via pip

To install the `pyuploader` simply run the following command:

```sh
(<venv_name>) $ pip install pyuploader
```

# File server and triple store API requirements #

In order to use the `pyuploader` tool for uploading files into the file server and triples into the triple store, both endpoints must accept the standard post requests. In case of the file server, if one wishes the `--subdirs` option to work, the request header must contain the following key value pair: `{'subDir': subdirs}` and the extra logic on how to handle this option needs to be provided on the server side. The `pyuploader` also expects the file server to return the uploaded file url in the server response `header['file']`. If that is missing, the `pyuploader` would simply return the server url.

# Command line interface usage #

## File uploader CLI

```bash
Usage:
    fs_upload <file_or_dir>  [--url=<url>]
                             [--auth-file=<file>]
                             [--no-auth]
                             [--file-ext=<ext>]
                             [--subdirs=<dir>]
                             [--log-file-name=<name>]
                             [--log-file-dir=<dir>]
                             [--no-file-logging]
                             [--dry-run]

Options:
--url=<url>             File server upload url. If not specified, the code
                        will try to read it from a file whose location
                        should be specified in user 'KG_FILE_SERVER_SPECS'
                        environment variable.
--auth-file=<file>      File path to the file server secrets file containing
                        the user authorization string of the following form:
                        "username:password". If not specified, the code will
                        try to read the secrets file path from a user
                        'KG_FILE_SERVER_SECRETS' environment variable.
                        DO NOT store your secrets directly in environment
                        variables, only store the secrets file path.
--no-auth               Disables reading credentials from the environment
                        variables and sending it to the file server.
--file-ext=<ext>        List of extensions used to select files
                        that will be uploaded to the file server.
                        Example: --file-ext='log,txt'                       [default: log]
--subdirs=<dir>         Optional subdirectories to be created on
                        the file server to upload your files into.
                        Example: --subdirs='dir1/dir2/'                     [default: ]
--log-file-name=<name>  Name of the generated log file.                     [default: fs_uploader.log]
--log-file-dir=<dir>    Path to the log file storing information of
                        what has been uploaded and where. Defaults
                        to the <file_or_dir> directory.
--no-file-logging       No logging to a file flag.
--dry-run               Run the file uploader tool in a dry
                        run without uploading any files.
```

```bash
# Example usage
# - upload single file,
#   read the server url and auth from env variables
$ fs_upload file1.log
# - upload single file,
#   read the server auth from env variables
$ fs_upload file1.log --url="http://<server_url>"
# - upload single file,
#   read the server url from env variables, do not use authorisation
$ fs_upload file1.log --url="http://<server_url>" --no-auth
# - upload all log, txt and tiff files in my_directory,
#   read the server url and auth from env variables
$ fs_upload my_directory --file-ext="log,txt,tiff"
# - similar to the previous example, but it does not upload the files
$ fs_upload my_directory --file-ext="log,txt,tiff" --dry-run
```

## Triple store uploader CLI

```bash
Usage:
    ts_upload <file_or_dir>  [--url=<url>]
                             [--auth-file=<file>]
                             [--no-auth]
                             [--file-ext=<ext>]
                             [--log-file-name=<name>]
                             [--log-file-dir=<dir>]
                             [--no-file-logging]
                             [--dry-run]

Options:
--url=<url>             Triple store upload endpoint. If not specified, the code
                        will try to read it from a file whose location
                        should be specified in user 'TRIPLE_STORE_SPECS'
                        environment variable.
--auth-file=<file>      File path to the triple store secrets file containing
                        the user authorization string of the following form:
                        "username:password". If not specified, the code will
                        try to read the secrets file path from a user
                        'TRIPLE_STORE_SECRETS' environment variable.
                        DO NOT store your secrets directly in environment
                        variables, only store the secrets file path.
--no-auth               Disables reading credentials from the environment
                        variables and sending it to the triple store.
--file-ext=<ext>        List of extensions used to select files             [default: owl]
                        that will be uploaded to the triple store.
--log-file-name=<name>  Name of the generated log file.                     [default: ts_uploader.log]
--log-file-dir=<dir>    Path to the log file storing information of
                        what has been uploaded and where. Defaults
                        to the <file_or_dir> directory.
--no-file-logging       No logging flag to a file.
--dry-run               Run the triple store uploader tool in a dry
                        run without uploading any triples.
```

```bash
# Example usage
# - upload triples from a single file,
#   read the triple store endpoint and auth from env variables
$ ts_upload file1.owl
# - upload triples from a single file,
#   read the triple store auth from env variables
$ ts_upload file1.owl --url="http://<triple_store_endpoint>"
# - upload triples from a single file,
#   read the triple store endpoint from env variables, do not use authorisation
$ ts_upload file1.owl --url="http://<triple_store_endpoint>" --no-auth
# - upload triples from all owl files in my_directory,
#   read the triple store endpoint and auth from env variables
$ ts_upload my_directory --file-ext="owl"
# - similar to the previous example, but it does not upload the triples
$ ts_upload my_directory --file-ext="owl" --dry-run
```
# Module usage #

Apart from using `pyuploader` via its command line interface it is also possible to use it as a python module. The recommended way is to add the following import statement in your code:

```python
from pyuploader import get_uploader
```

The `get_uploader` is a factory function for creating file or triple store uploaders. The function's signature is as follows:

```python
#------------------------------------------------
# SIGNATURE
def get_uploader(
    uploader_type: str,                      # pass either "fs_uploader" or "ts_uploader" string
    default_url: Optional[str] = None,       # optional
    default_auth_file: Optional[str] = None, # optional
    default_no_auth: bool = False            # optional
    ) -> Uploader:

    ...
#------------------------------------------------

# Example creation of the file uploader,
# url and auth details to be read from
# the system env variables
file_uploader = get_uploader(
    uploader_type="fs_uploader")

# Example creation of the triple store uploader,
# triple store endpoint and auth details to be read from
# the system env variables
triple_store_uploader = get_uploader(
    uploader_type="ts_uploader")

# The created uploader objects provide two main methods:
# "set_logging" and "upload" to configure the file or
# triple upload behaviour. Here is how to use them:
uploader.set_logging(
    log_file_dir=log_file_dir,
    log_file_name=log_file_name,
    no_file_logging=no_file_logging
    )

uploaded_locations = uploader.upload(
    file_or_dir=file_or_dir,
    file_ext=file_ext,
    subdirs=subdirs,                   # only used in the file uploader
    dry_run=dry_run
    )
# where the "uploaded_locations" return value will contain:
# - file uploader case
#    *  {file_path: file_url} key - value pairs
#       if file server supports it
#    *  {file_path: server_url} key - value pairs
#       if file server does not support it
#
# - triple store uploader case
#    *  {file_path: triple_store_upload_endpoint} key - value pairs.
```

# Authors #
Daniel Nurkowski (danieln@cmclinnovations.com), 09 December 2021