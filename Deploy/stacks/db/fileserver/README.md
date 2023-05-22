# FileServer API

The base URL of all requests is:

    http(s)://<hostname>:<port>/FileServer/

## Deployment

To deploy the docker image, plese follow below steps:
1. Pull image in command line via `docker pull ghcr.io/cambridge-cares/fileserver:1.1.0`
2. Create a `docker-compose.yml` file with the below content, or adding the `fileserver` service block and relevant credential block to your docker-compose file (you may want to adjust the `container_name` and first part of the `ports` as you wish):
    ```yml
    version: "3.8"

    services:
        # File server
        fileserver:
            image: ghcr.io/cambridge-cares/fileserver:1.1.0
            container_name: "fileserver"
            ports:
                - 8888:8080
            # Add secret to set BASIC authentication password
            secrets:
                - file_server_password

    # Secrets used to set runtime passwords
    secrets:
        file_server_password:
            file: ./secrets/fileserver_password.txt
    ```
3. Populate the `./secrets/fileserver_password.txt` with a credential that you would like to set **NOTE NEVER COMMIT THIS FILE TO GIT**
4. Compose up via `docker compose -f "docker-compose.yml" up -d --build `

You should now be able to access the fileserver with a username `fs_user` and the credential you set.


## POST

### Requests

The path component of the URL after the base component is the destination path of the file(s).
The files themselves should be passed as fileparts.

If a file already exists on the server then a new copy is stored with the current Linux time appended to its name.

There are several modes it can operate in:
1. If there is only one file and the URL ends in a "/" then the file is copied into that directory (with any missing directories being created)

        http://<hostname>:<port>/FileServer/destination_dir/

2. If there is only one file and the URL does not ends in a "/", and there isn't already a directory with that path, then the last component is taken to be the intended name for the file on the server. The file is copied into the patent directory with that name (with any missing directories being created)

        http://<hostname>:<port>/FileServer/destination_dir/new_filename.txt

    or (not recommended), if the `destination_dir` directory already exists,

        http://<hostname>:<port>/FileServer/destination_dir

3. If there are multiple files passed then the path component of the URL is always treated as the destination directory, so

        http://<hostname>:<port>/FileServer/destination_dir/

    and

        http://<hostname>:<port>/FileServer/destination_dir

    are equivalent.

### Responses

If successful a status of `200` is returned and a map of filepart names to the URLs of the newly uploaded files is returned in the header.

A URL returned by the POST method can be used directly in subsequent GET and DELETE requests.

If any part of the request fails then a status other than `200` is returned along with an error message in the body of the response.

## GET

### Requests

The path component of the URL after the base component is the path of the file to be downloaded.

    http://<hostname>:<port>/FileServer/destination_dir/new_filename.txt

### Responses

If successful a status of `200` is returned.

If any part of the request fails then a status other than `200` is returned along with an error message in the body of the response.

## DELETE

### Requests

The path component of the URL after the base component is the path of the file or directory to be deleted.

    http://<hostname>:<port>/FileServer/destination_dir/new_filename.txt

or

    http://<hostname>:<port>/FileServer/destination_dir

### Responses

If successful a status of `200` is returned.

If any part of the request fails then a status other than `200` is returned along with an error message in the body of the response.