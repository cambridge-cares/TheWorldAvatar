# User directory

This directory exists to act as the target for a Docker volume or bind mount.

Within the deployed container, it should be mounted to the "/app/uploads" directory. Files within it can then be accessed using the "$HOST/uploads/..." URL route.

See the `code/README.md` document for more details.