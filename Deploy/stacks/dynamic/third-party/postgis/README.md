This image takes a postgis image that contains several bundled extensions and adds some extra pgrouting executables (particularly ```osm2pgrouting```) and the 3DCityDB extension.
See the [Dockerfile](./Dockerfile) for source information.

To build run:
```docker compose build```
To build and push run:
```docker compose build --push```