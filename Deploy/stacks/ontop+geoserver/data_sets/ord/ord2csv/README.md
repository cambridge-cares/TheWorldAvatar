# ord2csv convertor tool

## Dependencies
### ORD Schema
#### Installation from PyPi
    $ pip install ord-schema
#### Development version
To install in editable/development mode:

    $ git clone https://github.com/open-reaction-database/ord-schema.git
    $ cd ord-schema
    $ pip install -e .

### Google API Python Client
Install the Google API Python Client from PyPi using the following command:

    $ pip install google-api-python-client

### PyTest, Wget, OS, Pandas
    $ pip install pytest wget os-sys pandas

## Important Protocol Buffer (ProtoBuf) Documentation Links
### Message
### Descriptor

## Running the docker command
    $ docker run -it -v $(pwd)/results:/results -v $(pwd)/inputs:/inputs ord2csv