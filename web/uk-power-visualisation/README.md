# Visualisation of the UK Base World

This directory contains the documentation, configuration files, and associated scripts for a visualisation of The World Avatar's base world (focussing on assets within the United Kingdom) Whilst other data and capabilities related to the base world may exist elsewhere in The World Avatar, this documentation only covers the steps needed to acquire, upload, and visualise data used in the live UK Base World visualisation currently hosted on The World Avatar's website (theworldavatar.io).

This documentation was written in August of 2023. The data available from the listed sources may have changed since this time, hopefully the documented processes are still applicable to the new

## Gathering data

Data for this visualisation has been gathered from the below sources; the original raw files and the processed files have been archived at CMCL on their Pavilion file server.

Hopefully this process is repeatable with future versions of these data sets, if not then the archived data can be used as a fall-back. If the visualisation is updated with future versions of these data, the raw and processed versions of said files should also be archived.

### Digest of UK Energy Statistics (DUKES)

Once a year, the UK government publishes a Digest of UK Energy Statistics (DUKES);  note this was formally published by the Department for Business, Energy and Industrial Strategy (BEIS) before it was dissolved, subsequent publications should be from the new Department for Energy Security and Net Zero (DESNZ).

Read the associated [DUKES Data](./docs/data-dukes.md) page for details on how the DUKES data was acquired and processed.

## Uploading data

Now that we have a clean CSV for each data set, we can spin up an instance of the stack (see [here](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager) for details on how to do this) then run the data uploader to get our data into a relational database. Before trying to upload data, the [uploader's documentation](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-data-uploader) is considered required reading; this file will not detail the generic upload process.

A number of pre-written configuration files (separated by data set) are linked to below, note that these (along with the data set CSVs) will need to be copied into the correct directories before running the uploader.

Once the data uploader has finished running, you should be able to log into the GeoServer web dashboard and preview the layers (and feature locations within them).

### Power

The below files relate to the aforementioned DUKES 2023 data set.

* [Uploader config](./inputs/config/dukes_2023.json)
* [Ontop mapping](./inputs/data/uk_base_world//dukes_2023.obda)
  * Note that at the time of writing, this mapping utilises TBoxes that do not appear within the OntoEIP ontology. Nothing in the mapping contradicts the ontology, but the existing ontology does not contain enough concepts to cover all of the concepts provided by DUKES. 
* [OntoEIP ontology](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontoeip)
  * Note that when uploading the ontology files, you may need to rename any Turtle files with the `.ttl` extension. The stack data uploader assumes that `.owl` files are using the XML format, if an `.owl` file is using Turtle then this will cause errors during upload.


## Creating a visualisation

### Loading sources

### Loading layers


## Support

For any support in reproducing this visualisation, please contact 