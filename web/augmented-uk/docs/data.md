# Augmented UK - Data

This page details the data currently available on the visualisation of the augmented UK.

## Adding the data

Each of the data sources below contain one or more files that need to be added to the `augmented-uk/config/uploader/data/` subdirectory. Each of these directories should contain a short `README.md` file detailing the files that are expected to be copied into that folder; although the relevant directories are also listed under each data source below.

## Data sources

### Regions

```
Data directories:
    augmented-uk/config/uploader/data/boundaries/countries_gb
    augmented-uk/config/uploader/data/boundaries/countries_ni
    augmented-uk/config/uploader/data/boundaries/counties_gb
    augmented-uk/config/uploader/data/boundaries/counties_ni
```

This data provides polygonal regions for the boundaries of countries with the UK and their ceremonial counties. Note that these are split between Great Britain and Northern Ireland as they use different projections (hence the need to load them as separate tables). Fortunately, these are seamlessly combined into two layers via the visualisation's `data.json` file.

The shape files used for this data set are provided by [Ordnance Survey](https://www.ordnancesurvey.co.uk/products/boundary-line), [Ordnance Survey Northern Ireland](https://www.data.gov.uk/dataset/d3ca9d44-a7eb-4380-86cb-0cc28e1f1b27/osni-open-data-largescale-boundaries-ni-outline), and [Edinburgh Data Share](https://datashare.ed.ac.uk/handle/10283/2595?show=full). No pre-processing is needed on this data set, we're using it as is.

[Ordnance Survey] provides information about the boundaries of England, Scotland and Wales, as well as the counties of the whole Great Britain. Set the data format to be ESRI Shapefile and download it. ALL FOUR files with the name 'county_region' should go to inputs/data/uk_base_world/boundaries/counties_gb folder. Similarly, ALL FOUR files with the name 'english_region_region' should go to inputs/data/uk_base_world/boundaries/countries_gb folder.

[Ordnance Survey Northern Ireland] provides information about the largescale boundaries of the whole Northern Ireland. Download the CSV file below and put it into inputs/data/uk_base_world/boundaries/countries_ni folder.

[Edinburgh Data Share] provides information about counties in Northern Ireland. Download the data and put ALL FOUR files into inputs/data/uk_base_world/boundaries/counties_ni folder.

### Digest of UK Energy Statistics (DUKES)

```
Data directories:
    augmented-uk/config/uploader/data/dukes_2023
    augmented-uk/config/uploader/data/ontoeip
```

Once a year, the UK government publishes a Digest of UK Energy Statistics (DUKES); note this was formally published by the Department for Business, Energy and Industrial Strategy (BEIS) before it was dissolved, subsequent publications should be from the new Department for Energy Security and Net Zero (DESNZ).

Read the associated [DUKES Data](./data-dukes.md) page for details on how the DUKES data was acquired and processed.

#### Associated files

- [Uploader config](../inputs/uploader/config/dukes2023.json)
- [Ontop mapping](../inputs/uploader/data/dukes_2023/dukes_2023.obda)
  - Note that at the time of writing, this mapping utilises TBoxes that do not appear within the OntoEIP ontology. Nothing in the mapping contradicts the ontology, but the existing ontology does not contain enough concepts to cover all of the concepts provided by DUKES.
- [OntoEIP ontology](https://github.com/TheWorldAvatar/ontology/tree/main/ontology/ontoeip)
  - Note that when uploading the ontology files, you may need to rename any Turtle files with the `.ttl` extension. The stack data uploader assumes that `.owl` files are using the XML format, if an `.owl` file is using Turtle then this will cause errors during upload.

Open the OntoEIP ontology link and find the resource_network folder. Put 'resource_network.ttl' into 'inputs/data/ontoeip' folder. Contact CMCL if you need the newest dukes data.

### United Kingdom: High Resolution Population Count

```
Data directories:
    augmented-uk/config/uploader/data/population
```

The [Humanitarian Data Exchange](https://data.humdata.org/) publishes a number of data sets from around the globe. On an irregular basis, they publish population density data for the UK. In this example we're using the `population_gbr.geotiff.zip` file (which contains a GeoTIFF of population density) from the [United Kingdom: High Resolution Population Density Maps + Demographic Estimates](https://data.humdata.org/dataset/united-kingdom-high-resolution-population-density-maps-demographic-estimates) page.

**Note:** Whilst meta reports this data as a population density, their documentation tells another tale; it is in fact a population count. Having said that, this only matters when calculating the amount of people within the 1KM radius of a power plant.

No pre-processing is needed on this data set, we're using it as is.

#### Associated files

- [Uploader config](../inputs/uploader/config/population.json)
- [Geospatial SQL Query](../inputs/uploader/config/sql/dukes_2023_pop.sql)
  - An SQL query to determine the number of people within a 1KM radius of each power plant.
- [Raster style](../inputs/uploader/config/sld/uk-population-style.sld)
  - SLD file to style the population raster data in GeoServer.

### Digest of UK Energy Statistics (DUKES)

### National Grid

National Grid publish shapefiles of their whole network including lines, pylons and substations. There are links to :Peach download page in the relevant `data` subdirectory. There is a good number of files (20) to download but they are all backed up on pavilion.

### UKPN

Similarly to above, download links are in each of the relevant Uk Power Networks subdirectory and are backed up on pavilion.

### Forestry

Shapefiles are obtained from [national forestry inventory 2020](https://data-forestry.opendata.arcgis.com/datasets/eb05bd0be3b449459b9ad0692a8fc203_0/explore?location=55.089693%2C-2.724655%2C6.98) and backed upon pavilion. The feature info agent is used to cross reference with the power lines and determine whether or not they intersect.

### Streetlamps, traffic signals and England highways

Links to sources (mostly local council data portals) are in each relevant [data folder](../inputs/uploader/data).

