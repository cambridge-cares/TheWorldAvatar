# Example visualisations

This directory contains the sample data, configuration, and miscellaneous resources needed to spin up a TWA Stack with some example visualisations.

Please note that this example is aimed as showing new users what the visualisation framework is capable of, and to create an experimentation space in which data formats, styling, and other visualisation functionality can be played with. The generation of the sample data, and its stack configuration files are not explained here; for more details on this, see the [TWA Stack](../../../Deploy/stacks/dynamic/stack-manager) documentation. 

## Mapbox

An example visualisation using the Mapbox library has been created with sample data in each of our commonly used formats. For more information on these, see the [associated documentation](./mapbox.md).

Once the stack is spun up, this visualisation should be accessible at `http://localhost:38383/mapbox-vis`.

## Cesium

An example visualisation using the CesiumJS library has been created with sample data in each of our commonly used formats. For more information on these, see the [associated documentation](./cesium.md).

Once the stack is spun up, this visualisation should be accessible at `http://localhost:38383/cesium-vis`.

## Spinning up the example stack

The example stack can be started by running the `start.sh` script from within this directory, supplying a `PASSWORD` argument to set the password for data containers within the stack. Example usage is shown below.

```
start.sh PASSWORD=MyPassword
```