# Smell Agent

SmellAgent is an IoT sensor Python Agent designed to detect and categorise smells. It reads data from InfluxDB and leverages a trained machine learning model to predict the type of smell.

## Table of Contents

- [Smell Agent](#smell-agent)
  - [Table of Contents](#table-of-contents)
  - [Installation](#installation)
  - [Resources](#resources)
    - [Building the machine learning model](#building-the-machine-learning-model)
      - [Model Output](#model-output)
      - [Data Format](#data-format)
  - [Usage](#usage)
  - [Docker Deployment](#docker-deployment)
- [Author](#author)
## Installation

1. Install the required dependencies:

`pip install -r requirements.txt`

## Resources

The `resources` directory contains essential files for the SmellAgent application, including:

- `*.joblib`: The trained machine learning model used for smell prediction.
- `smellagent.properties`: Configuration settings for connecting to InfluxDB and other parameters.

### Building the machine learning model
This section provides details about the machine learning model used in the SmellAgent application for predicting smell categories.

The data for training is provide by the sensor with the following columns:

   - _time: The specific timestamp at which the data was recorded.
   - _category: The actual smell category (e.g., Fresh Air, Coffee, etc.).
   - _measurement: The type of measurement taken (e.g., Air Quality Index (AQI), humidity, pressure, temperature).
   - _value: The value of the recorded measurement.

#### Model Output
A trained machine learning model saved as *.joblib. At the current stage of development, the agent filters the data to only include the measurements: AQI, humidity, pressure, and temperature.

#### Data Format
The trained model expects a 2D array-like or Pandas DataFrame as input, where each row is a set of features in the following order:

 - AQI (Air Quality Index)
 - Humidity
 - Pressure
 - Temperature

Here's how the input should be formatted:

```
import pandas as pd

input_data = pd.DataFrame({
    'aqi': [30, 40],
    'humidity': [60, 55],
    'pressure': [1000, 1020],
    'temperature': [25, 22]
})
```

## Usage

Use command:

`python -m smellagent.entry_point`

The server will be accessible at `http://localhost:9047`.

## Docker Deployment

SmellAgent can be run using Docker via Docker Compose:

`docker-compose up`

# Author
Jiawei Lai (jlai@cmcl.io), August 2023