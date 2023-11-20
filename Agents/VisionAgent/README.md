# Vision Agent

Vision Agent is designed to leverage computer vision models for object detection in images and videos. Utilising state-of-the-art models such as YOLO (You Only Look Once), this application can identify and categorise objects returning JSON objects.

## Table of Contents

- [Vision Agent](#vision-agent)
  - [Table of Contents](#table-of-contents)
  - [Installation](#installation)
  - [Resources](#resources)
    - [Configuration](#configuration)
  - [Running the Agent](#running-the-agent)
    - [Locally Without Docker](#locally-without-docker)
  - [Deploy with Docker:](#deploy-with-docker)
  - [Usage](#usage)
- [Author](#author)

## Installation
To install the necessary dependencies:

```
pip install -r requirements.txt
```

## Resources
The `resources/` directory includes:

 - `visionagent.properties`: Configuration file for model parameters.
 - YOLO Weights (`*.weights`): Pre-trained model weights.
 - YOLO Config (`*.cfg`): Model configuration files.
 - Class Names (`*.names`): Object classification labels.
 - Images (`*.jpg`): Place your images here to be processed.

Ensure that the images you wish to process are stored in the `resources/` folder.

### Configuration
Modify the visionagent.properties file to set model parameters and thresholds:

```
# YOLO model parameters.
cv.model.weights=[YOUR_YOLO_WEIGHTS_FILE].weights
cv.model.config=[YOUR_YOLO_CONFIG_FILE].cfg
cv.class.names=[YOUR_CLASS_NAMES_FILE].names

# Threshold parameters for detection confidence and non-maximum suppression.
cv.threshold.score=[DETECTION_CONFIDENCE_THRESHOLD]
cv.threshold.nms=[NMS_THRESHOLD]

```

Replace [YOUR_YOLO_WEIGHTS_FILE], [YOUR_YOLO_CONFIG_FILE], and [YOUR_CLASS_NAMES_FILE] with the appropriate filenames for your YOLO model's weights, configuration, and class names files. Replace [DETECTION_CONFIDENCE_THRESHOLD] and [NMS_THRESHOLD] with the values you wish to set for the score and non-maximum suppression thresholds, respectively.

## Running the Agent

### Locally Without Docker
To run the Vision Agent directly on your local machine:

1. Ensure you have Python 3.x installed.
2. Install the necessary dependencies: `pip install -r requirements.txt`
3. Run the application: `python -m visionagent`

## Deploy with Docker:

There are two methods to deploy with docker. 

1. Build the image locally and deploy: 

```
docker-compose -f docker-compose-build.yml up -d
```

2. Pull the image from TWA and spin up the container: 

```
docker-compose up -d
```

## Usage
The Vision Agent can be accessed through the following HTTP endpoints after deployment:

- **Detect Endpoint**:
  - `GET /detect?source=image&path=[IMAGE_FILE_NAME]`
  - This endpoint returns an annotated image with detection boxes drawn around identified objects.
  - Query Parameters:
    - `source`: The type of source, set to `image` for image files.
    - `path`: The relative path to the image file within the resources directory.
  - Example: `http://localhost:9048/detect?source=image&path=test.jpg`
  - Replace `[IMAGE_FILE_NAME]` with the actual name of your image file, including the extension.

- **Update Endpoint**:
  - `GET /update?source=image&path=[IMAGE_FILE_NAME]`
  - This endpoint provides a JSON response containing the history of detected objects for the specified image.
  - Query Parameters are identical to the Detect Endpoint.
  - Example: `http://localhost:9048/update?source=image&path=test.jpg`

Replace `[IMAGE_FILE_NAME]` with the name of your image file with extension specified (e.g. test.jpg or test.png).

Note:

- Ensure that the `path` parameter points to an image file stored within the resources directory.
- The provided image file name should include the appropriate file extension (e.g., `.jpg`, `.png`).


# Author
Jiawei Lai (jlai@cmcl.io), November 2023
