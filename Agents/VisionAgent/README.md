# Vision Agent

Vision Agent is designed to leverage computer vision models for object detection in images and videos. Utilising state-of-the-art models such as YOLO (You Only Look Once), this application can identify and categorise objects returning JSON objects.

## Table of Contents

- [Vision Agent](#vision-agent)
  - [Table of Contents](#table-of-contents)
  - [Installation](#installation)
  - [Resources](#resources)
    - [Configuration](#configuration)
  - [Docker Deployment](#docker-deployment)
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
Modify the visionagent.properties file:

```
# YOLO model parameters.
cv.model.weights=[YOUR_YOLO_WEIGHTS_FILE].weights
cv.model.config=[YOUR_YOLO_CONFIG_FILE].cfg
cv.class.names=[YOUR_CLASS_NAMES_FILE].names
```

## Docker Deployment

Deploy with Docker:

```
docker-compose build

docker-compose up -d
```

## Usage
Endpoints when running on http://localhost:9048:

`GET /detect?source=image&path=[IMAGE_FILE_NAME]`: Returns an image with detection annotations. Replace `[IMAGE_FILE_NAME]` with the name of your image file.

`GET /update?source=image&path=[IMAGE_FILE_NAME]`: Provides a JSON history of detected objects for the specified image. Replace `[IMAGE_FILE_NAME]` with the name of your image file.

Note: The system now accepts the image source and file path as query parameters, allowing dynamic selection of the image to be processed without the need to modify the configuration files.

# Author
Jiawei Lai (jlai@cmcl.io), November 2023
