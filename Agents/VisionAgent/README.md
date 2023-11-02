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
The resources directory includes:

 - *visionagent.properties*: Configuration file.
 - Images (*.jpg*): Sample images for detection.
 - YOLO Weights (*.weights*): Pre-trained model weights.
 - YOLO Config (*.cfg*): Model configuration files.
 - Class Names (*.names*): Object classification labels.

### Configuration
Modify the visionagent.properties file:

```
# Input source: 'image' for images, 'video' for video streams.
input.source=image

# Image file for detection.
image.file_name=[YOUR_IMAGE_FILE].jpg

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

`GET /detect`: Returns an image with detection annotations.

`GET /update`: Provides a JSON history of detected objects.

# Author
Jiawei Lai (jlai@cmcl.io), November 2023