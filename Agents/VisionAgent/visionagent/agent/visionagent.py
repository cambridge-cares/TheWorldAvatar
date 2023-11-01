import cv2
import numpy as np
from datetime import datetime
from visionagent.utils.tools import load_model

class VisionAgent:
    def __init__(self, video_source=0, image_source=None):
        self.video_source = video_source
        self.image_source = image_source
        self.net = load_model("visionagent/resources/yolov3.weights", "visionagent/resources/yolov3.cfg")
        with open("visionagent/resources/coco.names", "r") as f:
            self.classes = [line.strip() for line in f.readlines()]

    def perform_detection(self, image):
        height, width = image.shape[:2]
        blob = cv2.dnn.blobFromImage(image, 1/255.0, (416, 416), (0, 0, 0), True, crop=False)
        self.net.setInput(blob)
        output_layer_names = self.net.getUnconnectedOutLayersNames()
        outputs = self.net.forward(output_layer_names)
        return outputs, height, width

    def draw_boxes(self, image, outputs, height, width):

        detected_objects = []
        timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

        boxes = []
        confidences = []
        class_ids = []

        for output in outputs:
            for detection in output:
                scores = detection[5:]
                class_id = np.argmax(scores)
                confidence = scores[class_id]
                if confidence > 0.5:
                    box = detection[0:4] * np.array([width, height, width, height])
                    (centerX, centerY, w, h) = box.astype("int")
                    x = int(centerX - (w / 2))
                    y = int(centerY - (h / 2))
                    boxes.append([x, y, int(w), int(h)])
                    confidences.append(float(confidence))
                    class_ids.append(class_id)

        indexes = cv2.dnn.NMSBoxes(boxes, confidences, 0.5, 0.4)

        for i in range(len(boxes)):
            if i in indexes:
                x, y, w, h = boxes[i]
                label = str(self.classes[class_ids[i]])
                confidence = confidences[i]
                cv2.rectangle(image, (x, y), (x + w, y + h), (0, 255, 0), 2)
                cv2.putText(image, f"{label} {round(confidence, 2)}", (x, y - 10), cv2.FONT_HERSHEY_SIMPLEX, 0.5, (0, 255, 0), 2)

                detected_object = {
                    'timestamp':timestamp,
                    'class': label,
                    'confidence': round(confidence, 2)
                }
                detected_objects.append(detected_object)

        return image, detected_objects

    def run_one_frame(self):
        if self.image_source:
            frame = cv2.imread(self.image_source)
            if frame is None:
                raise Exception("Failed to read an image file")
        else:  # Else use video source
            cap = cv2.VideoCapture(self.video_source)
            ret, frame = cap.read()
            if not ret:
                raise Exception("Failed to read a frame from a video source")
            cap.release()

        outputs, height, width = self.perform_detection(frame)
        return frame, outputs, height, width

    def run(self):
        cap = cv2.VideoCapture(self.video_source)

        while True:
            ret, frame = cap.read()
            if not ret:
                print("Failed to grab frame")
                break

            outputs, height, width = self.perform_detection(frame)
            frame_with_boxes = self.draw_boxes(frame, outputs, height, width)
            cv2.imshow('YOLO Object Detection', frame_with_boxes)

            if cv2.waitKey(1) & 0xFF == ord('q'):
                break

        cap.release()
        cv2.destroyAllWindows()

