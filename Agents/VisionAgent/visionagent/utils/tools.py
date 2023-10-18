import cv2

def load_model(weights_path, cfg_path):
    return cv2.dnn.readNet(weights_path, cfg_path)