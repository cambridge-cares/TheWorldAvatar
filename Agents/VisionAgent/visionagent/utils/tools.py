import cv2

def load_model(weights_path, cfg_path):
    return cv2.dnn.readNet(weights_path, cfg_path)

def read_properties_file(filename):
    properties = {}
    with open(filename, 'r') as f:
        for line in f:
            line = line.strip()
            if line and not line.startswith('#'):
                key, value = line.split('=', 1)
                properties[key.strip()] = value.strip()
    return properties
