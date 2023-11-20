import cv2
import logging
import io
from flask import Flask, jsonify, Response, request, send_file
from visionagent.agent.visionagent import VisionAgent
from visionagent.utils.tools import read_properties_file

# Configure logging
logging.getLogger("cv2").setLevel(logging.ERROR)
logger = logging.getLogger('vision-agent')

# Initialise Flask app
app = Flask(__name__)

def process_request(source,path):
    # Use the pre-configured global 'agent' instance
    # Load properties
    properties = read_properties_file("visionagent/resources/visionagent.properties")

    # Construct the VisionAgent
    agent = VisionAgent(
        video_source=0 if source == 'video' else None,
        image_source=f"visionagent/resources/{path}" if source == 'image' else None,
        weights_path=f"visionagent/resources/{properties['cv.model.weights']}",
        cfg_path=f"visionagent/resources/{properties['cv.model.config']}",
        names_path=f"visionagent/resources/{properties['cv.class.names']}",
        score_threshold=float(properties['cv.threshold.score']),
        nms_threshold=float(properties['cv.threshold.nms'])
    )

    try:
        frame, outputs, height, width = agent.run_one_frame()
        detected_objects, detected_objects_frame = agent.draw_boxes(frame, outputs, height, width)
        return detected_objects, frame, detected_objects_frame
    except Exception as e:
        logger.error(f"An error occurred during Vision Agent processing: {e}")
        return None, None, None

@app.route('/detect', methods=['GET'])
def detect_route():
    source = request.args.get('source')
    path = request.args.get('path')
    detected_objects, frame_with_boxes, _ = process_request(source,path)
    if detected_objects is not None:
        # Encode image with detection boxes
        _, buffer = cv2.imencode('.jpg', frame_with_boxes)
        # Create a file object
        buffer_file = io.BytesIO(buffer.tobytes())
        return Response(
            buffer_file.read(),
            mimetype='image/jpeg',
            headers={'Content-Disposition': 'inline; filename=image.jpg'}
        )
    else:
        return jsonify({'Error': 'Failed to process image'}), 500

@app.route('/update', methods=['GET'])
def update_route():
    source = request.args.get('source')
    path = request.args.get('path')

    _, _, detected_objects = process_request(source,path)

    if detected_objects is not None:
        return jsonify({'object_history': detected_objects})
    else:
        return jsonify({'Error': 'Failed to process image'}), 500

if __name__ == "__main__":
    app.run(host='0.0.0.0', port=9048)
    logger.info('The Vision Agent has started')