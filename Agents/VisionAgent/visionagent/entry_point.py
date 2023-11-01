import cv2
import logging
import io
from flask import Flask, jsonify, Response, request, send_file
from visionagent.agent.visionagent import VisionAgent

# Configure logging
logging.getLogger("cv2").setLevel(logging.ERROR)
logger = logging.getLogger('prod')

# Initialise Flask app
app = Flask(__name__)

# Create an instance of the VisionAgent class
agent = VisionAgent()

def process_request(image_source_keyword):

    if image_source_keyword == "yes":
        image_source = "visionagent/resources/test01.jpg"
    else:
        image_source = None
    
    agent = VisionAgent(image_source=image_source)
    
    try:
        frame, outputs, height, width = agent.run_one_frame()
        detected_objects, _ = agent.draw_boxes(frame, outputs, height, width)
        return detected_objects, frame
    except Exception as e:
        logger.error(f"An error occurred during Vision Agent processing: {e}")
        return None, None

@app.route('/detect', methods = ['GET'])
def detect_route():

    image_source = request.args.get('image_source', None)
    detected_objects, frame = process_request(image_source)
    
    if detected_objects is not None:
        # Encode image
        _, buffer = cv2.imencode('.jpg', frame)
        
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

    image_source = request.args.get('image_source', None)
    detected_objects, _ = process_request(image_source)
    
    if detected_objects is not None:
        return jsonify({'object_history': detected_objects})
    else:
        return jsonify({'Error': 'Failed to process image'}), 500

if __name__ == "__main__":
    app.run(host='0.0.0.0', port=9048)
    logger.info('The Vision Agent has started')