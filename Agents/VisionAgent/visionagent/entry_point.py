import cv2
import logging
import io
from flask import Flask, jsonify, Response, send_file
from visionagent.agent.visionagent import VisionAgent

# Configure logging
logging.getLogger("cv2").setLevel(logging.ERROR)
logger = logging.getLogger('prod')

# Initialize Flask app
app = Flask(__name__)

# Create an instance of the VisionAgent class
agent = VisionAgent()

@app.route('/detect', methods = ['GET'])
def detect_route():
    try:
        frame, outputs, height, width = agent.run_one_frame()
        detected_objects, _ = agent.draw_boxes(frame, outputs, height, width)

        # Encode image
        _, buffer = cv2.imencode('.jpg', detected_objects)
        
        # Create a file object
        buffer_file = io.BytesIO(buffer.tobytes())
        
        return Response(
            buffer_file.read(),
            mimetype='image/jpeg',
            headers={'Content-Disposition': 'inline; filename=image.jpg'}
        )

    except Exception as e:
        logger.error(f"An error occurred during Vision Agent detection: {e}")
        return jsonify({'Error': str(e)}), 500

@app.route('/update', methods=['GET'])
def update_route():
    try:
        frame, outputs, height, width = agent.run_one_frame()
        _, detected_objects = agent.draw_boxes(frame, outputs, height, width)
        return jsonify({'object_history': detected_objects})

    except Exception as e:
        logger.error(f"An error occurred during Vision Agent update: {e}")
        return jsonify({'Error': str(e)}), 500

if __name__ == "__main__":
    app.run(host='0.0.0.0', port=9048)
    logger.info('The Vision Agent has started')