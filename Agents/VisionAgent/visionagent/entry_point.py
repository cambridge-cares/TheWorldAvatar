import cv2
import base64
import logging
import io
from flask import Flask, jsonify, Response, send_file
from visionagent.agent.visionagent import VisionAgent

logging.getLogger("cv2").setLevel(logging.ERROR)
logger = logging.getLogger('prod')

app = Flask(__name__)

# Create the instance of the VisionAgent class
agent = VisionAgent()

@app.route('/detect', methods = ['GET'])
def detect_route():
    try:
        frame, outputs, height, width = agent.run_one_frame()
        detected_objects = agent.draw_boxes(frame, outputs, height, width)

        #return jsonify({'detected_objects': detected_objects})
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
        return jsonify({'Error': str(e)}), 500
    
if __name__ == "__main__":
    app.run(host='0.0.0.0', port=9048)
    logger.info('App started')