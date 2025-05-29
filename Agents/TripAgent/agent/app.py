from flask import Flask
import agent.trip.process_trajectory as process_trajectory

app = Flask(__name__)
app.register_blueprint(process_trajectory.process_trajectory_bp)

if __name__ == "__main__":
    app.run()
