import logging
#from flask import Flask, jsonify
from visionagent.agent.visionagent import VisionAgent

if __name__ == "__main__":

    print('Test...')
    try:
        print('Try...')
        agent = VisionAgent()
        agent.run()
    except Exception as e:
        print(f"An error occurred: {e}")
