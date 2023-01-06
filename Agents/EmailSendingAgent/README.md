# Email Sending Agent

This agent is designed to call the EmailSender class in the JPS Base Lib to send an email via the Email Agent. This is a temporary agent for testing the modified version of the Email Agent.

Before building the agent, change the EMAIL_AGENT_URL in the Dockerfile according to where your Email Agent instance is located at. 

To build and start the agent, open up the command prompt in the same directory as this README, run
```
docker-compose up -d
```
The agent is reachable at "email-sending-agent" on localhost port 1021.

#### Run the agent
To run the agent, a POST request must be sent to http://localhost:1021/email-sending-agent/.
Follow the request shown below.

```
curl -X POST --header "Content-Type: application/json" -d "{\"message\":\"start\"}" http://localhost:1021/email-sending-agent/
```