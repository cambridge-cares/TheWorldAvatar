package uk.ac.cam.cares.jps.agent.emailsending;

import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.email.EmailSender;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import javax.servlet.annotation.WebServlet;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;


@WebServlet(urlPatterns = {"/send"})
public class EmailSendingAgent extends JPSAgent{
	/**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(EmailSendingAgent.class);
    
    private static final String SENDEMAIL_ERROR_MSG = "Unable to send the email!" ;
    
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
    	JSONObject jsonMessage = new JSONObject();
        LOGGER.info("Passing request to Agent..");
        String[] args = new String[] {requestParams.getString("message")};
        jsonMessage = initializeAgent(args);
        jsonMessage.put("message","POST request has been sent successfully.");
        requestParams = jsonMessage;
        return requestParams;
}
    
    
    public JSONObject initializeAgent(String[] args) {
        EmailSender email = new EmailSender();

    	try {
			email.sendEmail("testing", "The email was sent successfully.");
            JSONObject result = new JSONObject();
            result.put("message", "The email was sent successfully.");
            return result;
		} catch (Exception e) {
            throw new JPSRuntimeException(SENDEMAIL_ERROR_MSG);
		}
		

    }
}
