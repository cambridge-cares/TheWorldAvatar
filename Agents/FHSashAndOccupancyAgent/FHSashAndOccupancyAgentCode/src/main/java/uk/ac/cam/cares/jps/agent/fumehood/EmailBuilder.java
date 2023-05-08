package uk.ac.cam.cares.jps.agent.fumehood;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.email.EmailSender;

import java.text.DecimalFormat;
import java.util.*;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;


/**
 * Class to construct an email content and send the email content to the email agent via the email sender
 **/
public class EmailBuilder {
	/**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(FHSashAndOccupancyAgent.class);

    public static final String SENDEMAIL_ERROR_MSG = "Unable to send email via the email agent!";

    public void parsesMapAndPostProcessing(Map<String, List<String>> map, Double threshold) {
        StringBuilder sb = new StringBuilder();
        DecimalFormat df = new DecimalFormat("####0.00");
        sb.append("Listed below are the fumehoods, walkin-fumehoods, their occupied state and sash opening values: <br> <br>");
        
        sb.append("<style>" +
        "td { padding: 6px; border: 1px solid #ccc; text-align: left; }" + 
        "th { background: #333; color: white; font-weight: bold; padding: 6px; border: 1px solid #ccc; text-align: left;}" +
        "</style>");

        //String emailMessages = "Listed below are the fumehoods, walkin-fumehoods, their occupied state and sash opening values: <br>";
        sb.append("<table>");
        sb.append("<tr>");
        sb.append("<th>Devices");
        sb.append("</th>");
        sb.append("<th>Occupied State");
        sb.append("</th>");
        sb.append("<th>Sash Opening");
        sb.append("</th>");
        sb.append("</tr>");
        EmailSender sender = new EmailSender();
        
        for (int i = 0; i < map.get("FHandWFH").size(); i++){
            String occupiedStateData = map.get("OccupiedStateTsData").get(i);
            String sashOpeningData = map.get("SashOpeningTsData").get(i);
            String occupiedStateTimeStamp = map.get("OccupiedStateTimeStamps").get(i);
            String sashOpeningTimeStamp = map.get("SashOpeningTimeStamps").get(i);

            if (occupiedStateData.contains("This device does not have an occupied state.") && sashOpeningData.contains("This device does not have a sash opening.")) {
                sb.append("<tr>");
                sb.append("<td> " + map.get("Label").get(i));
                sb.append("</td>");
                sb.append("<td> " + occupiedStateData);
                sb.append("</td>");
                sb.append("<td> " + sashOpeningData);
                sb.append("</td>");
                sb.append("</tr>");
            }
            else if (occupiedStateData.contains("This device does not have an occupied state.") && !sashOpeningData.contains("This device does not have a sash opening.")) {
                Double sashOpeningValue = Double.parseDouble(sashOpeningData);
                sb.append("<tr>");
                sb.append("<td> " + map.get("Label").get(i));
                sb.append("</td>");
                sb.append("<td> " + occupiedStateData);
                sb.append("</td>");
                sb.append("<td> " + df.format(sashOpeningValue) + " % since the following timestamp: " + sashOpeningTimeStamp);
                sb.append("</td>");
                sb.append("</tr>");
            }
            else if (!occupiedStateData.contains("This device does not have an occupied state.") && sashOpeningData.contains("This device does not have a sash opening.")) {
                Double occupiedStateValue = Double.parseDouble(occupiedStateData);
                String occupiedState;
                if (occupiedStateValue == 0.0) {
                    occupiedState = "Not occupied";
                } else {
                    occupiedState = "Occupied";
                }
                sb.append("<tr>");
                sb.append("<td> " + map.get("Label").get(i));
                sb.append("</td>");
                sb.append("<td> " + occupiedState + " since the following timestamp: " + occupiedStateTimeStamp);
                sb.append("</td>");
                sb.append("<td> " + sashOpeningData);
                sb.append("</td>");
                sb.append("</tr>");
            } 
            else {
                Double occupiedStateValue = Double.parseDouble(occupiedStateData);
                Double sashOpeningValue = Double.parseDouble(sashOpeningData);
                String occupiedState;
                
                if (occupiedStateValue == 0.0) {
                    occupiedState = "Not occupied";
                } else {
                    occupiedState = "Occupied";
                }
                if (occupiedState.contains("Not occupied") && sashOpeningValue > threshold) {
                    sb.append("<tr>");
                    sb.append("<td style=\"color:Red;\"> " + map.get("Label").get(i));
                    sb.append("</td>");
                    sb.append("<td style=\"color:Red;\"> " + occupiedState + " since the following timestamp: " + occupiedStateTimeStamp);
                    sb.append("</td>");
                    sb.append("<td style=\"color:Red;\"> " + df.format(sashOpeningValue) + " % since the following timestamp: " + sashOpeningTimeStamp);
                    sb.append("</td>");
                    sb.append("</tr>");
                } else {
                    sb.append("<tr>");
                    sb.append("<td> " + map.get("Label").get(i));
                    sb.append("</td>");
                    sb.append("<td> " + occupiedState + " since the following timestamp: " + occupiedStateTimeStamp);
                    sb.append("</td>");
                    sb.append("<td> "  + df.format(sashOpeningValue) + " % since the following timestamp: " + sashOpeningTimeStamp);
                    sb.append("</td>");
                    sb.append("</tr>");
                }
            }
        }

        try {
            sb.append("</table>");
            LOGGER.info("The email message is " + sb.toString());
            sender.sendEmail("Fumehoods and WalkIn-Fumehoods Sash and Occupancy Alert!", sb.toString());
        } catch (Exception e) {
            throw new JPSRuntimeException(SENDEMAIL_ERROR_MSG, e);
        }
    }
}