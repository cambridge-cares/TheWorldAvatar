package uk.ac.cam.cares.jps.agent.ceavisualisation;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;

import javax.servlet.annotation.WebServlet;

@WebServlet(
        urlPatterns = {
                CEAVisualisationAgent.URI_RUN})
public class CEAVisualisationAgent extends JPSAgent {
    public static final String URI_RUN = "/run";


}
