package uk.ac.cam.cares.jps.agent.androidstatus;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.regex.Pattern;

@WebServlet(urlPatterns = {"/set", "/get"})
public class AndroidStatusAgent extends JPSAgent{
    private static final Logger LOGGER = LogManager.getLogger(AndroidStatusAgent.class);
    private String equipmentIRI = "";
    private final String EQUIPMENT_IRI_KEY = "equipmentIRI";

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSSS");
        String datetime = dateFormat.format(new Date());
        LOGGER.info("POST Request received at: {}", datetime);

        String url = request.getRequestURI();
        if (url.contains("?")) url = url.split(Pattern.quote("?"))[0];

        if (url.contains("set")) {
            if (!check(request)) {
                throw new JPSRuntimeException("Unable to validate request sent to the agent.");
            }

            this.equipmentIRI = request.getParameter(EQUIPMENT_IRI_KEY);
            LOGGER.info("Successfully set equipment iri to " + equipmentIRI);

            response.setStatus(HttpServletResponse.SC_OK);
            return;
        }

        throw new JPSRuntimeException("Unknown Path");
    }

    private boolean check(HttpServletRequest request) {
        if (request.getParameter(EQUIPMENT_IRI_KEY).isEmpty()) {
            LOGGER.error("equipmentIRI is missing");
            return false;
        }

        return true;
    }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
        response.setContentType("text/json");
        response.setHeader("Access-Control-Allow-Origin", "*");
        response.setHeader("Access-Control-Allow-Methods", "GET,PUT,OPTIONS");
        response.setHeader("Access-Control-Allow-Headers", "Access-Control-Allow-Origin, Content-Type, Accept, Accept-Language, Origin, User-Agent");

        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSSS");
        String datetime = dateFormat.format(new Date());
        LOGGER.info("GET Request received at: {}", datetime);

        String url = request.getRequestURI();
        if (url.contains("?")) url = url.split(Pattern.quote("?"))[0];

        if (url.contains("get")) {
            JSONObject result = new JSONObject();
            result.put("equipmentIRI", equipmentIRI);
            response.setStatus(HttpServletResponse.SC_OK);
            response.getWriter().write(result.toString());
            LOGGER.info("Return: " + result);
            return;
        }

        throw new JPSRuntimeException("Unknown Path");
    }
}
