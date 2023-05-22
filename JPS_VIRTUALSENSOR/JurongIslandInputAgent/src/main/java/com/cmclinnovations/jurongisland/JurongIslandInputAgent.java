package com.cmclinnovations.jurongisland;

import java.io.IOException;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONArray;
import org.json.JSONObject;
import org.apache.http.entity.ContentType;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

@WebServlet(urlPatterns = {"/update"})
public class JurongIslandInputAgent extends HttpServlet {
    private static final Logger LOGGER = LogManager.getLogger(JurongIslandInputAgent.class);
    
    private QueryClient queryClient;

    @Override
    public void init() throws ServletException {
        EndpointConfig endpointConfig = new EndpointConfig(); 
        RemoteStoreClient queryStoreClient = new RemoteStoreClient(EnvConfig.QUERY_ENDPOINT);
        RemoteStoreClient updateStoreClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());
        queryClient = new QueryClient(queryStoreClient,updateStoreClient);
    }

    @Override
    public void doPost(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
        LOGGER.info("Received POST request to update pollutant source data for Jurong Island");
        JSONArray pollutantSourceData = queryClient.pollutantSourceQuery();
        queryClient.updateEmissionsData(pollutantSourceData);

        // TODO: Remove this method later once the Pirmasens input agent is created. 
        queryClient.updatePirmasensEmissions();
        JSONObject responseJson = new JSONObject();
        responseJson.put("emissionsUpdate: ","done");
        resp.setContentType(ContentType.APPLICATION_JSON.getMimeType());
        resp.setCharacterEncoding("UTF-8");
        resp.getWriter().print(responseJson);
    }
 
}