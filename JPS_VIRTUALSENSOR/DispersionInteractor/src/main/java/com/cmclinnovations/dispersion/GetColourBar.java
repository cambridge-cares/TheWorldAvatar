package com.cmclinnovations.dispersion;

import java.io.IOException;
import java.time.Instant;
import java.util.Base64;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

@WebServlet(urlPatterns = { "/GetColourBar" })
public class GetColourBar extends HttpServlet {
    private static final Logger LOGGER = LogManager.getLogger(GetColourBar.class);
    QueryClient queryClient;

    @Override
    protected void doGet(HttpServletRequest req, HttpServletResponse resp) {
        String pollutant = req.getParameter("pollutant");
        String timestep = req.getParameter("timestep");
        String derivationIri = req.getParameter("derivationIri");
        String zIri = req.getParameter("zIri");

        String colourBarUrl = queryClient.getColourBarURL(pollutant, Instant.parse(timestep).getEpochSecond(),
                derivationIri, zIri);

        HttpGet httpGet = new HttpGet(colourBarUrl);
        String auth = "fs_user" + ":" + "fs_pass"; // default credentials for the file server container
        String encodedAuth = Base64.getEncoder().encodeToString(auth.getBytes());
        httpGet.setHeader("Authorization", "Basic " + encodedAuth);

        try (CloseableHttpClient httpClient = HttpClients.createDefault();
                CloseableHttpResponse httpResponse = httpClient.execute(httpGet)) {
            httpResponse.getEntity().getContent().transferTo(resp.getOutputStream());
        } catch (IOException e) {
            LOGGER.error(e.getMessage());
            LOGGER.error("Failed while closing httpClient or httpResponse");
        }

    }

    @Override
    public void init() throws ServletException {
        EndpointConfig endpointConfig = Config.ENDPOINT_CONFIG;
        RemoteStoreClient storeClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());
        RemoteRDBStoreClient remoteRDBStoreClient = new RemoteRDBStoreClient(endpointConfig.getDburl(),
                endpointConfig.getDbuser(), endpointConfig.getDbpassword());
        TimeSeriesClient<Long> tsClient = new TimeSeriesClient<>(storeClient, Long.class);
        TimeSeriesClient<Instant> tsClientInstant = new TimeSeriesClient<>(storeClient,
                Instant.class);
        queryClient = new QueryClient(storeClient, remoteRDBStoreClient, tsClient, tsClientInstant);
        queryClient.setOntopUrl(endpointConfig.getOntopUrl());
    }
}
