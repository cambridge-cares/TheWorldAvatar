package uk.ac.cam.cares.jps.agent.sensorloggermobileappquery_agent;


import com.cmclinnovations.stack.clients.ontop.OntopClient;

import net.sf.jsqlparser.statement.select.Offset;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;


import org.locationtech.jts.*;
import org.locationtech.jts.io.*;
import org.locationtech.jts.geom.Geometry;




import org.postgis.Polygon;
import org.springframework.core.io.ClassPathResource;
import uk.ac.cam.cares.jps.agent.sensorloggermobileappquery_agent.objects.PersonGPSPoint;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;

import java.sql.Connection;
import java.sql.SQLException;
import java.time.OffsetDateTime;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;


import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Path;
import java.util.List;


@WebServlet("/geo")
public class SensorLoggerMobileAppQueryAgent extends JPSAgent {

    private static final Logger LOGGER = LogManager.getLogger(SensorLoggerMobileAppQueryAgent.class);
    static final String PREFIX = "http://www.theworldavatar.com/kg/sensorloggermobileappqueryagent/";
    private SLPostGISClient slPostGisClient;
    private QueryClient queryClient;

    @Override
    public void init() throws ServletException {
        EndpointConfig endpointConfig = EnvConfig.ENDPOINT_CONFIG;
        slPostGisClient = new SLPostGISClient(endpointConfig.getDburl(), endpointConfig.getDbuser(), endpointConfig.getDbpassword());
        RemoteStoreClient storeClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());
        RemoteRDBStoreClient remoteRDBStoreClient = new RemoteRDBStoreClient(endpointConfig.getDburl(), endpointConfig.getDbuser(), endpointConfig.getDbpassword());
        TimeSeriesClient<Long> tsClient = new TimeSeriesClient<>(storeClient, Long.class);
        RemoteStoreClient ontopStoreClient = new RemoteStoreClient(endpointConfig.getOntopurl());
        queryClient = new QueryClient(storeClient,ontopStoreClient,remoteRDBStoreClient);
    }

    @Override
    protected void doPost(HttpServletRequest req, HttpServletResponse response)
            throws ServletException, IOException {

        response.setStatus(HttpServletResponse.SC_OK);
        PrintWriter out = response.getWriter();
        out.println("HTTP POST request processed.");


        // EWKT literal for the scope to create
        String ewkt = req.getParameter("ewkt");
        OffsetDateTime lowerBound = OffsetDateTime.parse(req.getParameter("lowerBound"));
        OffsetDateTime upperBound = OffsetDateTime.parse(req.getParameter("upperBound"));

        Polygon polygonProvided = null;
        try {
            polygonProvided = new Polygon(ewkt);
        } catch (SQLException e) {
            LOGGER.error("Failed to parse given EWKT literal", e);
        }



        if (polygonProvided != null) {
            // String scopeIri = null;
            // Polygon polygon4326 = null;
            // try (Connection conn = slPostGisClient.getConnection()) {
            //     if (!slPostGisClient.tableExists(EnvConfig.SCOPE_TABLE_NAME, conn)) {
            //         // first time initialisation
            //         slPostGisClient.createTable(EnvConfig.SCOPE_TABLE_NAME, conn);

            //         // add ontop mapping
            //         Path obdaFile = new ClassPathResource("ontop.obda").getFile().toPath();
            //         new OntopClient().updateOBDA(obdaFile);

            //        // adds OntoAgent instance
            //        queryClient.initialiseAgent();

            //        queryClient.initialiseScopeDerivation(scopeIri);
            //     }

            //     if (polygonProvided.getSrid() != 4326) {
            //         polygon4326 = slPostGisClient.getPolygonAs4326(polygonProvided, conn);
            //     } else {
            //         polygon4326 = polygonProvided;
            //     }

            //     if (!slPostGisClient.scopeExists(polygon4326, conn)) {
            //         scopeIri = slPostGisClient.addScope(polygon4326, conn);
            //     } else {
            //         String responseString = "Given EWKT literal already exists in the database, or the scopeExists query failed, check logs";
            //         response.getWriter().write(String.format("Created scope <%s>", scopeIri));
            //         LOGGER.warn(responseString);
            //     }
            // } catch (SQLException e) {
            //     LOGGER.error("SQL state {}");
            //     LOGGER.error(e.getMessage());
            //     LOGGER.error("Probably failed to close SQL connection or failed to connect");
            // } catch (IOException e) {
            //     LOGGER.error(e.getMessage());
            //     LOGGER.error("Probably failed to add ontop mapping");
            // }






            // get ships within a scope and time
            List<PersonGPSPoint> personGPSPoints;
            try {

                // OffsetDateTime lowerBound = OffsetDateTime.parse("2023-03-07T11:13:42.775012200+08:00");
                // OffsetDateTime upperBound = OffsetDateTime.parse("2023-03-10T11:13:42.775012200+08:00");
                personGPSPoints = queryClient.getPersonGPSPointsWithinTimeAndScopeViaTSClient(lowerBound,upperBound, convert(ewkt));
                for (int i=0; i<personGPSPoints.size();i++)
                {System.out.println(personGPSPoints.get(i).getLocation());}
            
            } catch (Exception e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }



        }



    

    }

    public static Geometry convert(String ewkt) throws Exception {
        WKTReader reader = new WKTReader();
        Geometry polygon = (Geometry) reader.read(ewkt);
        return polygon;
    }





    

    





}

