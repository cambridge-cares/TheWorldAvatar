package cares.cam.ac.uk.ouraring;

import java.io.IOException;
import java.sql.Connection;
import java.sql.SQLException;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;

import com.cmclinnovations.stack.clients.postgis.PostGISClient;

import cares.cam.ac.uk.ouraring.data.User;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

@WebServlet(urlPatterns = { "/" })
public class OuraRingAgent extends HttpServlet {
    private static final Logger LOGGER = LogManager.getLogger(OuraRingAgent.class);
    RemoteRDBStoreClient remoteRDBStoreClient;

    @Override
    protected void doPost(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
        String startDateTime = req.getParameter("start_datetime");
        String endDateTime = req.getParameter("end_datetime");

        LOGGER.info("Received request with parameters");
        LOGGER.info("start_datetime = {}", startDateTime);
        LOGGER.info("end_datetime = {}", endDateTime);

        Instant startDateTimeInstant = null;

        Instant endDateTimeInstant = null;

        if (endDateTime == null) {
            endDateTimeInstant = Instant.now();
        } else {
            endDateTimeInstant = Instant.parse(endDateTime);
        }

        if (startDateTime == null) {
            // heart rate API restricts range to 30 days
            startDateTimeInstant = endDateTimeInstant.minus(Duration.ofDays(30));
        } else {
            startDateTimeInstant = Instant.parse(startDateTime);
        }

        PostgresClient postgresClient = new PostgresClient();
        Map<String, String> userToOuraApiMap = postgresClient.getUserToOuraApiMap();

        List<User> users = new ArrayList<>();
        userToOuraApiMap.keySet().forEach(userId -> {
            User user = new User(userId);
            user.setOuraApiKey(userToOuraApiMap.get(userId));
            users.add(user);
        });

        KgClient kgClient = new KgClient();

        kgClient.setUserIri(users);
        kgClient.initialiseOntop(users);
        kgClient.setHeartRateIris(users);

        try (Connection conn = remoteRDBStoreClient.getConnection()) {
            kgClient.setLastUpdate(users, conn);
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }

        // download data from API
        // each user may have a different last update, so the API call is customised for
        // each user
        for (User user : users) {
            Instant lowerbound;
            Instant upperbound;
            if (user.getHeartRateData().getLastUpdate() == null) {
                lowerbound = startDateTimeInstant;
            } else {
                lowerbound = user.getHeartRateData().getLastUpdate().plusMillis(1);
            }

            // heart rate API restricts range to 30 days
            if (Duration.between(lowerbound, endDateTimeInstant).toDays() > 30) {
                upperbound = lowerbound.plus(Duration.ofDays(30));
            } else {
                upperbound = endDateTimeInstant;
            }

            OuraRingApi.setHeartRateData(user, lowerbound, upperbound);
        }

        try (Connection conn = remoteRDBStoreClient.getConnection()) {
            kgClient.initialiseTimeSeries(users, conn);
            kgClient.addTimeSeriesData(users, conn);
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }

        LOGGER.info("Update complete");
    }

    @Override
    public void init() throws ServletException {
        remoteRDBStoreClient = PostGISClient.getInstance().getRemoteStoreClient();
    }
}
