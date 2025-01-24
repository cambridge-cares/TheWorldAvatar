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

import com.cmclinnovations.stack.clients.postgis.PostGISClient;

import cares.cam.ac.uk.ouraring.data.User;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

@WebServlet(urlPatterns = { "/" })
public class OuraRingAgent extends HttpServlet {
    RemoteRDBStoreClient remoteRDBStoreClient;

    @Override
    protected void doPost(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
        String endDateTime = req.getParameter("end_datetime");

        Instant upperbound;

        if (endDateTime == null) {
            upperbound = Instant.now();
        } else {
            upperbound = Instant.parse(endDateTime);
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

        try (Connection conn = remoteRDBStoreClient.getConnection()) {
            kgClient.setLastUpdate(users, conn);
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }

        // download data from API
        users.forEach(user -> {
            Instant lowerbound;
            if (user.getHeartRateData().getLastUpdate() == null) {
                lowerbound = upperbound.minus(Duration.ofDays(1));
            } else {
                lowerbound = user.getHeartRateData().getLastUpdate();
            }
            OuraRingApi.setHeartRateData(user, lowerbound, upperbound);
        });

        kgClient.setUserIri(users);
        kgClient.initialiseOntop(users);
        kgClient.setHeartRateIris(users);

        try (Connection conn = remoteRDBStoreClient.getConnection()) {
            kgClient.initialiseTimeSeries(users, conn);
            kgClient.addTimeSeriesData(users, conn);
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void init() throws ServletException {
        remoteRDBStoreClient = PostGISClient.getInstance().getRemoteStoreClient();
    }
}
