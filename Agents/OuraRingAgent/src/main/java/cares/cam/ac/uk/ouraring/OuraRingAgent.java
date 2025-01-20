package cares.cam.ac.uk.ouraring;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import cares.cam.ac.uk.ouraring.data.User;

@WebServlet(urlPatterns = { "/" })
public class OuraRingAgent extends HttpServlet {

    @Override
    protected void doPost(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
        PostgresClient postgresClient = new PostgresClient();
        Map<String, String> userToOuraApiMap = postgresClient.getUserToOuraApiMap();

        List<User> users = new ArrayList<>();
        userToOuraApiMap.keySet().forEach(userId -> {
            User user = new User(userId);
            user.setOuraApiKey(userToOuraApiMap.get(userId));
            users.add(user);
        });

    }
}
