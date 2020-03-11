package uk.ac.cam.cares.jps.aws;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Paths;

import javax.ws.rs.BadRequestException;
import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.HttpClientBuilder;
import org.json.JSONObject;


@Path("/watcher")
public class AsynchronousWatcherService {
    private static final String STATUS_WATCHING = "Watching";
    private static final String STATUS_ERROR = "Error";
    private static final String KEY_WATCH = "watch";
    private static final String KEY_CALLBACK_URL = "callback";
    private static final String ERR_NO_DIR = "Directory does not exist: ";
    //Default watcher timeout
    private final int PARAM_TIMEOUT = 48;
    private final int TIMEOUT_MUL_SECONDS = 1000;
    private final int TIMEOUT_MUL_MINUTES = TIMEOUT_MUL_SECONDS * 60;
    private final int TIMEOUT_MUL_HOURS = TIMEOUT_MUL_MINUTES * 60;
    //Timeout for the task can be set to seconds, minutes or hours(default)
    private final int TIMEOUT_MUL = TIMEOUT_MUL_HOURS;


    @POST
    @Consumes({MediaType.APPLICATION_JSON})
    @Produces({MediaType.APPLICATION_JSON})
    public Response readRequests(String json) {
        Response rsp;
        ResponseBody response = new ResponseBody();

        try {
            validateInput(json);
            response = watchObject(json);
            rsp = Response.ok().entity(response).build();
            System.out.println("Response: " + response);
        } catch (BadRequestException e) {
            response.setStatus(STATUS_ERROR);
            response.setPath(e.getLocalizedMessage());
            rsp = Response.status(Response.Status.BAD_REQUEST).entity(response).build();
            System.out.println("Response: " + response);
        }

        return rsp;

    }

    private ResponseBody watchObject(String json) {
        ResponseBody response = new ResponseBody();

        try {
            JSONObject args = new JSONObject(json);
            String path = getPath(args);
            String url = args.get(KEY_CALLBACK_URL).toString();
            CreateFileWatcher watcher = new CreateFileWatcher(new File(path), PARAM_TIMEOUT * TIMEOUT_MUL);
            WatcherCallback callback = getCallback(url, json);
            watcher.setCallback(callback);
            watcher.start();
            response.setPath(path);
        } catch (Exception e) {
            response.setStatus(STATUS_ERROR);
            response.setPath(e.getLocalizedMessage());
        }

        return response;
    }

    private String getPath(JSONObject args) throws Exception {
        String path;

        path = args.get(KEY_WATCH).toString();
        String dir = new File(path).getAbsoluteFile().getParent();
        if (path.equals("") || !Files.isDirectory(Paths.get(dir))) {
            String msg = ERR_NO_DIR;
            if (!path.equals("")) {
                msg = msg + dir;
            }
            throw new IOException(msg);
        }

        return path;
    }

    private WatcherCallback getCallback(String url, String json) {

        return () -> {
            HttpClient httpClient = HttpClientBuilder.create().build();
            try {
                HttpPost request = new HttpPost(url);
                StringEntity entity = new StringEntity(json, ContentType.APPLICATION_JSON);
                request.setEntity(entity);
                System.out.println("Calling back " + url + " POST: " + json);
                httpClient.execute(request);
            } catch (IOException e) {
                e.printStackTrace();
            }
        };
    }

    private void validateInput(String json) {

        try {
            JSONObject args = new JSONObject(json);
            String url = args.get(KEY_CALLBACK_URL).toString();
            String path = args.get(KEY_WATCH).toString();
            URI.create(url).toURL();
            new File(path).getAbsoluteFile().getParentFile();
        } catch (Exception e) {
            throw new BadRequestException();
        }

    }

    public static class ResponseBody {
        public String status = STATUS_WATCHING;
        public String path;

        public void setStatus(String s) {
            status = s;
        }

        public void setPath(String p) {
            path = p;
        }
    }
}
