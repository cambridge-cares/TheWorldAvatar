package uk.ac.cam.cares.jps.aws;

import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.HttpClientBuilder;
import org.json.JSONObject;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;


@Path("/watcher")
public class AsynchronousWatcherService {
    private static final String STATUS_WATCHING = "Watching";
    private static final String STATUS_ERROR = "Error";
    private static final String KEY_WATCH = "watch";
    private static final String KEY_CALLBACK_URL = "callback";
    private static final String ERR_NO_DIR = "Directory does not exist: ";

    @POST
    @Consumes({MediaType.APPLICATION_JSON})
    @Produces({MediaType.APPLICATION_JSON})
    public Response readRequests(String json) {
        Response response = watchObject(json);

        return response;
    }

    private Response watchObject(String json) {
        Response response = new Response();

        try {
            JSONObject args = new JSONObject(json);
            String path = getPath(args);
            String url = args.get(KEY_CALLBACK_URL).toString();
            CreateFileWatcher watcher = new CreateFileWatcher(new File(path));
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

        try {
            path = args.get(KEY_WATCH).toString();
            String dir = new File(path).getAbsoluteFile().getParent();
            if (!Files.isDirectory(Paths.get(dir))) {
                throw new IOException(ERR_NO_DIR + dir);
            }
        } catch (Exception e) {
            throw e;
        }

        return path;
    }

    private WatcherCallback getCallback(String url, String json) {
        WatcherCallback callback = () -> {
            HttpClient httpClient = HttpClientBuilder.create().build();
            try {
                HttpPost request = new HttpPost(url);
                StringEntity entity = new StringEntity(json, ContentType.APPLICATION_JSON);
                request.setEntity(entity);
                httpClient.execute(request);
            } catch (IOException e) {
                e.printStackTrace();
            }
        };

        return callback;
    }

    class Response {
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
