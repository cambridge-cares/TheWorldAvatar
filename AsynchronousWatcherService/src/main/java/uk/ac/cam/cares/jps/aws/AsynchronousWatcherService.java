package uk.ac.cam.cares.jps.aws;

import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.HttpClientBuilder;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import java.io.File;
import java.io.IOException;


@Path("/watcher")
public class AsynchronousWatcherService {
    private static final String STATUS_WATCHING = "Watching";

    @POST
    @Consumes({MediaType.APPLICATION_JSON})
    @Produces({MediaType.APPLICATION_JSON})
    public Response readRequests(String json_str) {
        String watched = watchObject(json_str);
        Response response = new Response();
        response.setPath(watched);
        return response;
    }


    private String watchObject(String json_str) {
        //@TODO AC: extract watch file path and callback url from the json string provided in the POST request body.
        String path = "/tmp/watch/results.csv";
        String url = null;

        WatcherCallback callback = () -> {
            HttpClient httpClient = HttpClientBuilder.create().build();
            try {
                HttpPost request = new HttpPost(url);
                StringEntity entity = new StringEntity(json_str, ContentType.APPLICATION_JSON);
                request.setEntity(entity);
                httpClient.execute(request);
            } catch (IOException e) {
                e.printStackTrace();
            }
        };
        CreateFileWatcher watcher = new CreateFileWatcher(new File(path));
        watcher.setCallback(callback);
        watcher.start();
        return path;
    }

    class Response {
        public String status = STATUS_WATCHING;
        public String path;

        public void setPath(String p) {
            path = p;
        }
    }
}
