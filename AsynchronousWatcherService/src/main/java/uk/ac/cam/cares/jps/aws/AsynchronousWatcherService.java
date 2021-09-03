package uk.ac.cam.cares.jps.aws;

import org.json.JSONObject;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Paths;


@Path("/watcher")
public class AsynchronousWatcherService {
    private static final String STATUS_WATCHING = "Watching";
    private static final String STATUS_ERROR = "Error";
    public static final String KEY_WATCH = "watch";
    public static final String KEY_CALLBACK_URL = "callback";
    private static final String ERR_NO_DIR = "Directory does not exist: ";
    //Default watcher timeout
    public static final int PARAM_TIMEOUT = 48;
    private static final int TIMEOUT_MUL_SECONDS = 1000;
    private static final int TIMEOUT_MUL_MINUTES = TIMEOUT_MUL_SECONDS * 60;
    private static final int TIMEOUT_MUL_HOURS = TIMEOUT_MUL_MINUTES * 60;
    //Timeout for the task can be set to seconds, minutes or hours(default)
    public static final int TIMEOUT_MUL = TIMEOUT_MUL_HOURS;


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
        } catch (BadRequestException e) {
            response.setStatus(STATUS_ERROR);
            response.setPath(e.getLocalizedMessage());
            rsp = Response.status(Response.Status.BAD_REQUEST).entity(response).build();
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
            WatcherCallback callback = watcher.getCallback(url, json);
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
