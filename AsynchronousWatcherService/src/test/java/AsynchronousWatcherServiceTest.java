import junit.framework.TestCase;
import org.apache.tomcat.util.http.fileupload.FileUtils;
import uk.ac.cam.cares.jps.aws.AsynchronousWatcherService;

import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import java.io.File;
import java.io.IOException;

public class AsynchronousWatcherServiceTest extends TestCase {

    public void testNewAsynchronousWatcherService() {
        AsynchronousWatcherService aws = null;
        try {
            aws = new AsynchronousWatcherService();
        } finally {
            assertNotNull(aws);
            assertEquals(aws.getClass(), AsynchronousWatcherService.class);
            assertEquals(1, aws.getClass().getAnnotations().length);
            assertTrue(aws.getClass().isAnnotationPresent(Path.class));
            assertEquals("/watcher", aws.getClass().getAnnotation(Path.class).value());
        }
    }

    public void testNewAsynchronousWatcherServiceFields() {
        AsynchronousWatcherService aws = new AsynchronousWatcherService();
        assertEquals(10, aws.getClass().getDeclaredFields().length);
    }

    public void testNewAsynchronousWatcherServiceMethods() throws IOException, NoSuchMethodException {
        AsynchronousWatcherService aws = new AsynchronousWatcherService();
        assertEquals(5, aws.getClass().getDeclaredMethods().length);
        String file = "./tmp/test.watch";
        try {
            assertTrue(aws.getClass().getDeclaredMethod("readRequests", String.class).isAnnotationPresent(POST.class));
            assertTrue(aws.getClass().getDeclaredMethod("readRequests", String.class).isAnnotationPresent(Consumes.class));
            assertTrue(aws.getClass().getDeclaredMethod("readRequests", String.class).isAnnotationPresent(Produces.class));
            assertEquals(MediaType.APPLICATION_JSON, aws.getClass().getDeclaredMethod("readRequests", String.class)
                    .getAnnotation(Consumes.class).value()[0]);
            assertEquals(MediaType.APPLICATION_JSON, aws.getClass().getDeclaredMethod("readRequests", String.class)
                    .getAnnotation(Produces.class).value()[0]);
            String jsonStr = "{}";
            AsynchronousWatcherService.Response rsp = new AsynchronousWatcherService.Response();
            rsp.setStatus("Error");
            rsp.setPath("JSONObject[\"watch\"] not found.");
            assertEquals(rsp.status, aws.readRequests(jsonStr).status);
            assertEquals(rsp.path, aws.readRequests(jsonStr).path);
            jsonStr = "{\"watch\":\"" + file + "\"}";
            assertEquals(rsp.status, aws.readRequests(jsonStr).status);
            assertTrue(aws.readRequests(jsonStr).path.contains("Directory does not exist: "));
            boolean dir = new File(file).getParentFile().mkdirs();
            assertTrue(dir);
            rsp.setPath("JSONObject[\"callback\"] not found.");
            assertEquals(rsp.status, aws.readRequests(jsonStr).status);
            assertEquals(rsp.path, aws.readRequests(jsonStr).path);
            jsonStr = "{\"watch\":\"" + file + "\", \"callback\":\"\"}";
            rsp.setPath("HTTP 400 Bad Request");
            assertEquals(rsp.status, aws.readRequests(jsonStr).status);
            assertEquals(rsp.path, aws.readRequests(jsonStr).path);
            jsonStr = "{\"watch\":\"" + file + "\", \"callback\":\"http://localhost:8080/back\"}";
            rsp.setStatus("Watching");
            rsp.setPath(file);
            assertEquals(rsp.status, aws.readRequests(jsonStr).status);
            assertEquals(rsp.path, aws.readRequests(jsonStr).path);
            //@todo: [AC] test remaining methods
        } finally {
            FileUtils.deleteDirectory(new File(new File(file).getParent()));
        }
    }

}
