import junit.framework.TestCase;
import org.apache.tomcat.util.http.fileupload.FileUtils;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.aws.AsynchronousWatcherService;
import uk.ac.cam.cares.jps.aws.WatcherCallback;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

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

    public void testNewAsynchronousWatcherServiceMethods() {
        AsynchronousWatcherService aws = new AsynchronousWatcherService();
        assertEquals(4, aws.getClass().getDeclaredMethods().length);
    }

    public void testNewAsynchronousWatcherServiceReadRequestsMethod() throws IOException, NoSuchMethodException {
        AsynchronousWatcherService aws = new AsynchronousWatcherService();
        String file = System.getProperty("java.io.tmpdir") + "/test/test.watch";
        String badReq = "HTTP 400 Bad Request";
        File watchDir = null;
        try {
            assertNotNull(aws.getClass().getDeclaredMethod("readRequests", String.class));
            assertTrue(aws.getClass().getDeclaredMethod("readRequests", String.class).isAnnotationPresent(POST.class));
            assertTrue(aws.getClass().getDeclaredMethod("readRequests", String.class).isAnnotationPresent(Consumes.class));
            assertTrue(aws.getClass().getDeclaredMethod("readRequests", String.class).isAnnotationPresent(Produces.class));
            assertEquals(MediaType.APPLICATION_JSON, aws.getClass().getDeclaredMethod("readRequests", String.class)
                    .getAnnotation(Consumes.class).value()[0]);
            assertEquals(MediaType.APPLICATION_JSON, aws.getClass().getDeclaredMethod("readRequests", String.class)
                    .getAnnotation(Produces.class).value()[0]);
            AsynchronousWatcherService.ResponseBody rsp = new AsynchronousWatcherService.ResponseBody();
            String jsonStr = "";
            rsp.setStatus("Error");
            rsp.setPath(badReq);
            assertEquals(rsp.status, ((AsynchronousWatcherService.ResponseBody) aws.readRequests(jsonStr).getEntity()).status);
            assertEquals(rsp.path, ((AsynchronousWatcherService.ResponseBody) aws.readRequests(jsonStr).getEntity()).path);
            jsonStr = "{}";
            assertEquals(rsp.status, ((AsynchronousWatcherService.ResponseBody) aws.readRequests(jsonStr).getEntity()).status);
            assertEquals(rsp.path, ((AsynchronousWatcherService.ResponseBody) aws.readRequests(jsonStr).getEntity()).path);
            jsonStr = "{\"watch\":\"" + file + "\"}";
            assertEquals(rsp.status, ((AsynchronousWatcherService.ResponseBody) aws.readRequests(jsonStr).getEntity()).status);
            assertEquals(rsp.path, ((AsynchronousWatcherService.ResponseBody) aws.readRequests(jsonStr).getEntity()).path);
            jsonStr = "{\"watch\":\"" + file + "\", \"callback\":\"\"}";
            assertEquals(rsp.status, ((AsynchronousWatcherService.ResponseBody) aws.readRequests(jsonStr).getEntity()).status);
            assertEquals(rsp.path, ((AsynchronousWatcherService.ResponseBody) aws.readRequests(jsonStr).getEntity()).path);
            jsonStr = "{\"watch\":\"" + file + "\", \"callback\":\"http://localhost:8080/back\"}";
            assertEquals(rsp.status, ((AsynchronousWatcherService.ResponseBody) aws.readRequests(jsonStr).getEntity()).status);
            assertTrue(((AsynchronousWatcherService.ResponseBody) aws.readRequests(jsonStr).getEntity()).path.contains("Directory does not exist: "));
            watchDir = new File(file).getParentFile();
            boolean dir = watchDir.mkdirs();
            assertTrue(dir);
            rsp.setStatus("Watching");
            rsp.setPath(file);
            assertEquals(rsp.status, ((AsynchronousWatcherService.ResponseBody) aws.readRequests(jsonStr).getEntity()).status);
            assertEquals(rsp.path, ((AsynchronousWatcherService.ResponseBody) aws.readRequests(jsonStr).getEntity()).path);
        } finally {
            if (watchDir != null && watchDir.isDirectory()) {
                FileUtils.deleteDirectory(watchDir);
            }
        }
    }

    public void testNewAsynchronousWatcherServiceWatchObjectMethod() throws IOException, NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        AsynchronousWatcherService aws = new AsynchronousWatcherService();
        String file = System.getProperty("java.io.tmpdir") + "/test/test.watch";
        File watchDir = null;
        try {
            assertNotNull(aws.getClass().getDeclaredMethod("watchObject", String.class));
            Method watchObject = aws.getClass().getDeclaredMethod("watchObject", String.class);
            watchObject.setAccessible(true);
            AsynchronousWatcherService.ResponseBody rsp = new AsynchronousWatcherService.ResponseBody();
            String jsonStr = "";
            rsp.setStatus("Error");
            rsp.setPath("A JSONObject text must begin with '{' at 0 [character 1 line 1]");
            Object result = watchObject.invoke(aws, jsonStr);
            assertEquals(rsp.status, ((AsynchronousWatcherService.ResponseBody) result).status);
            assertEquals(rsp.path, ((AsynchronousWatcherService.ResponseBody) result).path);
            jsonStr = "{}";
            rsp.setPath("JSONObject[\"watch\"] not found.");
            result = watchObject.invoke(aws, jsonStr);
            assertEquals(rsp.status, ((AsynchronousWatcherService.ResponseBody) result).status);
            assertEquals(rsp.path, ((AsynchronousWatcherService.ResponseBody) result).path);
            jsonStr = "{\"watch\":\"" + file + "\"}";
            result = watchObject.invoke(aws, jsonStr);
            assertTrue(((AsynchronousWatcherService.ResponseBody) result).path.contains("Directory does not exist: "));
            watchDir = new File(file).getParentFile();
            boolean dir = watchDir.mkdirs();
            assertTrue(dir);
            result = watchObject.invoke(aws, jsonStr);
            rsp.setPath("JSONObject[\"callback\"] not found.");
            assertEquals(rsp.status, ((AsynchronousWatcherService.ResponseBody) result).status);
            assertEquals(rsp.path, ((AsynchronousWatcherService.ResponseBody) result).path);
            jsonStr = "{\"watch\":\"" + file + "\", \"callback\":\"\"}";
            result = watchObject.invoke(aws, jsonStr);
            rsp.setStatus("Watching");
            rsp.setPath(file);
            assertEquals(rsp.status, ((AsynchronousWatcherService.ResponseBody) result).status);
            assertEquals(rsp.path, ((AsynchronousWatcherService.ResponseBody) result).path);
        } finally {
            if (watchDir != null && watchDir.isDirectory()) {
                FileUtils.deleteDirectory(watchDir);
            }
        }
    }

    public void testNewAsynchronousWatcherServiceGetPathMethod() throws NoSuchMethodException, IllegalAccessException, IOException, InvocationTargetException {
        AsynchronousWatcherService aws = new AsynchronousWatcherService();
        String file = System.getProperty("java.io.tmpdir") + "/test/test.watch";
        File watchDir = null;

        assertNotNull(aws.getClass().getDeclaredMethod("getPath", JSONObject.class));
        Method getPath = aws.getClass().getDeclaredMethod("getPath", JSONObject.class);
        getPath.setAccessible(true);

        JSONObject args = new JSONObject("{}");
        try {
            getPath.invoke(aws, args);
        } catch (InvocationTargetException e) {
            assertEquals("JSONObject[\"watch\"] not found.", e.getTargetException().getMessage());
        }

        args = new JSONObject("{\"watch\":\"\"}");
        try {
            getPath.invoke(aws, args);
        } catch (InvocationTargetException e) {
            assertEquals("Directory does not exist: ", e.getTargetException().getMessage());
        }
        args = new JSONObject("{\"watch\":\"" + file + "\"}");

        try {
            getPath.invoke(aws, args);
        } catch (InvocationTargetException e) {
            assertTrue(e.getTargetException().getMessage().contains("Directory does not exist: "));
        }
        try {
            watchDir = new File(file).getParentFile();
            boolean dir = watchDir.mkdirs();
            assertTrue(dir);
            String result = (String) getPath.invoke(aws, args);
            assertEquals(result, file);
        } finally {
            if (watchDir != null && watchDir.isDirectory()) {
                FileUtils.deleteDirectory(watchDir);
            }
        }
    }

    public void testNewAsynchronousWatcherServiceValidateInputkMethod() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        AsynchronousWatcherService aws = new AsynchronousWatcherService();
        String badReq = "HTTP 400 Bad Request";
        String jsonStr = "";

        assertNotNull(aws.getClass().getDeclaredMethod("validateInput", String.class));
        Method validateInput = aws.getClass().getDeclaredMethod("validateInput", String.class);
        validateInput.setAccessible(true);

        try {
            validateInput.invoke(aws, jsonStr);
        } catch (InvocationTargetException e) {
            assertEquals(badReq, e.getTargetException().getMessage());
        }

        try {
            jsonStr = "{}";
            validateInput.invoke(aws, jsonStr);
        } catch (InvocationTargetException e) {
            assertEquals(badReq, e.getTargetException().getMessage());
        }

        try {
            jsonStr = "{\"watch\":\"\"}";
            validateInput.invoke(aws, jsonStr);
        } catch (InvocationTargetException e) {
            assertEquals(badReq, e.getTargetException().getMessage());
        }

        try {
            jsonStr = "{\"watch\":\"\", \"callback\":\"\"}";
            validateInput.invoke(aws, jsonStr);
        } catch (InvocationTargetException e) {
            assertEquals(badReq, e.getTargetException().getMessage());
        }

        try {
            jsonStr = "{\"watch\":\"abc\", \"callback\":\"\"}";
            validateInput.invoke(aws, jsonStr);
        } catch (InvocationTargetException e) {
            assertEquals(badReq, e.getTargetException().getMessage());
        }

        try {
            jsonStr = "{\"watch\":\"abc\", \"callback\":\"123\"}";
            validateInput.invoke(aws, jsonStr);
        } catch (InvocationTargetException e) {
            assertEquals(badReq, e.getTargetException().getMessage());
        }

        jsonStr = "{\"watch\":\"abc\", \"callback\":\"http://www.host.com:123/test\"}";
        assertNull(validateInput.invoke(aws, jsonStr));

    }

    public void testNewAsynchronousWatcherServiceResponse() {
        AsynchronousWatcherService.ResponseBody rsp = null;

        try {
            rsp = new AsynchronousWatcherService.ResponseBody();
        } finally {
            assertNotNull(rsp);
            assertEquals(rsp.getClass(), AsynchronousWatcherService.ResponseBody.class);
            assertEquals(2, rsp.getClass().getDeclaredFields().length);
            assertEquals(2, rsp.getClass().getDeclaredMethods().length);
            assertEquals("Watching", rsp.status);
            assertNull(rsp.path);
            rsp.setStatus("test");
            assertEquals("test", rsp.status);
            assertNull(rsp.path);
            rsp.setPath("test");
            assertEquals("test", rsp.path);
        }
    }

}
