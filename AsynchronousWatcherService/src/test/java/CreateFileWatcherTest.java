import junit.framework.TestCase;
import org.apache.tomcat.util.http.fileupload.FileUtils;
import uk.ac.cam.cares.jps.aws.AsynchronousWatcherService;
import uk.ac.cam.cares.jps.aws.CreateFileWatcher;
import uk.ac.cam.cares.jps.aws.Watcher;
import uk.ac.cam.cares.jps.aws.WatcherCallback;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.concurrent.atomic.AtomicBoolean;

public class CreateFileWatcherTest extends TestCase {

    public void testNewCreateFileWatcher() {
        CreateFileWatcher cfw = null;
        try {
            cfw = new CreateFileWatcher(new File(System.getProperty("java.io.tmpdir") + "/test/test.watch"), 1);
        } finally {
            assertNotNull(cfw);
            assertEquals(cfw.getClass(), CreateFileWatcher.class);
            assertEquals(0, cfw.getClass().getAnnotations().length);
            assertEquals(cfw.getClass().getGenericSuperclass(), Thread.class);
            assertEquals(cfw.getClass().getGenericInterfaces()[0], Watcher.class);
        }
    }

    public void testNewCreateFileWatcherFields() throws NoSuchFieldException, IllegalAccessException {
        File fileIn = new File(System.getProperty("java.io.tmpdir") + "/test/test.watch");
        CreateFileWatcher cfw = new CreateFileWatcher(fileIn, 1);
        Field file = null;
        Field timeout = null;
        Field stop = null;
        Field callback = null;
        Field anyFile = null;
        assertEquals(5, cfw.getClass().getDeclaredFields().length);
        try {
            file = cfw.getClass().getDeclaredField("file");
            file.setAccessible(true);
            timeout = cfw.getClass().getDeclaredField("timeout");
            timeout.setAccessible(true);
            stop = cfw.getClass().getDeclaredField("stop");
            stop.setAccessible(true);
            callback = cfw.getClass().getDeclaredField("callback");
            callback.setAccessible(true);
            anyFile = cfw.getClass().getDeclaredField("anyFile");
            anyFile.setAccessible(true);
        }   finally {
            assertEquals(file.get(cfw), fileIn);
            assertEquals(timeout.get(cfw), 1);
            assertEquals(false, ((AtomicBoolean) stop.get(cfw)).get());
            assertNull(callback.get(cfw));
            assertFalse((Boolean) anyFile.get(cfw));
        }
    }

    public void testNewCreateFileWatcherMethods() throws NoSuchFieldException, IllegalAccessException, IOException {
        File fileIn = new File(System.getProperty("java.io.tmpdir") + "/test/test.watch");
        new File(fileIn.getParent()).mkdirs();
        CreateFileWatcher cfw = new CreateFileWatcher(fileIn, 1000);
        WatcherCallback cb = () -> {assertTrue(true);};
        Field stop = null;
        Field callback = null;
        try {
            stop = cfw.getClass().getDeclaredField("stop");
            stop.setAccessible(true);
            callback = cfw.getClass().getDeclaredField("callback");
            callback.setAccessible(true);
        } finally {
            assertEquals(false, ((AtomicBoolean) stop.get(cfw)).get());
            assertNull(callback.get(cfw));
            assertFalse(cfw.isStopped());
            cfw.stopThread();
            assertTrue(cfw.isStopped());
            cfw.setCallback(cb);
            assertNotNull(callback.get(cfw));
            assertEquals(callback.get(cfw).getClass().getInterfaces()[0], WatcherCallback.class);
            assertFalse(cfw.isAlive());
            cfw.start();
            assertTrue(cfw.isAlive());
            cfw.doOnChange();
            assertTrue(cfw.isStopped());
        }
        if (fileIn.getParentFile().isDirectory()) {
            cfw.suspend();
            FileUtils.deleteDirectory(fileIn.getParentFile());
        }
    }

    public void testNewCreateFileWatcherRunMethodNotTimedout() throws IOException {
        File fileIn = new File(System.getProperty("java.io.tmpdir") + "tmp/test.watch");
        File fileOut = new File(System.getProperty("java.io.tmpdir") + "tmp/test.callback");
        new File(fileIn.getParent()).mkdirs();
        CreateFileWatcher cfw = new CreateFileWatcher(fileIn, 60000);
        WatcherCallback cb = () -> {
            try {
                assertTrue(fileOut.createNewFile());
            } catch (IOException e) {
                assertTrue(false);
            }
        };
        cfw.setCallback(cb);
        try {
            cfw.start();
            assertFalse(cfw.isStopped());
            Thread.sleep(500);
            fileIn.createNewFile();
            Thread.sleep(500);
        } catch (InterruptedException e) {
            assertTrue(false);
        } finally {
            if (fileIn.getParentFile().isDirectory()) {
                cfw.suspend();
                FileUtils.deleteDirectory(fileIn.getParentFile());
            }
        }

    }

    public void testNewCreateFileWatcherRunMethodTimedout() throws IOException {
        File fileIn = new File(System.getProperty("java.io.tmpdir") + "/test/test.watch");
        File fileOut = new File(System.getProperty("java.io.tmpdir") + "/test/test.callback");
        new File(fileIn.getParent()).mkdirs();
        CreateFileWatcher cfw = new CreateFileWatcher(fileIn, 100);
        WatcherCallback cb = () -> {
            try {
                assertTrue(fileOut.createNewFile());
            } catch (IOException e) {
                assertTrue(false);
            }
        };
        cfw.setCallback(cb);
        try {
            cfw.start();
            assertFalse(cfw.isStopped());
            Thread.sleep(500);
            fileIn.createNewFile();
            Thread.sleep(500);
        } catch (InterruptedException e) {
            assertTrue(false);
        } finally {
            assertFalse(fileOut.exists());
        }
        if (fileIn.getParentFile().isDirectory()) {
            cfw.suspend();
            FileUtils.deleteDirectory(fileIn.getParentFile());
        }
    }

    public void testNewCreateFileWatcherGetCallbackMethod() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        CreateFileWatcher aws = new CreateFileWatcher(new File(System.getProperty("java.io.tmpdir") + "/test/test.watch"), 1);

        assertNotNull(aws.getClass().getDeclaredMethod("getCallback", String.class, String.class));
        Method getCallback = aws.getClass().getDeclaredMethod("getCallback", String.class, String.class);
        getCallback.setAccessible(true);
        String url = "";
        String json = "";

        Object result = getCallback.invoke(aws, url, json);
        assertEquals(result.getClass().getInterfaces()[0], WatcherCallback.class);
    }

    public void testNewCreateFileWatcherSetAnyFileMethod() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException, NoSuchFieldException {
        CreateFileWatcher aws = new CreateFileWatcher(new File(System.getProperty("java.io.tmpdir") + "/test/test.watch"), 1);

        assertNotNull(aws.getClass().getDeclaredMethod("setWatchAnyFile", boolean.class));
        Method setWatchAnyFile = aws.getClass().getDeclaredMethod("setWatchAnyFile", boolean.class);
        Field anyFile = aws.getClass().getDeclaredField("anyFile");
        anyFile.setAccessible(true);
        assertFalse((Boolean) anyFile.get(aws));
        setWatchAnyFile.setAccessible(true);
        setWatchAnyFile.invoke(aws, true);
        assertTrue((Boolean) anyFile.get(aws));
    }

}
