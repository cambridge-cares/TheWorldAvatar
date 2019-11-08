import junit.framework.TestCase;
import uk.ac.cam.cares.jps.aws.CreateFileWatcher;
import uk.ac.cam.cares.jps.aws.Watcher;

import java.io.File;
import java.lang.reflect.Field;
import java.util.concurrent.atomic.AtomicBoolean;

public class CreateFileWatcherTest extends TestCase {

    public void testNewCreateFileWatcher() {
        CreateFileWatcher cfw = null;
        try {
            cfw = new CreateFileWatcher(new File("./tmp/test.watch"), 1);
        } finally {
            assertNotNull(cfw);
            assertEquals(cfw.getClass(), CreateFileWatcher.class);
            assertEquals(0, cfw.getClass().getAnnotations().length);
            assertEquals(cfw.getClass().getGenericSuperclass(), Thread.class);
            assertEquals(cfw.getClass().getGenericInterfaces()[0], Watcher.class);
        }
    }

    public void testNewCreateFileWatcherFields() throws NoSuchFieldException, IllegalAccessException {
        File fileIn = new File("./tmp/test.watch");
        CreateFileWatcher cfw = new CreateFileWatcher(fileIn, 1);
        Field file = null;
        Field timeout = null;
        Field stop = null;
        Field callback = null;
        assertEquals(4, cfw.getClass().getDeclaredFields().length);
        try {
            file = cfw.getClass().getDeclaredField("file");
            file.setAccessible(true);
            timeout = cfw.getClass().getDeclaredField("timeout");
            timeout.setAccessible(true);
            stop = cfw.getClass().getDeclaredField("stop");
            stop.setAccessible(true);
            callback = cfw.getClass().getDeclaredField("callback");
            callback.setAccessible(true);
        }   finally {
            assertEquals(file.get(cfw), fileIn);
            assertEquals(timeout.get(cfw), 1);
            assertEquals(false, ((AtomicBoolean) stop.get(cfw)).get());
            assertNull(callback.get(cfw));
        }
    }

    public void testNewCreateFileWatcherMethods() throws NoSuchFieldException, IllegalAccessException {
        File fileIn = new File("./tmp/test.watch");
        CreateFileWatcher cfw = new CreateFileWatcher(fileIn, 1);
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
            //@todo: [AC] test remaining methods
        }
    }


}
