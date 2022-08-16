import junit.framework.TestCase;
import uk.ac.cam.cares.jps.aws.AsynchronousWatcherService;
import uk.ac.cam.cares.jps.aws.ResourceLoader;

import javax.ws.rs.core.Application;
import java.util.HashSet;

public class ResourceLoaderTest extends TestCase {

    public void testNewResourceLoader() {
        ResourceLoader rl = null;
        try {
            rl = new ResourceLoader();
        } finally {
            assertNotNull(rl);
            assertEquals(rl.getClass(), ResourceLoader.class);
            assertEquals(0, rl.getClass().getAnnotations().length);
            assertEquals(rl.getClass().getGenericSuperclass(), Application.class);
        }
    }

    public void testNewResourceLoaderFields() {
        ResourceLoader rl = new ResourceLoader();
        assertEquals(1, rl.getClass().getDeclaredFields().length);
        assertEquals(rl.WATCHER_SERVLET, "AsynchronousWatcherService");
    }

    public void testNewResourceLoaderMethods() {
        ResourceLoader rl = new ResourceLoader();
        assertEquals(1, rl.getClass().getDeclaredMethods().length);
        assertNotNull(rl.getClasses());
        assertEquals(rl.getClasses().getClass(), HashSet.class);
        assertEquals(rl.getClasses().size(), 1);
        assertEquals(rl.getClasses().iterator().next(), AsynchronousWatcherService.class);
    }
}
