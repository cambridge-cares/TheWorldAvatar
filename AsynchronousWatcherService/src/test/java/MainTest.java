import java.io.IOException;

import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.HttpClientBuilder;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.aws.Main;

public class MainTest extends TestCase {

    public void testNewMain() {
        Main m = null;
        try {
            m = new Main();
        } finally {
            assertNotNull(m);
            assertEquals(m.getClass(), Main.class);
            assertEquals(0, m.getClass().getAnnotations().length);
        }
    }

    public void testNewMainFields() {
        Main m = new Main();
        assertEquals(1, m.getClass().getDeclaredFields().length);
        assertEquals(m.PORT, 8084);
    }

    public void testNewMainMethods() throws IOException {
        Main m = new Main();
        String[] args = null;
        String url = "http://localhost:8084/watcher";
        String json400 = "{}";
        String json200 = "{\"watch\":\"../tmp.tst\", \"callback\":\"" + url + "\"}";
        assertEquals(2, m.getClass().getDeclaredMethods().length);
        new Thread(new Runnable() {
            public void run() {
                try {
                    m.main(args);
                } catch (Exception e) {
                    assertTrue(false);
                }
            }
        }).start();
        try {
            Thread.sleep(1000);
        } catch (InterruptedException e) {
            assertTrue(false);
        }
        
        HttpClient httpClient = HttpClientBuilder.create().build();
        HttpPost request = new HttpPost(url);
        StringEntity entity = new StringEntity(json400, ContentType.APPLICATION_JSON);
        request.setEntity(entity);
        HttpResponse resp = httpClient.execute(request);
        assertEquals(400, resp.getStatusLine().getStatusCode());
        entity = new StringEntity(json200, ContentType.APPLICATION_JSON);
        request.setEntity(entity);
        resp = httpClient.execute(request);
        assertEquals(200, resp.getStatusLine().getStatusCode());
    }

}
