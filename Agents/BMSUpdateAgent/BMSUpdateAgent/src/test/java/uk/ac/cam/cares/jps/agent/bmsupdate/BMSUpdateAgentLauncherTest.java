package uk.ac.cam.cares.jps.agent.bmsupdate;

import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import java.nio.file.Paths;
import java.util.List;
import java.lang.reflect.Method;

import static uk.ac.cam.cares.jps.agent.bmsupdate.helper.PropertiesFileHelper.readSetClientPropertiesTemplateFile;
import static uk.ac.cam.cares.jps.agent.bmsupdate.helper.PropertiesFileHelper.readSparqlClientPropertiesTemplateFile;
import static uk.ac.cam.cares.jps.agent.bmsupdate.helper.PropertiesFileHelper.readWriteClientPropertiesTemplateFile;
import static uk.ac.cam.cares.jps.agent.bmsupdate.helper.PropertiesFileHelper.writeToTempFolder;

public class BMSUpdateAgentLauncherTest {
    @ClassRule
    public static TemporaryFolder folder = new TemporaryFolder();

    static BMSUpdateAgentLauncher launcher;

    @BeforeClass
    public static void setUpBeforeClass() {
        launcher = new BMSUpdateAgentLauncher();
    }

    @Test
    public void testInitSetProperties() throws Exception {
        List<String> lines = readSetClientPropertiesTemplateFile(null, "esp_update_host", "blazegraph_query_host", "blazegraph_update_host");
        writeToTempFolder(Paths.get(folder.getRoot().toString(), "client.properties").toString(), lines);

         // Set private method to be accessible
        Method initSetProperties = BMSUpdateAgentLauncher.class.getDeclaredMethod("initSetProperties", String.class);
        initSetProperties.setAccessible(true);

        //test missing esphome.agent.toggle
        try {
            initSetProperties.invoke(launcher, Paths.get(folder.getRoot().toString(), "client.properties").toString());
            Assert.fail();
        } catch (Exception e) {
            Assert.assertEquals("Properties file is missing \"esphome.agent.toggle=<esphome_agent_toggle>\"", e.getCause().getMessage());
        }

        lines = readSetClientPropertiesTemplateFile("esp_home_host", null, "blazegraph_query_host", "blazegraph_update_host");
        writeToTempFolder(Paths.get(folder.getRoot().toString(), "client.properties").toString(), lines);
        
        //test missing esphome.update.agent.retrieve
        try {
            initSetProperties.invoke(launcher, Paths.get(folder.getRoot().toString(), "client.properties").toString());
            Assert.fail();
        } catch (Exception e) {
            Assert.assertEquals("Properties file is missing \"esphome.update.agent.retrieve=<esphome_update_agent_retrieve>\"", e.getCause().getMessage());
        }

        lines = readSetClientPropertiesTemplateFile("esp_home_host", "esp_update_host", null, "blazegraph_update_host");
        writeToTempFolder(Paths.get(folder.getRoot().toString(), "client.properties").toString(), lines);
        
        //test missing sparql.query.endpoint
        try {
            initSetProperties.invoke(launcher, Paths.get(folder.getRoot().toString(), "client.properties").toString());
            Assert.fail();
        } catch (Exception e) {
            Assert.assertEquals("Properties file is missing \"sparql.query.endpoint=<sparql_endpoint>\"", e.getCause().getMessage());
        }

        lines = readSetClientPropertiesTemplateFile("esp_home_host", "esp_update_host", "blazegraph_query_host", null);
        writeToTempFolder(Paths.get(folder.getRoot().toString(), "client.properties").toString(), lines);
        
        //test missing sparql.update.endpoint
        try {
            initSetProperties.invoke(launcher, Paths.get(folder.getRoot().toString(), "client.properties").toString());
            Assert.fail();
        } catch (Exception e) {
            Assert.assertEquals("Properties file is missing \"sparql.update.endpoint=<sparql_endpoint>\"", e.getCause().getMessage());
        }
    }

    @Test
    public void testinitUpdateTriplesProperties() throws Exception {
        List<String> lines = readWriteClientPropertiesTemplateFile(null, "sparql_update_host", "sparql_user", "sparql_password", "db_url", "db_user", "db_password");
        writeToTempFolder(Paths.get(folder.getRoot().toString(), "client.properties").toString(), lines);

         // Set private method to be accessible
        Method initUpdateTriplesProperties = BMSUpdateAgentLauncher.class.getDeclaredMethod("initUpdateTriplesProperties", String.class);
        initUpdateTriplesProperties.setAccessible(true);

        //test missing sparql.query.endpoint
        try {
            initUpdateTriplesProperties.invoke(launcher, Paths.get(folder.getRoot().toString(), "client.properties").toString());
            Assert.fail();
        } catch (Exception e) {
            Assert.assertEquals("Properties file is missing \"sparql.query.endpoint=<sparql_endpoint>\"", e.getCause().getMessage());
        }

        lines = readWriteClientPropertiesTemplateFile("sparql_query_host", null, "sparql_user", "sparql_password", "db_url", "db_user", "db_password");
        writeToTempFolder(Paths.get(folder.getRoot().toString(), "client.properties").toString(), lines);
        
        //test missing sparql.update.endpoint
        try {
            initUpdateTriplesProperties.invoke(launcher, Paths.get(folder.getRoot().toString(), "client.properties").toString());
            Assert.fail();
        } catch (Exception e) {
            Assert.assertEquals("Properties file is missing \"sparql.update.endpoint=<sparql_endpoint>\"", e.getCause().getMessage());
        }

        lines = readWriteClientPropertiesTemplateFile("sparql_query_host", "sparql_update_host", "sparql_user", "sparql_password", null, "db_user", "db_password");
        writeToTempFolder(Paths.get(folder.getRoot().toString(), "client.properties").toString(), lines);
        
        //test missing db.url
        try {
            initUpdateTriplesProperties.invoke(launcher, Paths.get(folder.getRoot().toString(), "client.properties").toString());
            Assert.fail();
        } catch (Exception e) {
            Assert.assertEquals("Properties file is missing \"db.url=<db_url>\"", e.getCause().getMessage());
        }

        lines = readWriteClientPropertiesTemplateFile("sparql_query_host", "sparql_update_host", "sparql_user", "sparql_password", "db_url", null, "db_password");
        writeToTempFolder(Paths.get(folder.getRoot().toString(), "client.properties").toString(), lines);
        
        //test missing db.user
        try {
            initUpdateTriplesProperties.invoke(launcher, Paths.get(folder.getRoot().toString(), "client.properties").toString());
            Assert.fail();
        } catch (Exception e) {
            Assert.assertEquals("Properties file is missing \"db.user=<db_user>\"", e.getCause().getMessage());
        }

        lines = readWriteClientPropertiesTemplateFile("sparql_query_host", "sparql_update_host", "sparql_user", "sparql_password", "db_url", "db_user", null);
        writeToTempFolder(Paths.get(folder.getRoot().toString(), "client.properties").toString(), lines);
        
        //test missing db.password
        try {
            initUpdateTriplesProperties.invoke(launcher, Paths.get(folder.getRoot().toString(), "client.properties").toString());
            Assert.fail();
        } catch (Exception e) {
            Assert.assertEquals("Properties file is missing \"db.password=<db_password>\"", e.getCause().getMessage());
        }
    }

    @Test
    public void testinitSparqlProperties() throws Exception {
        List<String> lines = readSparqlClientPropertiesTemplateFile(null, "sparql_update_host", "sparql_user", "sparql_password");
        writeToTempFolder(Paths.get(folder.getRoot().toString(), "client.properties").toString(), lines);

         // Set private method to be accessible
        Method initSparqlProperties = BMSUpdateAgentLauncher.class.getDeclaredMethod("initSparqlProperties", String.class);
        initSparqlProperties.setAccessible(true);

        //test missing sparql.query.endpoint
        try {
            initSparqlProperties.invoke(launcher, Paths.get(folder.getRoot().toString(), "client.properties").toString());
            Assert.fail();
        } catch (Exception e) {
            Assert.assertEquals("Properties file is missing \"sparql.query.endpoint=<sparql_endpoint>\"", e.getCause().getMessage());
        }

        lines = readSparqlClientPropertiesTemplateFile("sparql_query_host", null, "sparql_user", "sparql_password");
        writeToTempFolder(Paths.get(folder.getRoot().toString(), "client.properties").toString(), lines);
        
        //test missing sparql.update.endpoint key
        try {
            initSparqlProperties.invoke(launcher, Paths.get(folder.getRoot().toString(), "client.properties").toString());
            Assert.fail();
        } catch (Exception e) {
            Assert.assertEquals("Properties file is missing \"sparql.update.endpoint=<sparql_endpoint>\"", e.getCause().getMessage());
        }
    }
}
