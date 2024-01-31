package main.java.uk.ac.cam.cares.jps.agent.gfaagent;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.sql.SQLException;
import java.util.Properties;

import javax.servlet.annotation.WebServlet;

import org.json.JSONObject;

@WebServlet(urlPatterns = {"/gfa", "/floors"})
public class GFAAgent extends JPSAgent{
    private static final String PROPERTIES_PATH = "/resources/config.properties";
    
    private EndpointConfig endpointConfig = new EndpointConfig();

    private String dbName;
    private String dbUrl;
    private String dbUser;
    private String dbPassword;
    public String floorsCsv;

    public void init() {
        readConfig();
        this.dbUrl = endpointConfig.getDbUrl(dbName);
        this.dbUser = endpointConfig.getDbUser();
        this.dbPassword = endpointConfig.getDbPassword();
    }

    public void readConfig() {
        try (InputStream input = FileReader.getStream(PROPERTIES_PATH)) {
            Properties prop = new Properties();
            prop.load(input);
            this.dbName = prop.getProperty("db.name");
            this.floorsCsv = prop.getProperty("floors.csv");
        } catch (FileNotFoundException e) {
            e.printStackTrace();
            throw new JPSRuntimeException("config.properties file not found");
        } catch (IOException e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
    }

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        try {
            if(requestParams.equals("gfa")){            
                //calculate GFA
            }else if(requestParams.equals("floors")){
               //integrate floors data
               
            }


        } catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }

        return requestParams;
    }
}
