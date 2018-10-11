package uk.ac.cam.cares.jps.base.config;

import java.io.IOException;
import java.util.Iterator;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

@WebServlet(urlPatterns = {"/keys/get", "/keys/set"})
public class KeyValueServer extends HttpServlet {

	private static final long serialVersionUID = -7979108458102577070L;
	Logger logger = LoggerFactory.getLogger(KeyValueServer.class);
	
	public static void set(String... keyOrvalue) {
		AgentCaller.executeGet("/JPS_BASE/keys/set", keyOrvalue);
	}
	
	public static String get(String key) {
		String result = AgentCaller.executeGet("/JPS_BASE/keys/get", "key", key);
		try {
			JSONObject jo = new JSONObject(result);
			return jo.getString(key);
		} catch (JSONException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
	}	
	
	protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
		
		String path = req.getServletPath();
		JSONObject jo = AgentCaller.readJsonParameter(req);

		try {		
			if ("/keys/get".equals(path)) {
				String key = jo.getString("key");
				String value = KeyValueMap.getInstance().get(key);
				JSONObject result = new JSONObject().put(key, value);	
				AgentCaller.writeJsonParameter(resp, result);
				logger.debug("get key-value-pair: " + key + "=" + value);
				//System.out.println("Get key-value-pair with key=" + key + ", value=" + value);
			} else if ("/keys/set".equals(path)) {
	
				Iterator<String> it = jo.keys();
				while (it.hasNext()) {
					String key = it.next();
					String value = jo.getString(key);
					String replacedValue = KeyValueMap.getInstance().put(key, value);
					logger.info("set key-value-pair: " + key + "=" + value + " (replaced=" + replacedValue + ")");
					//System.out.println("Set key-value-pair with key=" + key + ", value=" + value + ", replaced value=" + replacedValue);		
				}
			}
		} catch (JSONException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
	}
}
