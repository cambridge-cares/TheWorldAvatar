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

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.log.LogServer;

@WebServlet(urlPatterns = {"/keys/get", "/keys/set"})
public class KeyValueServer extends HttpServlet {

	private static final long serialVersionUID = -7979108458102577070L;
	
	public static void set(String... keyOrValue) {
		
		for (int i=0; i<keyOrValue.length; i=i+2) {
			KeyValueMap.getInstance().put(keyOrValue[i], keyOrValue[i+1]);
		}
		
		AgentCaller.executeGet("/JPS_BASE/keys/set", keyOrValue);
	}
	
	public static String get(String key) {
		//TODO-AE URGENT URGENT every call to host, port etc. results into a HTTP Get --> caching solution, or redis, ...
		
		if (IKeys.HOST.equals(key) || IKeys.PORT.equals(key)) {
			return KeyValueMap.getInstance().get(key);
		}
		
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
			} else if ("/keys/set".equals(path)) {
				Iterator<String> it = jo.keys();
				while (it.hasNext()) {
					String key = it.next();
					String value = jo.getString(key);
					String replacedValue = KeyValueMap.getInstance().put(key, value);
					LogServer.info(this, "set key-value-pair: " + key + " = " + value + " (replaced = " + replacedValue + ")");
				}
			}
		} catch (JSONException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
	}
}
