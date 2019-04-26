package uk.ac.cam.cares.jps.base.config.server;

import java.io.IOException;
import java.util.Iterator;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.config.KeyValueMap;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.log.JPSBaseLogger;

@WebServlet(urlPatterns = {"/keys/get", "/keys/set"})
public class KeyValueServer extends HttpServlet {

	private static final long serialVersionUID = -7979108458102577070L;
	
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
					JPSBaseLogger.info(this, "set key-value-pair: " + key + " = " + value + " (replaced = " + replacedValue + ")");
				}
			}
		} catch (JSONException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
	}
}
