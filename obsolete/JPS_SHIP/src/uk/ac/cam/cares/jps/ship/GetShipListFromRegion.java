package uk.ac.cam.cares.jps.ship;

import java.io.IOException;
import java.util.ArrayList;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.util.CommandHelper;

/**
 * Servlet implementation class GetShipListFromRegion
 * 
 * IT IS UNUSED ANYMORE, REPLACED BY POSTGRESQL  31/5/19
 */
@WebServlet("/GetShipListFromRegion")
public class GetShipListFromRegion extends HttpServlet {
	private static final long serialVersionUID = 1L;
	private Logger logger = LoggerFactory.getLogger(GetShipListFromRegion.class);
       
    /**
     * @see HttpServlet#HttpServlet()
     */
    public GetShipListFromRegion() {
        super();
    }

	protected void doGet(HttpServletRequest req, HttpServletResponse res) throws ServletException, IOException {
		res.setContentType("application/json");
//		String[] arrayOfShipIRIs = { 
//				"http://www.theworldavatar.com/kb/ships/Ship-1.owl#Ship-1",
//		        "http://www.theworldavatar.com/kb/ships/Ship-2.owl#Ship-2",
//		        "http://www.theworldavatar.com/kb/ships/Ship-3.owl#Ship-3",
//		        "http://www.theworldavatar.com/kb/ships/Ship-4.owl#Ship-4",
//		        "http://www.theworldavatar.com/kb/ships/Ship-5.owl#Ship-5",
//		        "http://www.theworldavatar.com/kb/ships/Ship-6.owl#Ship-6",
//		        "http://www.theworldavatar.com/kb/ships/Ship-7.owl#Ship-7",
//		        "http://www.theworldavatar.com/kb/ships/Ship-8.owl#Ship-8",
//		        "http://www.theworldavatar.com/kb/ships/Ship-9.owl#Ship-9",
//		        "http://www.theworldavatar.com/kb/ships/Ship-10.owl#Ship-10"
//		};
//		
//		JSONObject result = new JSONObject();
//		try {
//			result.put("shipIRIs", arrayOfShipIRIs);
//			res.getWriter().write(result.toString());
//		} catch (JSONException e) {
//			e.printStackTrace();
//		}
		
		String shipEp = "http://172.25.182.41/damecoolquestion/ships-persistent/sparql";
		String connectType= "endpoint";
		int shipNum = 25;
		JSONObject input = null;
		try {
			input = new JSONObject(req.getParameter("query"));
		} catch (JSONException e) {
			logger.error(e.getMessage());
		}
		System.out.println(input.toString());  
		//get parameter range		
		String[] pparams = new String[7];
		
		pparams[0] = shipEp;
		pparams[1] = connectType;
		pparams[2] = ""+shipNum;
		try {
			pparams[3] =""+ input.getJSONObject("region").getJSONObject("lowercorner").get("lowerx");
			pparams[5]  = ""+ input.getJSONObject("region").getJSONObject("uppercorner").get("upperx");
			pparams[4]  = ""+ input.getJSONObject("region").getJSONObject("lowercorner").get("lowery");
			pparams[6]  = ""+ input.getJSONObject("region").getJSONObject("uppercorner").get("uppery");
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		logger.info("FINDING SHIPS");
		logger.info(pparams[0]);
		logger.info(pparams[1]);
		logger.info(pparams[2]);
		logger.info(pparams[3]);
		logger.info(pparams[4]);
		logger.info(pparams[5]);
		logger.info(pparams[6]);
		
		String targetFolder = AgentLocator.getNewPathToPythonScript("caresjpsship", this);

		ArrayList<String> args = new ArrayList<String>();
		args.add("python");
		args.add("shipRegionQuery.py"); 
		args.add(pparams[0]);
		args.add(pparams[1]);
		args.add(pparams[2]);
		args.add(pparams[3]);
		args.add(pparams[4]);
		args.add(pparams[5]);
		args.add(pparams[6]);

		String shipListStr = CommandHelper.executeCommands(targetFolder, args);
		
		System.out.println(shipListStr);
		res.getWriter().write(convertFromUTF8(shipListStr));
	}
	
//	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
//		doGet(request, response);
//	}
	
    public static String convertFromUTF8(String s) {
        String out = null;
        try {
            out = new String(s.getBytes("ISO-8859-1"), "UTF-8");
        } catch (java.io.UnsupportedEncodingException e) {
            return null;
        }
        return out;
    }

}
