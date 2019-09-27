package uk.ac.cam.cares.jps.adms;



import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;

/**
 * Servlet implementation class AplShips
 *
 * To generate ships.json from .apl file quickly, use:
 * sed -n '/SrcX/,/SrcY/p' test.apl | sed 'N;s/\r\n/,/' |
 * sed 's/SrcX1 = /\{\"lon"\:/g' |
 * sed 's/\,SrcY1 = /\,"lat"\:/g' |
 * sed ':a;N;$!ba;s/\r\n/\}\,\n/g' >ships.json
 */
@WebServlet("/AplShips")
public class AplShips extends HttpServlet {
	private static final long serialVersionUID = 1L;

	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		request.setCharacterEncoding("UTF-8");
		
		JSONObject joforEN = AgentCaller.readJsonParameter(request);
		String folder = joforEN.getString("folder");
		String filePath = folder + "\\ships.json";

		String result = new String(Files.readAllBytes(Paths.get(filePath)), StandardCharsets.UTF_8);

		response.setContentType("application/json");
		response.getWriter().write(result);
	}
}
