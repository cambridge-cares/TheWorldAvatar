package uk.ac.cam.cares.jps.base.log.server;

import java.io.BufferedReader;
//import java.io.File;
import java.io.IOException;
//import java.io.PrintWriter;
//import java.util.Formatter;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import uk.ac.cam.cares.jps.base.log.JPSBaseLogger;

/**
 * Servlet implementation class LogServer
 */
@WebServlet("/LogServer")
public class LogServer extends HttpServlet {
	
	private static final long serialVersionUID = -1926926842257332242L;

	public LogServer() {
		JPSBaseLogger.info(this, "started");
	}

	protected void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		
		try {
			// Read from request
			StringBuilder buffer = new StringBuilder();
			BufferedReader reader = request.getReader();
			String line;
			while ((line = reader.readLine()) != null) {
				buffer.append(line);
			}
			
//			File file = new File("C:/jps/temp/file.txt");
//			file.getParentFile().mkdirs();
//			PrintWriter writer = new PrintWriter(file, "UTF-8");
//			writer.println(buffer.toString());
//			writer.close();	
			
			//response.getWriter().append("Served at: ").append(request.getContextPath()).append(" message: ").append(buffer.toString());
			JPSBaseLogger.info(this, buffer.toString());
			
		} catch (Exception e) {
			JPSBaseLogger.error(this, "failed to read the request body from the request.");
		}
	}
	
	protected void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		doGet(request, response);
	}
}
