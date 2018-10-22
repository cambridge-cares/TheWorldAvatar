package uk.ac.cam.cares.jps.base.log;

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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Servlet implementation class LogServer
 */
@WebServlet("/LogServer")
public class LogServer extends HttpServlet {
	private static final long serialVersionUID = 1L;
	private static Logger logger = LoggerFactory.getLogger(LogServer.class);

	public LogServer() {
		info(this, "started");
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
			logger.info(buffer.toString());
			
		} catch (Exception e) {
			error(this, "failed to read the request body from the request.");
		}
	}
	
	protected void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		doGet(request, response);
	}
	
	public static void info(Object sender, String message) {
		logger.info(sender.getClass().getSimpleName() + " " + message);
	}
	
	public static void error(Object sender, String message) {
		logger.error(sender.getClass().getSimpleName() + " " + message);
	}
	
	public static void error(Object sender, Exception exc) {
		logger.error(sender.getClass().getSimpleName() + " " + exc.getMessage() + "\n" + exc.getStackTrace());
	}
}
