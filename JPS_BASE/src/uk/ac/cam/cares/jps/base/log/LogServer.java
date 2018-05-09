package uk.ac.cam.cares.jps.base.log;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Formatter;

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

	/**
	 * @see HttpServlet#HttpServlet()
	 */
	public LogServer() {
		super();
	}

	private Logger logger = LoggerFactory.getLogger(LogServer.class);

	protected void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		response.getWriter().append("Served at: ").append(request.getContextPath());

		try {
			// Read from request
			StringBuilder buffer = new StringBuilder();
			BufferedReader reader = request.getReader();
			String line;
			while ((line = reader.readLine()) != null) {
				buffer.append(line);
			}
			
			File file = new File("C:/jps/temp/file.txt");
			file.getParentFile().mkdirs();

			PrintWriter writer = new PrintWriter(file, "UTF-8");
			writer.println(buffer.toString());
			writer.close();
			
			System.out.println("MY LOGSERVER = " + buffer.toString());
			
//			System.out.println(buffer.toString());
			// System.out.println("MESSAGE FROM LOGSERVER:");
			// System.out.println(buffer.toString());
			// logger.debug(buffer.toString());
			logger.info(buffer.toString());
			// logger.warn(buffer.toString());
			// logger.error(buffer.toString());
		} catch (Exception e) {
			logger.error("Failed to read the request body from the request.");
		}
	}

}
