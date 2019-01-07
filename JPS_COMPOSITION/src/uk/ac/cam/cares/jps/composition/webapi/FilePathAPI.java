package uk.ac.cam.cares.jps.composition.WebAPI;

import java.io.File;
import java.io.IOException;

import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Servlet implementation class FilePathAPI
 */
@WebServlet("/FilePathAPI")
public class FilePathAPI extends HttpServlet {
	private static final long serialVersionUID = 1L;

	public FilePathAPI() {
		super();
	}

	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		response.getWriter().write(getFilePath(request));
	}

	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		doGet(request, response);
	}

	public String getFilePath(HttpServletRequest request) {
		ServletContext servletContext = getServletContext();
		String contextPath = servletContext.getRealPath(File.separator);
		return contextPath;
	}

}
