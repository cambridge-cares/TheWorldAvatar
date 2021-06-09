package org.linkeddatafragments.servlet;

import java.io.IOException;
import java.nio.charset.StandardCharsets;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.http.HttpHeaders;
import org.linkeddatafragments.exceptions.NoRegisteredMimeTypesException;
import org.linkeddatafragments.util.MIMEParse;

/**
 * Servlet implementation class BufferServlet
 */
public class BufferServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;
       
    /**
     * @see HttpServlet#HttpServlet()
     */
    public BufferServlet() {
        super();
    }

	/**
	 * @see Servlet#init(ServletConfig)
	 */
	public void init(ServletConfig config) throws ServletException {
	}

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		
		String queryString = request.getQueryString();
		
		System.out.println("queryString received: ");
		System.out.println(queryString);
		
        String acceptHeader = request.getHeader(HttpHeaders.ACCEPT);
        String bestMatch;
	 
		try {
			bestMatch = MIMEParse.bestMatch(acceptHeader);
	        response.setHeader(HttpHeaders.SERVER, "Linked Data Fragments Server");
	        response.setContentType(bestMatch);
	        response.setCharacterEncoding(StandardCharsets.UTF_8.name());
	        
	        
	        
	        
		} catch (NoRegisteredMimeTypesException e) {
			e.printStackTrace();
		}
        // set additional response headers

        
        
		
		
		
		// It receives the request sent from the nodejs SPARQL agent, forward it to the ACTUAL LDF server 
		// It receive extra parameters (reactants/ products)
		
		
		
		
	}

}
