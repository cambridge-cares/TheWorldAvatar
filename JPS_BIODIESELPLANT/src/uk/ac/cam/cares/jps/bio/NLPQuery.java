package uk.ac.cam.cares.jps.bio;


import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.Console;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Servlet implementation class NLPQuery
 */
@WebServlet("/NLPQuery")

public class NLPQuery extends HttpServlet {
	private static final long serialVersionUID = 1L;
       
    /**
     * @see HttpServlet#HttpServlet()
     */
    public NLPQuery() {
        super();
        // TODO Auto-generated constructor stub
    }

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// TODO Auto-generated method stub
		
		if(request.getParameter("input")!=null)// if the request is empty, return "Empty Request"
		{
			// if request is not null, do according query basing
			String result = "";
			String input = request.getParameter("input");
			try {
				writeInput(input);
			    result = startBat();
			    
			    
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			//response.getWriter().println((request.getParameter("input")));
			response.getWriter().println(result.split("@@")[1]);
		}
		else
		{
			
			System.out.println("Here we are");
			System.out.println(request.getParameter("mode"));
			System.out.println("1" + request.getParameter("mode").getClass());

			
			if(request.getParameter("mode").matches("endpoint"))
			{
				System.out.println("Here we are");
				System.out.println(request.getParameter("mode").getClass());
				if(request.getParameter("Query")!=null)
				{
					String Query = request.getParameter("Query");
					String Result = doEndPointQuery(Query);
					response.getWriter().println(Result);
				}
				else
				{
					response.getWriter().println("Empty Query");
				}
			}
			else
			{
				
			}
			
		}
		
		
	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// TODO Auto-generated method stub
		doGet(request, response);
	}

	public static void writeInput(String input) throws UnsupportedEncodingException, IOException, Exception
	{
		try (Writer writer = new BufferedWriter(new OutputStreamWriter(
	              new FileOutputStream("C:/NLPPackage/input.txt"), "utf-8"))) {
	   writer.write(input);
	}
	}
	
	public static String startBat() throws IOException
	{
		 
		String result = "";
		String s ="";
		Process p = Runtime.getRuntime().exec("cmd /c C:/NLPPackage/convertor.bat");
		BufferedReader stdInput = new BufferedReader(new InputStreamReader(p.getInputStream()));
	    while ((s = stdInput.readLine()) != null) {result+= s;}
		return result;
	
	}
	
	
	public static String doEndPointQuery(String query) throws IOException
	{
		String result = "";
		String s ="";
	    String[] cmd = {
	    	      "python",
	    	      "C:/Demo/SearchYago.py",
	    	      query,
	    	    };
	    Process p = Runtime.getRuntime().exec(cmd);
		BufferedReader stdInput = new BufferedReader(new InputStreamReader(p.getInputStream()));
	    while ((s = stdInput.readLine()) != null) {result+= s;}
		return result;
	}
	
	public static String returnResult()
	{
		return "result";
	}
}
