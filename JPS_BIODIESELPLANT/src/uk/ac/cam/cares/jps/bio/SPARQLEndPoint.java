package uk.ac.cam.cares.jps.bio;


import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;


/**
 * Servlet implementation class SPARQLEndPoint
 */
@WebServlet("/SPARQLEndPoint")
public class SPARQLEndPoint extends HttpServlet {
	private static final long serialVersionUID = 1L;
       
    /**
     * @see HttpServlet#HttpServlet()
     */
    public SPARQLEndPoint() {
        super();
        // TODO Auto-generated constructor stub
    }

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// TODO Auto-generated method stub
		
		
 
 
		String uri = request.getParameter("uri");
		String update = request.getParameter("update");
		
		/*
		JSONArray uriArray = null;
		try {
			uriArray = new JSONArray(request.getParameter("uri"));
		} catch (JSONException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		JSONArray updateArray = null;
		try {
			updateArray = new JSONArray(request.getParameter("update"));
		} catch (JSONException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		for(int i = 0; i < uriArray.length(); i++)
		{
			try {
				System.out.println("uriArray ------" + uriArray.getString(i));
			} catch (JSONException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			try {
				System.out.println("updateArray ------" + updateArray.getString(i));
			} catch (JSONException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		*/
		System.out.println("=====================================================================");
		
		String mode = request.getParameter("mode");

		switch(mode)
		{
		case "update":

			System.out.println("Check Point 1");
			doUpdate(uri,update.replaceAll("\"", "^85564251"));
			/*
			ArrayList<String> uriList = new ArrayList<String>();
			ArrayList<String> updateList = new ArrayList<String>(); 
		
			
			System.out.println("Check Point 0 ");

			for(int i = 0; i < uriArray.length();i++)
			{
				System.out.println("Check Point 1 at " + i);
				System.out.println(uriArray.length());
				System.out.println(uriArray);

 				try {
 					System.out.println("Check Point 0.1 ");
 					System.out.println(uriArray.getString(i).getClass());
					uriList.add(uriArray.getString(i));
				} catch (JSONException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				try {
 					System.out.println("Check Point 0.2 ");
					updateList.add(updateArray.getString(i).replaceAll("\"","^$"));
 					System.out.println("Check Point 0.3 ");

					
					
					
				} catch (JSONException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
			 
			System.out.println("Check Point 2 ");

			doUpdate(uriList,updateList);
			
			System.out.println("Check Point 3 ");
			*/
			break;
			
		case "query":
			String query = request.getParameter("query");
			System.out.println("update:	" + query.replaceAll("\"","^$"));
			query = query.replaceAll("\"","^$");
		//	doQuery(uri,query);
			break;
		}
		
		
		
	//	System.out.println("uri:	" + uri);
	      response.setContentType("text/html");
	        response.setHeader("Cache-control", "no-cache, no-store");
	        response.setHeader("Pragma", "no-cache");
	        response.setHeader("Expires", "-1");
	        response.setHeader("Access-Control-Allow-Origin", "*");
        response.setStatus(HttpServletResponse.SC_OK);
	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// TODO Auto-generated method stub
		doGet(request, response);
	}

	public void doUpdate(String uri, String string)
	{
		String s = "";
		String result = "";
		try {
				System.out.println("Check Point 2");

				String[] cmd = {
				      "python",
				      "C:/TOMCAT/webapps/ROOT/SPARQLUpdate.py",
				      uri,
				      string
				    };
	            Process p = Runtime.getRuntime().exec(cmd);// execute the python script to do the query
				System.out.println("Check Point 3");

	            BufferedReader stdInput = new BufferedReader(new 
	                 InputStreamReader(p.getInputStream()));
	            System.out.println("Here is the standard output of the command:\n");
	            while ((s = stdInput.readLine()) != null) {
	                result+= s + "\n";
	            }
	            
	            System.out.println("result" + result);
	 
	        }
	        catch (IOException e) {
	            System.out.println("exception happened - here's what I know: ");
	            e.printStackTrace();
	            System.exit(-1);
	        }
	}
	
	
	public static String doQuery(String filename, String query)
	{
		String s = "";
		String result = "";
		try {
				String[] cmd = {
				      "python",
				      "C:/TOMCAT/webapps/ROOT/SPARQLQuery.py",
				      filename,
				      query
				    };
	            Process p = Runtime.getRuntime().exec(cmd);// execute the python script to do the query
	            
	            BufferedReader stdInput = new BufferedReader(new 
	                 InputStreamReader(p.getInputStream()));
	            System.out.println("Here is the standard output of the command:\n");
	            while ((s = stdInput.readLine()) != null) {
	                result+= s + "\n";
	            }
	            
	            System.out.println("result" + result);
	            return result;
	        }
	        catch (IOException e) {
	            System.out.println("exception happened - here's what I know: ");
	            e.printStackTrace();
	            System.exit(-1);
	        }
		return result;
	}
	
}
