/** This part of code is write to improve the query function of JParkSimulator-Zhou Li*/
package QUERYServlet;

import java.io.FileWriter;
import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import rawMaterialQuery.RawMaterialQuery;
import reactorQuery.ReactorQuery;

public class QUERYServlet extends HttpServlet {		
	/**
	 * author Li Zhou
	 */
	private static final long serialVersionUID = 1L;
	public static String httpReqCSV = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/httpReq.CSV"); 
		
	public QUERYServlet(){
		
	}
	
	/**
	 * @param args
	 */
	
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		/**reconstructing editStack from request string so that the information can be passed on to the relevant methods*/
		String[] QueryT = request.getParameter("QueryT").split(",");
		
		//natural language processing
		String[] QueryWords = null;
		String QueryTnew = null;
		System.out.println(QueryT.length);
		for(int i=0; i<QueryT.length; i++){
			QueryWords = QueryT[i].split(" ");
			System.out.println(QueryWords[0]);
			for(int j=0; j<QueryWords.length; j++){
				if(QueryWords[j].equalsIgnoreCase("rawMaterial")){
					QueryTnew = QueryWords[j];
					System.out.println(QueryTnew);
				}else if(QueryWords[j].equalsIgnoreCase("reactor")){
					QueryTnew = QueryWords[j];
					System.out.println(QueryTnew);
				}				
			}
		}
				
		/**check whether the httpRequst has been correctly received */
		FileWriter flag1 = null;                                 // testing structure of DataOutputStream object and of wr object
		flag1 = new FileWriter(httpReqCSV);
		flag1.append(", QueryT=" + QueryT[0]);
		flag1.flush();
		flag1.close(); 
 				
		/**the following part of code distinguishes which functionality of the JParkSimulator has been called, and then provides the corresponding service by evaluating the associated model */
		switch (QueryTnew) {		
		case ("rawMaterial"):					
            System.out.println("start extracting information.");
		    System.out.println(QueryT[0]);
		    
		    final String RawMaterial=RawMaterialQuery.executeRawMaterialQuery().toString();	
		    response.setContentLength(RawMaterial.length());
		    response.getOutputStream().write(RawMaterial.getBytes());		    
		    response.getOutputStream().flush();
		    response.getOutputStream().close();
		    System.out.println("Success!");
		    break;
		    
		case ("reactor"):					
            System.out.println("start extracting information.");
		    System.out.println(QueryT[0]);
		    String Reactor =  ReactorQuery.executeReactorQuery().toString();	
		    System.out.println("Result is ----> " + Reactor);
		    /*
		    String Reactor = ReactorQuery.QueryResult;
		    while(Reactor==null)
		    {
		    	try {
					Thread.sleep(500);
				} catch (InterruptedException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
		    	Reactor = ReactorQuery.QueryResult;
		    }
		    */
  		    response.setContentLength(Reactor.length());
		    response.getOutputStream().write(Reactor.getBytes());		    
		    response.getOutputStream().flush();
		    response.getOutputStream().close();
		    System.out.println("Success!");
		    break;
		    
	} // of doPost()

	
  }
}
