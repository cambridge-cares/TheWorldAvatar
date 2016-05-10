/*ZL-20160305: this servlet code is wrote for the OPAL-RT service of the JParkSimulator
**set up of the WEB-INF folder of this project (OPARTServlet\WebContent): 1) a web.xml file; 2) include the necessary libraries in the lib folder.
*/

package OPALRTServlet;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public class OPALRTServlet extends HttpServlet {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	public OPALRTServlet() {
		
		
	}
	
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {    //doPost method to handle the httpRequest and httpResponse

		runPyScript();
		System.out.println("message received!");

	}

	//method to call the python script in order to evaluate the corresponding model
	public void runPyScript() {
		long start = System.currentTimeMillis();
		try{
		Runtime rt = Runtime.getRuntime();
		//rt.exec("cmd /c start C:/apache-tomcat-8.0.24/webapps/ROOT/download_from_NUS/download_from_NUS.bat");
//		System.out.println("Inputs downloaded.");
		Process ps = rt.exec("cmd /c C:/apache-tomcat-8.0.24/webapps/ROOT/OPAL_files/OPAL_Python.bat");
		System.out.println("OPAL execution commenced.");
		ps.waitFor();
		System.out.println("OPAL executed.");
		if(ps.exitValue()==0){
			System.out.println("Success!");
		}else {
			System.out.println("failed!");
		}
		ps.destroy();
		//rt.exec("cmd /c start C:/apache-tomcat-8.0.24/webapps/ROOT/transmit_files_to_NUS/send_from_NTU.bat");
//		System.out.println("Outputs uploaded.");
		System.out.println("Full Servlet execution has taken" + String.valueOf(System.currentTimeMillis() - start) + "ms");
		
		}catch (Exception e) {
			e.printStackTrace();
		}
	}
	
}
