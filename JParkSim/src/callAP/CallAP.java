package callAP;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;

import MouseDrag.MouseDrag;

//testing script for the "runPyScript" command  ZL-151202
public class CallAP {

	public static void main(String args[]){
//		ArrayList<String[]> skeleton = new ArrayList<String[]>(); 
		callOPART ();
		System.out.println("success!");
//		initiate(); //initiate editStack ZL-151202
//		skeleton.add(new String[] {"OIL,MEOH","FOIL,FMEOH"});  //manually add elements to skeleton ZL-151202
//		writeAPCSV(skeleton,APINCSV); //write the APIN.CSV i.e. the input data for Aspen Plus ZL-151202
//		runPyScript(editStack); //call python script to run Aspen Plus model "BiodiesePlant" ZL-151202
		
	}
	
	public static void callOPART () {
		URL url;
		
//		String[] av = {"C:\\Users\\janusz\\Desktop\\Eddy FOO\\CurrentA.png"};
		
//		MouseDrag.Printing(av);
/*		
		UserCredentials user = new UserCredentials();
		user.setUserAccount("kleinelanghorstmj", "h3OBhT0gR4u2k22XZjQltp");
*/		
		try{
			System.out.println("1_o");
//			url = new URL("http://14.100.26.181/OPALRTServlet/");
//			url = new URL("http://172.25.182.41/OPALRTServlet/");
			url = new URL("http://caresremote1.dyndns.org:1700/OPALRTServlet/"); 
//			url = new URL("http://jparksimulator.com:80/OPALRTServlet/"); 

			
			StringBuilder outputString = new StringBuilder();
			outputString.append("ping");	
			
			try {
			    URL myURL = url;
			    URLConnection myURLConnection = myURL.openConnection();
			    System.out.println("sucessful_connection");
			    myURLConnection.setDoOutput(true);
			    OutputStreamWriter out = new OutputStreamWriter(myURLConnection.getOutputStream());
			    out.write(outputString.toString());
				out.close();
//				Runtime rt = Runtime.getRuntime();
//				rt.exec("cmd /c start C:/Users/janusz/Downloads/whatever.bat");
			    System.out.println("sucessful_outputwriting");
			    BufferedReader in = new BufferedReader(new InputStreamReader(myURLConnection.getInputStream()));
				String decodedString;
				while ((decodedString = in.readLine()) != null) {
					System.out.println(decodedString);
				}
				in.close();

			} 
			catch (MalformedURLException e) { 
				e.printStackTrace();
			} 
			catch (IOException e) {   
				e.printStackTrace();
			}
			
			
		}catch (IOException equery){
			equery.printStackTrace();
		}
	}
	


}