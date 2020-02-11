

import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

 
@WebServlet("/DoSimulation")
public class DoSimulation extends HttpServlet {
	private static final long serialVersionUID = 1L;
	public static String APINCSV = new String( "C:/apache-tomcat-8.0.24/webapps/ROOT/APIN.CSV");    
	public static String APPWSim = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/APPWSim");
	public static String PrAPPWOUTCSV = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/PrAPPWOUT.CSV"); // output CSV file from the pr aspen plus model

    /**
     * @see HttpServlet#HttpServlet()
     */
    public DoSimulation() {
        super();
        // TODO Auto-generated constructor stub
    }

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// TODO Auto-generated method stub
		
		System.out.println("Request "+ request.getQueryString());
		
 
		
		String[] inputs = request.getParameterValues("Input");
		
		String[] nums = inputs[0].split(" ");
		
		for(String input : nums)
		{
		System.out.println("Array---" + input );
		}
		Double input1 = null,input2 = null,input3 = null,input4 = null,input5 = null,input6 = null;
		
		
		Double[] inputs_num = {input1,input2,input3,input4,input5,input6};
		
		
		
		if(nums.length==6)
		{
			 for(int i = 0; i < nums.length ; i++)
			 {
				 inputs_num[i] = Double.parseDouble(nums[i]); // convert string inputs to doubles
			 }
		}
		else // if the request is empty, use the default array to be the inputs
		{
			inputs_num = new Double[]{33.0,30.0,180.0,30.0,233.135,4.0};
		}
		
		ArrayList<String[]> result = doSimulation(null,inputs_num);
		for(String[] arr : result)
		{
			response.getWriter().println(arr[0] + "$" + arr[1] + "#") ;
		}
  

	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// TODO Auto-generated method stub
		doGet(request, response);
 
	}
	
	// The main function that does the simulation which requires two inputs 
	// SimulationIRI find the simulation individuals in the owl file where the inputs, model name and outputs
	// The editStack stores the user inputs
	@SuppressWarnings("resource")
	public ArrayList<String[]> doSimulation(String SimulationIRI , Double[] Inputs) throws IOException
	{
		// First use hardcoded informations instead of reading info from owl files
		// 1. Write to APIN.csv 
		/*  FOIL, TOIL, FMEOH, TMEOH, FREWATER, PBOILER
		 *	33,30.0,180.0,30.0,233.135,4.0
		 *	The output needed to be collected are 
		 *	25 - ValueOfHeatDutyOfR-301     			in R-301
		 *	112- V_Angle_LoadPoint_R-602001 			in R-301
		 *	113- V_ActualVoltage_LoadPoint_R-602001		in R-301
		 *	23 - ValueOfHeatDutyOfR-302					in R-302
		 *	102- V_Angle_LoadPoint_R-602002				in R-302
		 *	103- V_ActualVoltage_LoadPoint_R-602002		in R-302
		 */
		Double[] x = Inputs;
		ArrayList<Double> xRow = new ArrayList<Double>(Arrays.asList(x));                                   // extra arraylist to collect the x-value required as input to the pr aspen plus model
	/*
		FileWriter filewriter = new FileWriter(APINCSV);
		filewriter.append("FOIL, TOIL, FMEOH, TMEOH, FREWATER, PBOILER");
		filewriter.append("33,30.0,180.0,30.0,233.135,4.0");
 */
		List<List<Double>> xData = new ArrayList<>(1);                                    // arraylist to
 
		String simDir = APPWSim;	
		String modelName = "Polynomial_Alg_1";
		FileWriter fileWriter = null;
		
		xData.add(xRow); 
		
		try {
	
			fileWriter = new FileWriter(PrAPPWOUTCSV);                                        // filewriter for the output of pr aspenplus model
			System.load("C:/apache-tomcat-8.0.24/webapps/ROOT/MoDS_Java_API.dll");            //the MoDS API at use is version 0.1  D:\MoDS_API\MoDS_Java_API_v0.1
			
			ArrayList<String> xNames = MoDSAPI.getXVarNamesFromAPI(simDir, modelName);		
			System.out.println("xNames= " + xNames);
			ArrayList<String> yNames = MoDSAPI.getYVarNamesFromAPI(simDir, modelName);
			System.out.println("yNames= " + yNames);
			for (int j = 0; j < yNames.size(); j++) {
				fileWriter.append(yNames.get(j));                                               // write the yNames to the output CSV file
				fileWriter.append(",");
			}									
		} catch (Error e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
 		List<List<Double>> yData = MoDSAPI.evaluateSurrogate(simDir, modelName, xData);   // call MoDS API to evaluate the surrogate model basing on the MoDS simulation file "simDir -> modelNam"  and  the input xData that was collected before
 		ArrayList<String[]> result = new ArrayList<String[]>();
 		List y = yData.get(0);
 		
 		
 		String[] arr = {"ValueOfHeatDutyOfR-301",String.valueOf(y.get(25))};
 		result.add(arr);
 		String[] arr1 = {"V_Angle_LoadPoint_R-301",String.valueOf(y.get(112))};
 		result.add(arr1); 		
 		String[] arr2 = {"V_ActualVoltage_LoadPoint_R-301",String.valueOf(y.get(113))};
 		result.add(arr2);
 		String[] arr3 = {"ValueOfHeatDutyOfR-302",String.valueOf(y.get(23))};
 		result.add(arr3);
 		String[] arr4 = {"V_Angle_LoadPoint_R-302",String.valueOf(y.get(102))};
 		result.add(arr4);
 		String[] arr5 = {"V_ActualVoltage_LoadPoint_R-302",String.valueOf(y.get(103))};
 		result.add(arr5);
 		
 		return result;
	}

}
