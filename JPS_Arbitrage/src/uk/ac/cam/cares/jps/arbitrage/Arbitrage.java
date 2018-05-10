package uk.ac.cam.cares.jps.arbitrage;


import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.cmclinnovations.mods.api.MoDSAPI;

import uk.ac.cam.cares.jps.util.PythonHelper;



public class Arbitrage {
	

	public static void Running_analysis_Aspen() throws Exception {
		
		/** this function executes 4 Python scripts which download market data and stores it in separate CSV files */ 
		
		String CPO_to_FAME_analysis = new String("caresjpsarbitrage/CPO_to_FAME.py"); 
		
		String market_data_plot = new String("C:\\Users\\Janusz\\Desktop\\JParkSimulator-git\\JPS_Arbitrage\\workingdir\\arbitrage_CPO.png"); 

		String result = PythonHelper.callPython(CPO_to_FAME_analysis, market_data_plot, new Arbitrage());
		System.out.println(result);
		   
		   
	}
	
	public static String Running_analysis_MoDS(String input) throws Exception {
		
		
		String[] sim_address = {"E:\\MoDS_Projects\\Arbitrage\\Models\\CPO_to_FAME_26042016_001\\Sims\\HDMR_50_001", "HDMR_Alg_1"};
		//Double[] inputs = {24220.0656};
		Double[] inputs = {Double.parseDouble(input)};
		List<Double> data = MoDS(inputs,sim_address);
	    
		String result = inputs[0].toString();
	    for (int i = 0; i <data.size(); i++){ 
	    	result += "," + data.get(i).toString();
	    }

		/** this function executes 4 Python scripts which download market data and stores it in separate CSV files  */
		
		String CPO_to_FAME_analysis = new String("caresjpsarbitrage/CPO_to_FAME_MoDS.py"); 
		String market_data_plot = new String("C:\\Users\\Janusz\\Desktop\\Commodity_prices\\Market_data\\arbitrage_CPO_MoDS.png"); 
		String result1 = PythonHelper.callPython(CPO_to_FAME_analysis, market_data_plot, result, new Arbitrage());
		return result1;

	}

	public static List<Double> MoDS(final Double[] args, final String[] args1) { //final String[] args
		final String simDir = args1[0];
	    final String surrogateAlgName = args1[1];
		
		//final String simDir = "E:\\MoDS_Projects\\Arbitrage\\Models\\CPO_to_FAME_26042016_001\\Sims\\HDMR_50_001";
	    //final String surrogateAlgName = "HDMR_Alg_1";
	    // Example is for a surrogate with 10 input variables
	    // Note that IndexOutOfBoundsException is thrown if you supply too many inputs, but *no exception is thrown if you supply too few!*.
	    final List<Double> inputs_1 = new ArrayList<>(Arrays.asList(args));

	    // Evaluate the surrogate for a single set of inputs.  The result is a List<Double> of size Noutputs
	    List<Double> outputs1 = MoDSAPI.evaluateSurrogate(simDir, surrogateAlgName, inputs_1);
	    return outputs1;
	}	  	   
	
	public static void main(String[] args) throws Exception {
		//Running_analysis_Aspen();
		//Running_analysis_MoDS("24220.0656");
	}
}
