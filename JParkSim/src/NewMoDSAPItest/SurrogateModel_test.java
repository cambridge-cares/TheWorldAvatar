package NewMoDSAPItest;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.cmclinnovations.modsapi.MoDSAPI;

public class SurrogateModel_test {

	public static void main(final String[] args) {
		List<List<Double>> xData = new ArrayList<>(1);
		List<Double> xRow;
		
		for(int k = 0; k<1; ++k){
			xRow = new ArrayList<>(Arrays.asList(4 * k + 1.0, 4 * k + 2.0, 4 * k + 1.0, 4 * k + 2.0, 4 * k + 1.0, 4 * k + 2.0));
			xData.add(xRow);
		}
		System.out.println("xData="+xData);
		
		String simDir = "C:/Users/Zhou Li/Desktop/APPWSim";
//		System.load("D:/MoDS_API/MoDS_Java_API_v0.1/MoDS_Java_API.dll");
	    // String modelName = "HDMR_Alg_1";
//	    String modelName = "Polynomial_Alg_1";
	    String modelName = "Polynomial_Alg_1";

	    ArrayList<String> xNames = MoDSAPI.getXVarNamesFromAPI(simDir, modelName);
	    System.out.println("xNames="+xNames);
	    ArrayList<String> yNames = MoDSAPI.getYVarNamesFromAPI(simDir, modelName);
	    System.out.println("yNames="+yNames);
	    List<List<Double>> yData;
				        
	    yData = MoDSAPI.evaluateSurrogate(simDir, modelName, xData);			        
	    System.out.println("yData="+yData);

	}
}