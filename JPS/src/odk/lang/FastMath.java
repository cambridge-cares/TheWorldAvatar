package odk.lang;

/**
 * JPS uses the libraries core and core-math (see https://github.com/openimaj/openimaj/tree/master/core/core-math) 
 * to support the calculation of length and width of building as ADMS input.
 * This libraries make use of the class FastMath and its methods sqrt and atan2. This class here provides the required
 * functionality. Without this class the libraries would throw class not found errors at runtime.
 * 
 * @author Andreas
 *
 */
public class FastMath {

	 public static double sqrt(double value) {
		 return Math.sqrt(value);
	 }
	
	 
	 public static double atan2(double y, double x) {
		 return Math.atan2(y, x);
	 }
}
