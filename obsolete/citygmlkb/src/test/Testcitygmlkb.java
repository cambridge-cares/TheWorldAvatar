package test;

import java.util.ArrayList;
import java.util.Arrays;

import citygmlkb.Citygmlkb;
import junit.framework.TestCase;

//building 9b9071f4
public class Testcitygmlkb extends TestCase {

	double[] result;
	double[] result2;

	ArrayList<String> xvalueground = new ArrayList<String>(
			Arrays.asList("79443.208", "79439.237", "79434.017", "79438.025", "79443.208"));
	ArrayList<String> yvalueground = new ArrayList<String>(
			Arrays.asList("454717.304", "454714.732", "454722.691", "454725.302", "454717.304"));

	ArrayList<String> x9D576B = new ArrayList<String>(
			Arrays.asList("79782.322","79782.35","79781.395","79782.092","79783.108","79783.217","79785.657","79785.519","79787.722","79796.366","79799.436","79804.083","79808.745","79813.498","79820.484","79825.205","79830.045	","79829.045","79828.044","79827.815","79825.474","79820.863","79818.552","79816.19","79811.505","79809.153","79804.497","79802.115","79797.457","79795.134","79790.455","79788.092","79783.401","79781.075","79776.401","79774.052","79757.349","79755.204","79753.296","79751.188","79749.297","79747.129","79745.715","79745.399","79744.169","79743.17","79742.783","79741.813","79741.636","79740.801","79740.627","79740.584","79740.311","79740.279","79740.248","79740.564","79741.259","79741.389","79742.288","79742.987","79743.085","79744.507","79746.309","79747.898","79749.648","79751.282","79753.072","79761.137","79763.1","79772.969","79774.852","79778.708","79780.663","79784.585","79786.548","79790.428","79792.351","79796.287","79798.23","79801.243","79806.938","79806.338","79807.643","79805.445","79808.254","79810.452","79811.109","79811.766","79810.837","79811.237","79809.986","79809.571","79807.708","79805.78","79803.775","79797.934","79793.447","79786.206","79782.322"));
	
	ArrayList<String> y9D576B = new ArrayList<String>(
			Arrays.asList("454918.041","454917.887","454917.724","454914.27","454914.448","454914.946","454914.41","454913.69","454913.192","454911.237","454910.543","454909.492","454908.438","454907.363","454905.784","454904.716","454903.622","454899.205","454894.787","454894.839","454884.502","454885.547","454886.07","454886.605","454887.666","454888.199","454889.254","454889.793","454890.848","454891.375","454892.434","454892.97","454894.032","454894.559","454895.618","454896.15","454899.934","454900.419","454900.852","454901.329","454901.758","454902.249","454902.569","454902.656","454902.991","454903.563","454903.784","454904.724","454904.896","454906.257","454906.819","454906.956","454908.575","454908.764","454909.378","454910.941","454912.381","454912.535","454913.603","454914.163","454914.233","454915.246","454916.531","454917.663","454918.91","454920.075","454921.35","454927.098","454928.497","454935.53","454936.872","454939.62","454941.013","454943.808","454945.207","454947.973","454949.343","454952.148","454953.533","454955.68","454947.122","454946.696","454944.858","454943.297","454939.342","454940.903","454939.978","454939.053","454938.393","454937.831","454936.942","454937.487","454936.158","454934.782","454933.351","454929.182","454925.98","454920.812","454918.041"));
	
	public void testcentroid() {

		/*result = Citygmlkb.centroid( xvalueground,  yvalueground);
		assertEquals(79438.61482286453, result[0], 1.); // x center
		assertEquals(454719.999956131, result[1], 1.); // y center
		assertEquals(45.3, Math.abs(result[2]), 1.); // area*/
		
	/*	result = Citygmlkb.centroid(x9D576B, y9D576B);
		assertEquals(79785.63975, result[0], 1.); // x center
		assertEquals(454914.2552281, result[1], 1.); // y center
		assertEquals(2663.468, Math.abs(result[2]), 1.); // area
*/
		ArrayList<String> xolist = new ArrayList<String>(Arrays.asList("2","2","4","5","4","2"));
		ArrayList<String> yolist = new ArrayList<String>(Arrays.asList("2","3","3","2","2","2"));
		result = Citygmlkb.centroid(xolist, yolist);
		
		assertEquals(3.5, result[0], 1.); // x center
		assertEquals(2.5, result[1], 1.); // y center
		assertEquals(2.5, Math.abs(result[2]), 1.); // area

	}

	public void testperimax() {
		
		ArrayList<String> xolist = new ArrayList<String>(Arrays.asList("2","2","4","5","4","2"));
		ArrayList<String> yolist = new ArrayList<String>(Arrays.asList("2","3","3","2","2","2"));
		result2 = Citygmlkb.perimax(xolist, yolist);
		assertEquals(7.41, result2[0], 1.); // perimeter
		assertEquals(3, result2[1], 1.); // length
		assertEquals(5, result2[2], 1.); // x0
		assertEquals(2, result2[3], 1.); // x1
		assertEquals(2, result2[4], 1.); // y0
		assertEquals(2, result2[5], 1.); // y1
	
		/*result2 = Citygmlkb.perimax(x9D576B, y9D576B);
		assertEquals(306.847479, result2[0], 1.); // perimeter
		assertEquals(17.126262, result2[1], 1.); // length
		assertEquals(79774.052, result2[2], 1.); // x0
		assertEquals(79757.349, result2[3], 1.); // x1
		assertEquals(454896.15, result2[4], 1.); // y0
		assertEquals(454899.15, result2[5], 1.); // y1
	*/
	}
	
	
	public void testsimplified() {
		
		ArrayList<String> xolist = new ArrayList<String>(Arrays.asList("2","4","5","4","3","2","2"));
		ArrayList<String> yolist = new ArrayList<String>(Arrays.asList("2","2","2","3","3","3","2"));
		result2 = Citygmlkb.simplified(xolist, yolist);
		assertEquals(7.41, result2[0], 1.); // perimeter
		assertEquals(3, result2[1], 1.); // length
		assertEquals(2, result2[2], 1.); // x0
		assertEquals(5, result2[3], 1.); // x1
		assertEquals(2, result2[4], 1.); // y0
		assertEquals(2, result2[5], 1.); // y1
		/*
		result2 = Citygmlkb.simplified(x9D576B, y9D576B);
		assertEquals(306.847479, result2[0], 1.); // perimeter
		assertEquals(17.126262, result2[1], 1.); // length
		assertEquals(79774.052, result2[2], 1.); // x0
		assertEquals(79757.349, result2[3], 1.); // x1
		assertEquals(454896.15, result2[4], 1.); // y0
		assertEquals(454899.15, result2[5], 1.); // y1
	*/
	}

	public void testcircular() {
		double area = Citygmlkb.centroid(xvalueground, yvalueground)[2];
		double perimeter = Citygmlkb.perimax(xvalueground, yvalueground)[0];

		double resultcircular = Citygmlkb.circulartest(area, perimeter);
		assertTrue(resultcircular < 0.9); // rectengular if less than 0.9
	}

	public void testangle() {
		result2 = Citygmlkb.perimax(xvalueground, yvalueground);
		double resultangle = Citygmlkb.angle(result2[1], result2[2], result2[3], result2[4], result2[5]);
		assertEquals(147.0, resultangle, 1.); // angle
	}

	public void testpostlistreaderground() {

		int totalpointinblock = 0;

		// String[] lines0 =
		// {"68357.394","352554.320","266.363","68357.225","352552.909","266.369",
		// "68355.582","352539.891","266.364","68359.902","352534.335","264.246",
		// "68359.994","352534.323","264.206","68360.716","352541.231","264.267",
		// "68358.869","352541.472","265.060","68360.214","352552.382","265.069",
		// "68359.284","352552.489","265.468","68359.280","352553.929","265.546",
		// "68357.394","352554.320","266.363"};
		String[] lines0 = { "68358.394 352554.320 266.363 68357.225 352552.909 266.369 68355.582 352539.891 266.364 "
				+ "68359.902 352534.335 264.246 68359.994 352534.323 264.206 68360.716 352541.231 264.267 "
				+ "68358.869 352541.472 265.060 68360.214 352552.382 265.069 68359.284 352552.489 265.468 "
				+ "68359.280 352553.929 265.546 68357.394 352554.320 266.363" };

		// int totalblock2 = new int;
		Citygmlkb converter = new Citygmlkb();
		int resulttest = converter.postlistreaderground(lines0, totalpointinblock);
		assertEquals(33, resulttest);
	}

	public void testpostlistreaderwall() {

		int totalpointinblock = 0;

		String[] lines0 = { "68357.394 352554.320 266.363 68357.225 352552.909 266.369 68355.582 352539.891 266.364 "
				+ "68359.902 352534.335 264.246 68359.994 352534.323 264.206 68360.716 352541.231 264.267 "
				+ "68358.869 352541.472 265.060 68360.214 352552.382 265.069 68359.284 352552.489 265.468 "
				+ "68359.280 352553.929 265.546 68357.394 352554.320 266.363" };

		// int totalblock2 = new int;
		Citygmlkb converter = new Citygmlkb();
		int resulttest = converter.postlistreaderwall(lines0, totalpointinblock);
		assertEquals(33, resulttest);
	}

	public void testpostlistreaderroof() {

		int totalpointinblock = 0;

		String[] lines0 = { "68357.394 352554.320 266.363 68357.225 352552.909 266.369 68355.582 352539.891 266.364 "
				+ "68359.902 352534.335 264.246 68359.994 352534.323 264.206 68360.716 352541.231 264.267 "
				+ "68358.869 352541.472 265.060 68360.214 352552.382 265.069 68359.284 352552.489 265.468 "
				+ "68359.280 352553.929 265.546 68357.394 352554.320 266.363" };

		// int totalblock2 = new int;
		Citygmlkb converter = new Citygmlkb();
		int resulttest = converter.postlistreaderroof(lines0, totalpointinblock);
		assertEquals(33, resulttest);
	}

	public void testpostlistreaderlinestring() {

		int totalpointinblock = 0;

		String[] lines0 = { "68357.395 352554.320 266.363 68357.225 352552.909 266.369 68355.582 352539.891 266.364 "
				+ "68359.902 352534.335 264.246 68359.994 352534.323 264.206 68360.716 352541.231 264.267 "
				+ "68358.869 352541.472 265.060 68360.214 352552.382 265.069 68359.284 352552.489 265.468 "
				+ "68359.280 352553.929 265.546 68357.394 352554.320 266.363" };

		// int totalblock2 = new int;
		Citygmlkb converter = new Citygmlkb();
		int resulttest = converter.postlistreaderlinestring(lines0, totalpointinblock);
		assertEquals(33, resulttest);
	}

	/*
	 * public int increaseCountByValue(int i) { System.out.println(i); i += 1;
	 * System.out.println(i); return i; }
	 * 
	 * public void increaseCountByValue(int[] b) { System.out.println(b[0]);
	 * b[0] += 1; System.out.println(b[0]); }
	 * 
	 * public void increaseCountByValueForInteger(Integer i) {
	 * System.out.println(i); i = new Integer(i.intValue() + 1);
	 * System.out.println(i); }
	 * 
	 * public void testCounter() {
	 * 
	 * //Integer a = new Integer(0); int a=0; increaseCountByValue(a);
	 * 
	 * System.out.println(a); }
	 */
}
