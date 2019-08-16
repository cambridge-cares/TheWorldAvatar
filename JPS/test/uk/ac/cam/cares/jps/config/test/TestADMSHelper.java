package uk.ac.cam.cares.jps.config.test;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.json.JSONArray;

import com.google.gson.Gson;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.adms.ADMSOutputAllForShips;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.exception.PythonException;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;
import uk.ac.cam.cares.jps.base.util.PythonHelper;

public class TestADMSHelper extends TestCase {
	
	public void testPythonException() throws IOException {
		
		String jsonString = "1";
		
		Gson g = new Gson();
		
		
		boolean pythonExcWasCaught = false;
		try {
			String result = PythonHelper.callPython("caresjpsadmsinputs/ADMSGeoJsonGetter.py", g.toJson(jsonString), this);
			System.out.println("result="+result);
//			assertEquals("INVALID QUERY", result);
		} catch (PythonException e) {
			e.printStackTrace();
			pythonExcWasCaught = true;
		}
		
		assertTrue(pythonExcWasCaught);
	}
	
	public void testpolcalculation() {
		
		
		String csv = new QueryBroker().readFile("D:/JPS-git/JParkSimulator-git/JPS_SHIP/workingdir/test.levels.gst");
		//String csv = new QueryBroker().readFile("D:/JPS-git/JParkSimulator-git/JPS/workingdir/ADMS/test.levels.gst");
		List<String[]> simulationResult = MatrixConverter.fromCsvToArray(csv);
		int startcontentindex=7;
		int sizeofpol = new ADMSOutputAllForShips().findHowManyPol(simulationResult, startcontentindex);
		int heightamount=(simulationResult.get(0).length-startcontentindex)/sizeofpol;
		
		assertEquals(4,heightamount);
		assertEquals(7,sizeofpol);
		
	}
	
	public void testheader() {
		
		ArrayList<String[]> copier=new ArrayList<String[]>();
		String csv = new QueryBroker().readFile("D:/JPS-git/JParkSimulator-git/JPS/workingdir/ADMS/test.levels.gst");
		List<String[]> simulationResult = MatrixConverter.fromCsvToArray(csv);
		
				
	int totalline=simulationResult.size();
	//hardcoded = 14,15,16 (numpol=9)
	int heightamount=4;
	int numpol=9;
	int startcontentindex=7;
	int pollostmerging=1;
	int newarrsize=simulationResult.get(0).length-(heightamount*pollostmerging);//43
	
		String[] newheader = new String[newarrsize];
		for (int line = 0; line < startcontentindex; line++) {
			newheader[line] = simulationResult.get(0)[line];
		}
		int counter1 = 0;
		for (int heightindex = 0; heightindex < heightamount; heightindex++) { // loop 4x based on height level
			
			for (int line1 = startcontentindex; line1 < 14; line1++) { // (col 7-13 which is normal)
				newheader[line1 + numpol * heightindex] = simulationResult.get(0)[line1 + numpol * heightindex
						- counter1];
				int a=(line1 + numpol * heightindex)- counter1;
				System.out.println("index= "+a);
				System.out.println("a"+simulationResult.get(0)[line1 + numpol * heightindex-counter1]);
			}

			String headernamepm25=simulationResult.get(0)[14 + numpol * heightindex];
			newheader[14 + numpol * heightindex] = headernamepm25.replace(headernamepm25.split("\\|")[2],"PM2.5");
			int b=14 + numpol * heightindex;
			System.out.println("indexb= "+b);
			System.out.println("b"+headernamepm25.replace(headernamepm25.split("\\|")[2],"PM2.5"));
			
			String headernamepm10=simulationResult.get(0)[16 + numpol * heightindex];
			newheader[15 + numpol * heightindex] = headernamepm10.replace(headernamepm10.split("\\|")[2],"PM10");
			int c=15 + numpol * heightindex;
			System.out.println("indexc= "+c);
			System.out.println("c"+headernamepm10.replace(headernamepm10.split("\\|")[2],"PM10"));
			System.out.println("-----------------------------");

			counter1 -= pollostmerging; // how many pol lost after merging

		}
		copier.add(newheader);

		for (int t = 1; t < totalline; t++) { // row
			// System.out.println("totalline= "+t);
			String[] newcontent = new String[newarrsize];
			for (int line = 0; line < startcontentindex; line++) {
				newcontent[line] = simulationResult.get(t)[line];
			}
			int counter = 0;
			for (int heightindex = 0; heightindex < heightamount; heightindex++) { // loop 4x based on height level

				double pm25 = Double.valueOf(simulationResult.get(t)[14 + numpol * heightindex- counter])
						+ Double.valueOf(simulationResult.get(t)[15 + numpol * heightindex- counter]);
				newcontent[14 + numpol * heightindex] = "" + pm25;

				double pm10 = Double.valueOf(simulationResult.get(t)[16 + numpol * heightindex- counter]) + pm25;
				newcontent[15 + numpol * heightindex] = "" + pm10;

				for (int line1 = startcontentindex; line1 < 14; line1++) { // (col 7-13 which is normal)
					newcontent[line1 + numpol * heightindex] = simulationResult.get(t)[line1 + numpol * heightindex
							- counter];
				}

				counter -= pollostmerging; // how many pol lost after merging

			}
			copier.add(newcontent);
		}

		QueryBroker broker = new QueryBroker();
		String baseUrl = QueryBroker.getLocalDataPath() + "/JPS";
		System.out.println(baseUrl);
		
		JSONArray a = new JSONArray();
		for (int z = 0; z < 4; z++) {
			JSONArray h = new JSONArray();
			for (int y = 7; y < 16; y++) { // index0-index 9 for 1 pollutant
				JSONArray pol = new JSONArray();
				for (int x = 1; x < copier.size(); x++) { // 1-the las line
					pol.put(copier.get(x)[y + 9*z]);
				}
				h.put(pol);
			}
			a.put(h);
		}
		System.out.println(a.getJSONArray(0).getJSONArray(8).toString());
		System.out.println(a.getJSONArray(0).getJSONArray(8).length());
		//broker.put(baseUrl + "/newresult.csv", MatrixConverter.fromArraytoCsv(copier));
		
		
		
//	double  pm251=Double.valueOf(simulationResult.get(t)[14])+Double.valueOf(simulationResult.get(t)[15]);
//	newcontent[14]=""+pm251;
//	double  pm101=Double.valueOf(simulationResult.get(t)[16])+pm251;
//	newcontent[15]=""+pm101;
//	
//	double  pm252=Double.valueOf(simulationResult.get(t)[23])+Double.valueOf(simulationResult.get(t)[24]);
//	newcontent[23]=""+pm251;
//	double  pm102=Double.valueOf(simulationResult.get(t)[25])+pm251;
//	newcontent[24]=""+pm101;
//	
//	double  pm253=Double.valueOf(simulationResult.get(t)[32])+Double.valueOf(simulationResult.get(t)[33]);
//	newcontent[32]=""+pm251;
//	double  pm103=Double.valueOf(simulationResult.get(t)[34])+pm251;
//	newcontent[33]=""+pm101;
	
	
	}

}
