package uk.ac.cam.cares.jps.config.test;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.commons.lang.ArrayUtils;
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
	
	public void testheaderpostprocessing() {
		
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
		int index=startcontentindex;
		
		
		for (int heightindex = 0; heightindex < heightamount; heightindex++) { // loop 4x based on height level
			boolean flag10 = false;
			boolean flag25 = false;
			int line1 = startcontentindex;
			while (Double.valueOf(simulationResult.get(0)[line1].split("Z=")[1].split("m")[0])
					- Double.valueOf(simulationResult.get(0)[startcontentindex].split("Z=")[1].split("m")[0]) == 0.0) {

				if (simulationResult.get(0)[line1 + numpol * heightindex + counter1].contains("PM2.5")) {
					if (flag25 == false) {
						String headernamepm25 = simulationResult.get(0)[line1 + numpol * heightindex];
						newheader[index] = headernamepm25.replace(headernamepm25.split("\\|")[2], "PM2.5");
						flag25 = true;
						
						index++;
					}

				} else if (simulationResult.get(0)[line1 + numpol * heightindex + counter1].contains("PM10")) {
					if (flag10 == false) {
						String headernamepm10 = simulationResult.get(0)[line1 + numpol * heightindex];
						newheader[index] = headernamepm10.replace(headernamepm10.split("\\|")[2], "PM10");
						flag10 = true;
						
						index++;
					}

				} else {
					newheader[index] = simulationResult.get(0)[line1 + numpol * heightindex + counter1];
					
					index++;
				}

				line1++;

			}
			counter1 += pollostmerging; // how many pol lost after merging

		}
		copier.add(newheader);

				
		for (int t = 1; t < totalline; t++) { // row
			String[] newcontent = new String[newarrsize];
			for (int line = 0; line < startcontentindex; line++) {
				newcontent[line] = simulationResult.get(t)[line];
			}
			int counter = 0;
			int indexcontent=startcontentindex;
			
			for (int heightindex = 0; heightindex < heightamount; heightindex++) { // loop 4x based on height level
				boolean flag10 = false;
				boolean flag25 = false;
				int line1 = startcontentindex;
				int lockindexpm25 = 0;
				double pm25 = 0;
				int lockindexpm10 = 0;
				double pm10 = 0;

				while (Double.valueOf(simulationResult.get(0)[line1].split("Z=")[1].split("m")[0]) - Double
						.valueOf(simulationResult.get(0)[startcontentindex].split("Z=")[1].split("m")[0]) == 0.0) {

					if (simulationResult.get(0)[line1 + numpol * heightindex + counter].contains("PM2.5")) {
						pm25 = pm25 + Double.valueOf(simulationResult.get(t)[line1 + numpol * heightindex + counter]);
						if (flag25 == false) {

							lockindexpm25 = indexcontent;
							flag25 = true;
							indexcontent++;
						}
//						newcontent[lockindexpm25] = "" + pm25;

					} else if (simulationResult.get(0)[line1 + numpol * heightindex + counter].contains("PM10")) {
						pm10 = pm10 + Double.valueOf(simulationResult.get(t)[line1 + numpol * heightindex + counter]);
						if (flag10 == false) {

							lockindexpm10 = indexcontent;
							flag10 = true;
							indexcontent++;
						}
//						newcontent[lockindexpm10] = "" + (pm10+pm25);

					} else {
						newcontent[indexcontent] = simulationResult.get(t)[line1 + numpol * heightindex + counter];
						// System.out.println("indexc= " + index);
						indexcontent++;
					}

					line1++;

				}
				newcontent[lockindexpm25] = "" + pm25;
				newcontent[lockindexpm10] = "" + (pm10+pm25);

				counter += pollostmerging; // how many pol lost after merging

			}
			copier.add(newcontent);
		}
		
		QueryBroker broker = new QueryBroker();
		String baseUrl = QueryBroker.getLocalDataPath() + "/JPS";
		System.out.println(baseUrl);
		
		JSONArray a = new JSONArray();
		for (int z = 0; z < heightamount; z++) {
			JSONArray h = new JSONArray();
			for (int y = startcontentindex; y < 16; y++) { // index0-index 9 for 1 pollutant
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
		broker.put(baseUrl + "/newresult2.csv", MatrixConverter.fromArraytoCsv(copier));
		
		
		
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
