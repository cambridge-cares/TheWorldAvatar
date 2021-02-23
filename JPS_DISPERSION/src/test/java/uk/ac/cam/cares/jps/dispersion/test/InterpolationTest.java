package uk.ac.cam.cares.jps.dispersion.test;

import java.io.File;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import org.json.JSONObject;
import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.annotate.MetaDataAnnotator;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.dispersion.interpolation.InterpolationAgent;

public class InterpolationTest extends TestCase{
	//test simulation
	public void testepisoderunTestinSequenceDirect() {
		InterpolationAgent ag = new InterpolationAgent();
		String baseUrl= QueryBroker.getLocalDataPath()+"/JPS_DIS";
		String coordinates = "[30207.15 26784.95 0]";
		String gasType, dispMatrix ="";
		String agentiri = "http://www.theworldavatar.com/kb/agents/Service__ADMS.owl#Service";
		String location = "http://dbpedia.org/resource/Singapore";
		String[] directory = ag.getLastModifiedDirectory(agentiri, location);
		File directoryFolderWrong = new File(directory[0]);
		String directoryFolder = directoryFolderWrong.getParent();
		String directorytime = directory[1];
		System.out.println("directorytime= "+directorytime);
		System.out.println("directoryfolder= "+directoryFolder);
		String[] arrayFile = ag.finder(directoryFolder);
		String fGas = arrayFile[0];
		File lstName = new File(directoryFolder, fGas);//fGas is the name of the test.levels.gst
		if (lstName.getName().endsWith(".dat")) {
			ArrayList<String> gsType = ag.determineGas(directoryFolder, lstName);
			gasType = gsType.get(0);
			String fileName = gsType.get(1);		
			dispMatrix = ag.copyOverFile(baseUrl,fileName);//to be swapped with copyOverFile
		}
		else {
			ArrayList<String> gsType = ag.determineGasGst(directoryFolder, lstName);
			gasType = gsType.get(0);
			String fileName = gsType.get(1);
			dispMatrix = ag.rearrangeGst(baseUrl, fileName, gasType);
			
		}
		ag.copyTemplate(baseUrl, "virtual_sensor.m");
		//modify matlab to read 
			try {
				ag.createBat(baseUrl, coordinates,gasType, "1", dispMatrix);
				ag.runModel(baseUrl);
	           
			} catch (Exception e) {
				e.printStackTrace();
			}
		 }
	
	public void testfinder() {
		String[] arrayFile = new InterpolationAgent().finder("C:/JPS_DATA/workingdir/JPS_SCENARIO/scenario/base/localhost_8080/data/db57c791-d9aa-4a6e-9f21-44e991ed9c17/JPS_ADMS");
		System.out.println(arrayFile[0]);
	
	}
	

	
	//test determineGas
	public void testdetermineGas() {
		System.out.println(new InterpolationAgent()
				.determineGasGst("C:\\Users\\ongajong\\Downloads\\JPS_ADMS\\JPS_ADMS",
						new File("C:\\Users\\ongajong\\Downloads\\JPS_ADMS\\JPS_ADMS","test.levels.gst")));
	}
	//test getLastModifiedDirectory
	public void testAddMetadataAnnotator() {
		String baseUrl = QueryBroker.getLocalDataPath();//folder baseUrl should be // and not \\
		//expect baseUrl to be returned
		String agent = "http://www.theworldavatar.com/kb/agents/Service__ComposedADMS#Service";
		String location = "http://dbpedia.org/resource/Singapore";
		List<String> lst = new ArrayList<String>();
		lst.add(location);
		MetaDataAnnotator.annotate(baseUrl, null, agent, true, lst);
		assertEquals(new InterpolationAgent().getLastModifiedDirectory(agent, location), baseUrl);
	}
	//test copyOverFile
	public void testcopyOverFile() {
		System.out.println(new InterpolationAgent().copyOverFile("C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\base\\localhost_8080\\data\\ddfd101b-33ca-4511-82f2-1f4fa48f4ee8\\JPS_DIS",
				"C://Users//ongajong//JParkSimulator-git//JPS_DISPERSION//workingdir//3D_instantanous_mainconc_center.dat"));
	}
	public void testgetLastModified() {
		System.out.println(new InterpolationAgent().getLastModifiedDirectory("http://www.theworldavatar.com/kb/agents/Service__ComposedEpisode.owl#Service",
				"http://dbpedia.org/resource/Singapore"));
	}
	public void testtimemodif() {
		String time=new InterpolationAgent().getLastModifiedDirectory("http://www.theworldavatar.com/kb/agents/Service__ADMS.owl#Service",
				"http://dbpedia.org/resource/Singapore")[1];
		System.out.println("time= "+time);
		DateFormat utcFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS");
		   utcFormat.setTimeZone(TimeZone.getTimeZone("GMT"));

		try {
			  Date date = utcFormat.parse(time);
			   DateFormat pstFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssXXX");
			   pstFormat.setTimeZone(TimeZone.getTimeZone("GMT+8"));

			   System.out.println("after mod=" +pstFormat.format(date));
//			   time=pstFormat.format(date)+"+08:00";
//			   System.out.println("time converted=");
		} catch (ParseException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	/**
	 * test validInput Function
	 */
	public void testInput() {
		JSONObject jo = new JSONObject();
		jo.put("agent","http://www.theworldavatar.com/kb/agents/Service__ComposedEpisode.owl#Service");
//		jo.put("agent","wanker");
		jo.put("options","1");
		jo.put("city","http://dbpedia.org/resource/Singapore");
		jo.put("airStationIRI", "http://www.theworldavatar.com/kb/sgp/singapore/AirQualityStation-001.owl#AirQualityStation-001");
		new InterpolationAgent().validateInput(jo.toString());
	}
	
}