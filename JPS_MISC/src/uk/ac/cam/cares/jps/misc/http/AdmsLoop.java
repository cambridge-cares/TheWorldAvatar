package uk.ac.cam.cares.jps.misc.http;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.annotate.MetaDataAnnotator;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;

public class AdmsLoop {

	public void printHelp() {
		System.out.println("\nAdmsLoop <wait> <from time> <to time> <scenario name> <from index> <to index>");
		System.out.println("wait - wait in seconds before calling ADMS the first time");
		System.out.println("from time - ");
		System.out.println("to time - exclusive");
		System.out.println("scenario name - use \"base\n for base scenario");
		System.out.println("from index - optional, inclusive, >= 0");
		System.out.println("to index - optional, exclusive");
	}
	
	public void start(String[] args) throws IOException {
		
		if (args.length == 0) {
			printHelp();
			return;
		}
		
		int waitSeconds = Integer.valueOf(args[0]);
		long fromMillis = MetaDataAnnotator.getMillisFromXsdTimeStampFormat(args[1]);
		long toMillis = MetaDataAnnotator.getMillisFromXsdTimeStampFormat(args[2]);
		String scenarioName = args[3];
		int fromIndex = -1;
		int toIndex = -1;
		if (args.length > 4) {
			fromIndex = Integer.valueOf(args[4]);
			toIndex = Integer.valueOf(args[5]);
		}
		
		performAdmsLoop(waitSeconds, fromMillis, toMillis, scenarioName, fromIndex, toIndex);
	}
	
	private void performAdmsLoop(int waitSeconds, long fromMillis, long toMillis, String scenarioName, int fromIndex, int toIndex) {
	
		List<Long> timePoints = getTimePointsInMillis(fromMillis, toMillis, fromIndex, toIndex);
		
		System.out.println("\n\n");
		if (timePoints.isEmpty()) {
			System.out.println("No time points have been found !!!");
			return;
		}
		
		int size = timePoints.size();

		int i = fromIndex;
		for (Long current : timePoints) {
			i++;
			String currentTime = MetaDataAnnotator.getTimeInXsdTimeStampFormat(current);
			System.out.println("index = " + i + ", time = " + currentTime);
		}
		
		System.out.println("\nstarting ADMS for the above " + size + " time points in " + waitSeconds + " seconds ...\n");
		
		try {
			Thread.sleep(1000 * waitSeconds);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
		
		i = fromIndex;
		for (Long current : timePoints) {
			i++;
			String currentTime = MetaDataAnnotator.getTimeInXsdTimeStampFormat(current);
			System.out.println("starting ADMS for index = " + i + ", time = " + currentTime);
			
			JSONObject jo = new JSONObject();
			jo.put("scenarioname", scenarioName);
			jo.put("simulationtime", currentTime);
			
			String url = "http://www.theworldavatar.com/JPS_SHIP//ADMSCoordinationAgentForShipWithoutComposition";
			//String result = AgentCaller.executeGetWithURLAndJSON(url, jo.toString());
		}
	}
	
	private List<Long> getTimePointsInMillis(long fromMillis, long toMillis, int fromIndex, int toIndex) {
	
		List<Long> result = new ArrayList<Long>();
		
		int i = -1;
		long hourInMillis = 60 * 60 * 1000;
		for (long currentMillis = fromMillis; currentMillis < toMillis; currentMillis += hourInMillis) {
			i++;
			if ((toIndex != -1) && (i >= toIndex)) {
				break;
			}
			if (i >= fromIndex) {
				result.add(currentMillis);
			}
		}
		
		return result;
	}
}
