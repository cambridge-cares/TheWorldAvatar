package uk.ac.cam.cares.jps.composition.util;

public class FilePathManager {

	public static String getFilePath(String host) throws Exception {

		System.out.println(host + "Host " + SendRequest.sendGet(host + "/JPS_COMPOSITION/FilePathAPI"));
		return SendRequest.sendGet(host + "/JPS_COMPOSITION/FilePathAPI");
	}
}
