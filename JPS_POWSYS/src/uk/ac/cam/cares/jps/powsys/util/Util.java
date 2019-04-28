package uk.ac.cam.cares.jps.powsys.util;

import uk.ac.cam.cares.jps.base.config.AgentLocator;

public class Util {

	public static String getResourceDir(Object thisObject) {
		return AgentLocator.getCurrentJpsAppDirectory(thisObject) + "/res";
	}
}
