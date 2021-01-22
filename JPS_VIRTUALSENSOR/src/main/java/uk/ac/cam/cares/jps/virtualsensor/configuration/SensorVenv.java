package uk.ac.cam.cares.jps.virtualsensor.configuration;

import java.nio.file.Path;
import java.nio.file.Paths;

import org.apache.commons.lang.SystemUtils;

/**
 * This class specifies the location of the virtual environment
 * @author Kok Foong Lee
 *
 */

public class SensorVenv {
	private static Path pyrelpath = SystemUtils.IS_OS_LINUX ? Paths.get("bin","python") : Paths.get("Scripts","python.exe");
	// be aware that Sensor_venv is hard-coded in the build script pyvenv.cmd
	public static Path pyexe = Paths.get(System.getProperty("user.home"),"Sensor_venv",pyrelpath.toString());
}
