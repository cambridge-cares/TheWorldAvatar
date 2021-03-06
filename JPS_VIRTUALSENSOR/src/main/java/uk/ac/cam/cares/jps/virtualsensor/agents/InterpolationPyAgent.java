package uk.ac.cam.cares.jps.virtualsensor.agents;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;

import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.util.CommandHelper;
import uk.ac.cam.cares.jps.virtualsensor.configuration.SensorVenv;

/**
 * Temporary interpolation agent that calls a python script instead of matlab
 * this script will only work with Episode output files
 * The python script takes in the location of the output file and coordinates to interpolate
 * It returns the interpolated values at each height in the simulation
 * @author Kok Foong Lee
 *
 */
@WebServlet(urlPatterns="/InterpolationPyAgent")
public class InterpolationPyAgent extends JPSAgent {
	@Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        JSONObject responseParams = new JSONObject();
        if (validateInput(requestParams)) {
	        Path pyWorkingDir = Paths.get(AgentLocator.getCurrentJpsAppDirectory(this),"python","interpolation");
	        ArrayList<String> args = new ArrayList<String>();
			args.add(SensorVenv.pyexe.toString());
			args.add("interpolation.py");
			args.add(requestParams.getString("filepath"));
			args.add(String.valueOf(requestParams.getDouble("x")));
			args.add(String.valueOf(requestParams.getDouble("y")));

			String result = CommandHelper.executeCommands(pyWorkingDir.toString(), args);
			responseParams = new JSONObject(result);
        }
        return responseParams;
    }

	@Override
	public boolean validateInput(JSONObject requestParams) {
    	boolean valid = false;
    	try {
    		// check output file exists
    		File outputfile = new File(requestParams.getString("filepath"));
    		if (!outputfile.exists()) throw new Exception();
    		// ensure that coordinates are valid doubles
    		requestParams.getDouble("x");
    		requestParams.getDouble("y");
    		valid = true;
    	} catch (Exception e) {
    		throw new BadRequestException(e);
    	}
    	return valid;
    }
}
