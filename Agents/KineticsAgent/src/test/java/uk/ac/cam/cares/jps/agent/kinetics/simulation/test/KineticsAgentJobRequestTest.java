package uk.ac.cam.cares.jps.agent.kinetics.simulation.test;

import com.google.common.io.Resources;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.nio.charset.StandardCharsets;

import org.junit.Test;

import uk.ac.cam.cares.jps.agent.kinetics.simulation.KineticsAgent;
import uk.ac.cam.cares.jps.agent.kinetics.simulation.KineticsAgentException;
import uk.ac.cam.cares.jps.base.slurm.job.SlurmJobException;
import uk.ac.cam.cares.jps.base.slurm.job.Status;

import org.junit.Assert;

public class KineticsAgentJobRequestTest {

	/**
	 * Performs a test of the agent's job setup using a sample JSON request (read from a resource file).
	 *
	 * Note that this test will only function if the kinetics agent scripts have been correctly installed (using the
	 * SRM backends "for_release" script) and structure of the agents directory matches the following (and is set in
	 * the "agent.scripts.location" property within the kinetics-agent.properties file).
	 * 
	 *	- agent directory
	 *		- simulation_templates
	 *		- venv
	 *			- Scripts
	 *				- agkin_pre.exe
	 *				- agkin_post.exe
	 */
	@Test
	public void testJobSetup() {
		try {
			// Read the sample JSON input file from the resources folder
			URL url = Resources.getResource("agent-case-1-input.json");
			String query = Resources.toString(url, StandardCharsets.UTF_8);
			Assert.assertTrue("Could not read JSON query from resources file!", query != null && query.length() > 0);
			
			// Submit the job
			KineticsAgent kineticsAgent = new KineticsAgent();
			String messageActual = kineticsAgent.setUpJob(query).toString();
			
			System.out.println("JOB MESSAGE:");
			System.out.println(messageActual);
			
			Assert.assertEquals(messageActual, Status.JOB_SETUP_SUCCESS_MSG.getName());
			
			
		} catch (KineticsAgentException e) {
			e.printStackTrace();
		} catch (UnsupportedEncodingException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		} catch (SlurmJobException e) {
			e.printStackTrace();
		}
	}
}
