package uk.ac.cam.cares.jps.agent.kinetics.simulation.test;

import java.io.IOException;
import java.io.UnsupportedEncodingException;

import org.junit.Test;

import uk.ac.cam.cares.jps.agent.kinetics.simulation.KineticsAgent;
import uk.ac.cam.cares.jps.agent.kinetics.simulation.KineticsAgentException;
import uk.ac.cam.cares.jps.base.slurm.job.SlurmJobException;
import uk.ac.cam.cares.jps.base.slurm.job.Status;

import org.junit.Assert;

public class KineticsAgentJobRequestTest {
	@Test
	public void testJobRequest() {
		try {
			String query = "{\"Inputs\":{\"$INP_CROSS_SECTION_VALUE\":\"2\","
					+ "\"$INP_CROSS_SECTION_UNIT\":\"m\","
					+ "\"$INP_TEMPERATURE_PROFILE_DEPVAR_UNIT\":\"K\","
					+ "\"$INP_TEMPERATURE_PROFILE_INDVAR_UNIT\":\"s\","
					+ "\"$INP_TEMPERATURE_PROFILE_VALUES\":\"0, 500 \n 0.01, 550 \n 1, 1000\","
					+ "\"$INP_MASS_FLOW_RATE_VALUE\":\"0.0005\","
					+ "\"$INP_MASS_FLOW_RATE_UNIT\":\"kg/s\","
					+ "\"$INP_PRESSURE_VALUE\":\"1.0\","
					+ "\"$INP_PRESSURE_UNIT\":\"atm\","
					+ "\"$INP_MIX_COMP_UNIT\":\"mole fraction\","
					+ "\"$INP_MIX_COMP_C6H6_FRACTION\":\"0.03\","
					+ "\"$INP_MIX_COMP_C2H2_FRACTION\":\"0.03\","
					+ "\"$INP_MIX_COMP_N2_FRACTION\":\"0.94\","
					+ "\"$INP_COAG_MULT\":\"0.237137371\","
					+ "\"$INP_ExhCADs\":\"141.0 \n 197.0 \n 253.0 \n 309.0 \n 365.0\"},"
					+ "\"Case\": \"src\", \"Outputs\":[\"$OUT_MEAN_PART_DIAMETER\", \"$OUT_PART_NUMBER\"]}";
			KineticsAgent kineticsAgent = new KineticsAgent();
			String messageActual = kineticsAgent.setUpJob(query).toString();
			Assert.assertEquals(messageActual, Status.JOB_SETUP_SUCCESS_MSG.getName());
			System.out.println(messageActual);
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
