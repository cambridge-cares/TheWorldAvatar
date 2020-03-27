package com.cmclinnovations.jps.agent.quantum.calculation;

import java.io.IOException;
import java.io.UnsupportedEncodingException;

import org.junit.Test;

import com.cmclinnovations.slurm.job.SlurmJobException;
import com.cmclinnovations.slurm.job.Status;

import org.junit.Assert;

public class EBRAgentJobRequestTest {
	@Test
	public void testJobRequest() {
		try {
			String query = "{\"job\":{\"levelOfTheory\":\"B3LYP/6-31G(d)\",\"keyword\": \"Opt\",\"algorithmChoice\": \"Freq\"},\"speciesIRI\": \"http://www.theworldavatar.com/kb/ontospecies/00b7e248-ae24-35bf-b7a0-b470b923ddf6.owl#00b7e248-ae24-35bf-b7a0-b470b923ddf6\"}";
			EBRAgent ebrAgent = new EBRAgent();
			String messageActual = ebrAgent.setUpJob(query);
			Assert.assertEquals(messageActual, Status.JOB_SETUP_SUCCESS_MSG.getName());
			System.out.println(messageActual);
		} catch (EBRAgentException e) {
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
