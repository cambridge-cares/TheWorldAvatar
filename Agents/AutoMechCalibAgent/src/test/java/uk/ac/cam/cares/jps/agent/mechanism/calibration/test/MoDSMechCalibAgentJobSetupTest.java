package uk.ac.cam.cares.jps.agent.mechanism.calibration.test;

import static org.junit.Assert.*;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;

import org.junit.Test;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import uk.ac.cam.cares.jps.agent.mechanism.coordination.AutoMechCalibAgent;
import uk.ac.cam.cares.jps.agent.mechanism.coordination.AutoMechCalibAgentException;
import uk.ac.cam.cares.jps.agent.mechanism.coordination.Utils;
import uk.ac.cam.cares.jps.base.slurm.job.JobSubmission;
import uk.ac.cam.cares.jps.base.slurm.job.SlurmJobException;
import uk.ac.cam.cares.jps.base.slurm.job.configuration.SlurmJobProperty;
import uk.ac.cam.cares.jps.base.slurm.job.configuration.SpringConfiguration;
import uk.ac.cam.cares.jps.base.util.FileUtil;

public class MoDSMechCalibAgentJobSetupTest extends AutoMechCalibAgent {

	@Test
	public void test() throws IOException, AutoMechCalibAgentException, SlurmJobException {
		if (applicationContext == null) {
			applicationContext = new AnnotationConfigApplicationContext(SpringConfiguration.class);
		}
		if (slurmJobProperty == null) {
			slurmJobProperty = applicationContext.getBean(SlurmJobProperty.class);
		}
		if (jobSubmission == null) {
			jobSubmission = new JobSubmission(slurmJobProperty.getAgentClass(), 
					slurmJobProperty.getHpcAddress());
		}
		BufferedReader br = FileUtil.openSourceFile(getClass().getClassLoader().getResource(slurmJobProperty.getJsonInputFileName().concat(slurmJobProperty.getJsonFileExtension())).getPath());
		
		String jsonString = "";
		String line = "";
		while ((line = br.readLine()) != null) {
			jsonString = jsonString.concat(line);
		}
		br.close();
		setUpJobOnAgentMachine(jsonString);
	}
	
	/**
	 * Sets up the calibration job for the current input. 
	 * 
	 * @param jsonString
	 * @return
	 * @throws IOException
	 * @throws AutoMechCalibAgentException
	 * @throws SlurmJobException
	 */
	private String setUpJobOnAgentMachine(String jsonString) throws IOException, AutoMechCalibAgentException, SlurmJobException {
		long timeStamp = Utils.getTimeStamp();
		return jobSubmission.setUpJob(jsonString, 
				new File(getClass().getClassLoader().getResource(slurmJobProperty.getSlurmScriptFileName()).getPath()), 
				new File(getClass().getClassLoader().getResource(slurmJobProperty.getInputFileName().concat(slurmJobProperty.getInputFileExtension())).getPath()), 
				timeStamp);
	}
}
