package uk.ac.cam.cares.jps.agent.mechanism.moo.test;

import static org.junit.Assert.*;

import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;

import org.json.JSONException;
import org.json.JSONObject;
import org.junit.Test;

import uk.ac.cam.cares.jps.agent.mechanism.moo.MoDSMooAgent;
import uk.ac.cam.cares.jps.agent.mechanism.moo.MoDSMooAgentException;
import uk.ac.cam.cares.jps.base.slurm.job.SlurmJobException;

public class AgentTest {

	@Test
	public void test() throws JSONException, IOException, MoDSMooAgentException, SlurmJobException, URISyntaxException {
		
		MoDSMooAgent agent = new MoDSMooAgent();
		
		String string = "{\"acceptHeaders\":\"text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9\",\"method\":\"GET\",\"requestUrl\":\"http://localhost:8080/MoDSMooAgent/job/request\",\"json\":{\"mods\":{\"calibrationAlg\":{\"initPoints\":\"1\"},\"sensana\":{\"relPerturbation\":\"1e-3\",\"maxORavg\":\"max\",\"topN\":\"10\"},\"samplingAlg\":{\"outputInterval\":\"1000\",\"sobolPoints\":\"10000\"},\"ignDelayOption\":{\"method\":\"1\",\"species\":\"AR\"},\"flameSpeedOption\":{\"tranModel\":\"mix-average\"},\"executable\":{\"path\":\"/home/hpcdeme1/codes/mods-backend/outputs/Release/bin/MoDS_mpi\"}},\"cantera\":{\"environment\":\"cantera_env\"},\"ontochemexpIRI\":{\"ignitionDelay\":[\"https://como.ceb.cam.ac.uk/kb/ontochemexp/x00001700.owl#Experiment_404313416274000\",\"https://como.ceb.cam.ac.uk/kb/ontochemexp/x00001701.owl#Experiment_404313804188800\",\"https://como.ceb.cam.ac.uk/kb/ontochemexp/x00001702.owl#Experiment_404313946760600\"],\"flameSpeed\":[\"https://como.ceb.cam.ac.uk/kb/ontochemexp/x00001703.owl#Experiment_2748799135285400\"]},\"ontokinIRI\":{\"mechanism\":\"http://www.theworldavatar.com/kb/ontokin/mechanism_6651393202518600.owl#ReactionMechanism_6651394316481501\"},\"kinetics\":{\"numerical\":{\"simEnd\":\"500\"}}}}";  
		JSONObject json = new JSONObject(string);  
		
//		MoDSMooAgent.processRequestParameters(json, "/job/request");
//		fail("Not yet implemented");
	
		String message = agent.setUpJob(string).get("jobFolderPath").toString();
	    //   String message = setUpJobOnAgentMachine(jsonString);
	    Path outputPath = agent.jobSubmission.getWorkspaceDirectory().toPath().resolve(message);
	    Map<String, String> fileMap = new HashMap<>();
	    String resultZipFilename = agent.jobSubmission.getHpcAddress() + outputPath.getFileName().toString().substring(outputPath.getFileName().toString().lastIndexOf("_")) + ".zip";
	    fileMap.put(resultZipFilename, "input");
	    Files.walk(outputPath)
            .filter(resultPath -> resultPath.toFile().isFile())
            .forEach(resultPath -> {
                try {
                    String relativeResultPath = outputPath.relativize(resultPath).toString();
                    final URL baseURL = AgentTest.class.getResource(fileMap.getOrDefault(relativeResultPath, relativeResultPath));
                    if (null != baseURL) {
                        Path basePath = Path.of(baseURL.toURI());
                        FileComparer.compareFiles(basePath, resultPath);
                    }
                } catch (URISyntaxException ex) {
                    throw new RuntimeException(resultPath.toString(), ex);
                }
            });
	}

}
