package uk.ac.cam.cares.jps.agent.file_management.marshallr.sensana;

import java.util.LinkedHashMap;

import uk.ac.cam.cares.jps.agent.mechanism.calibration.MoDSAgentException;
import uk.ac.cam.cares.jps.kg.OntoKinKG;

public class AppTest {
	public static void main(String[] args) throws MoDSAgentException {
		OntoKinKG ontoKinkg = new OntoKinKG();
		String mechanismIRI = "http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ReactionMechanism_1230848575548237";
		LinkedHashMap<String, String> queriedReactionList = ontoKinkg.queryAllReactions(mechanismIRI);
		System.out.println(queriedReactionList);
	}
}
