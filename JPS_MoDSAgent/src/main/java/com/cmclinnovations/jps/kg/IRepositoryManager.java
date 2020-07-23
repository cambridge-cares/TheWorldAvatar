package com.cmclinnovations.jps.kg;

import java.util.List;

import com.cmclinnovations.jps.agent.mechanism.calibration.MoDSAgentException;

public interface IRepositoryManager {
	public void loadOntology(String serverURL, String mechanismName, String mechanismFilePath, String baseURI, String repositoryID) throws OntoException;
	public void deleteOntology(String serverURL, String mechanismName, String contextURL, String repositoryID) throws OntoException;
	public void downloadOntology(String serverURL, String mechanismName, String contextURL, String repositoryID, String filePath) throws OntoException;
	public String queryRepositoryReturnJson(String serverURL, String repositoryID, String queryString) throws OntoException;
	public List<String> queryRepositoryExperimentalData(String serverURL, String repositoryID, String queryString) throws OntoException, MoDSAgentException;
	public List<List<String>> queryRepository(String serverURL, String repositoryID, String queryString) throws MoDSAgentException;
//	public String formReactionMechanismQuery(String ontochemPrefix);
//	public String formInstanceValueQuery(String instanceURL, String property, String ontochemPrefix);
}
