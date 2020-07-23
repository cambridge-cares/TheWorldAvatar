package com.cmclinnovations.jps.agent.json.parser;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.json.JSONArray;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.jayway.jsonpath.JsonPath;

public class JSonRequestParser {
	
	// {}

	// OntoChemExp part
//	public static JSONArray getOntoChemExpIgnitionDelayIRI(String jsonString) {
//		return JsonPath.read(jsonString, "$.json.ontochemexpIRI.ignitionDelay");
//	}
	
	public static List<String> getOntoChemExpIgnitionDelayIRI(String jsonString) throws JsonProcessingException, IOException {
		JsonNode locateNode = new ObjectMapper().readTree(jsonString).path("json").path("ontochemexpIRI").path("ignitionDelay");
		List<String> expIRI = new ArrayList<>();
		for (JsonNode arrayItem : locateNode) {
			expIRI.add(arrayItem.asText());
		}
		
		return expIRI;
	}
	
	public static List<String> getOntoChemExpFlameSpeedIRI(String jsonString) throws JsonProcessingException, IOException {
		JsonNode locateNode = new ObjectMapper().readTree(jsonString).path("json").path("ontochemexpIRI").path("flameSpeed");
		List<String> expIRI = new ArrayList<>();
		for (JsonNode arrayItem : locateNode) {
			expIRI.add(arrayItem.asText());
		}
		
		return expIRI;
	}
	
	
//	public static List<String> getOntoChemExpFlameSpeedIRI(String jsonString) {
//		return JsonPath.read(jsonString, "$.json.ontochemexpIRI.flameSpeed");
//	}
	
	// OntoKin part
	// mechanism
//	public static String getOntoKinMechanismIRI(String jsonString) {
//		return JsonPath.read(jsonString, "$.json.ontokinIRI.mechanism");
//	}
//	// reactions
//	public static List<String> getOntoKinReactionsIRI(String jsonString) {
//		return JsonPath.read(jsonString, "$.json.ontokinIRI.reactionList");
//	}
	
	public static String getOntoKinMechanismIRI(String jsonString) throws JsonProcessingException, IOException {
		JsonNode locateNode = new ObjectMapper().readTree(jsonString).path("json").path("ontokinIRI").path("mechanism");
		
		return locateNode.asText();
	}
	
	public static List<String> getOntoKinReactionsIRI(String jsonString) throws JsonProcessingException, IOException {
		JsonNode locateNode = new ObjectMapper().readTree(jsonString).path("json").path("ontokinIRI").path("reactionList");
		List<String> reactionIRI = new ArrayList<>();
		for (JsonNode arrayItem : locateNode) {
			reactionIRI.add(arrayItem.asText());
		}
		
		return reactionIRI;
	}
	
	// algorithm part
	public static String getAlgName(String jsonString) {
		return JsonPath.read(jsonString, "$.job.mods.algorithm.name");
	}

	public static String getAlgOptimisableParam(String jsonString) {
		return JsonPath.read(jsonString, "$.job.mods.algorithm.optimisable_param_subtypes");
	}

	public static String getAlgResponseParam(String jsonString) {
		return JsonPath.read(jsonString, "$.job.mods.algorithm.response_param_subtypes");
	}

	public static String getAlgType(String jsonString) {
		return JsonPath.read(jsonString, "$.job.mods.algorithm.algorithm_type");
	}

	public static String getAlgModelName(String jsonString) {
		return JsonPath.read(jsonString, "$.job.mods.algorithm.model_name");
	}

	public static String getAlgObjectiveFunction(String jsonString) {
		return JsonPath.read(jsonString, "$.job.mods.algorithm.objective_function");
	}

	public static String getAlgOutputByCase(String jsonString) {
		return JsonPath.read(jsonString, "$.job.mods.algorithm.output_by_case");
	}

	public static String getAlgOutputValues(String jsonString) {
		return JsonPath.read(jsonString, "$.job.mods.algorithm.output_values");
	}

	public static String getAlgNPoints(String jsonString) {
		return JsonPath.read(jsonString, "$.job.mods.algorithm.n_points");
	}

	public static String getAlgNIters(String jsonString) {
		return JsonPath.read(jsonString, "$.job.mods.algorithm.n_iters");
	}

	public static String getAlgNIntitialPoints(String jsonString) {
		return JsonPath.read(jsonString, "$.job.mods.algorithm.n_initial_points");
	}

	public static String getAlgConstrained(String jsonString) {
		return JsonPath.read(jsonString, "$.job.mods.algorithm.constrained");
	}

	public static String getAlgRho(String jsonString) {
		return JsonPath.read(jsonString, "$.job.mods.algorithm.rho");
	}

	public static String getAlgRhoFactor(String jsonString) {
		return JsonPath.read(jsonString, "$.job.mods.algorithm.rho_factor");
	}

	public static String getAlgEpsilon(String jsonString) {
		return JsonPath.read(jsonString, "$.job.mods.algorithm.epsilon");
	}

	public static String getAlgSeed(String jsonString) {
		return JsonPath.read(jsonString, "$.job.mods.algorithm.seed");
	}

	public static String getAlgOutputInterval(String jsonString) {
		return JsonPath.read(jsonString, "$.job.mods.algorithm.output_interval");
	}

	public static String getAlgPreviousAlg(String jsonString) {
		return JsonPath.read(jsonString, "$.job.mods.algorithm.previous_algorithm");
	}

	// model part
	public static String getModelName(String jsonString) {
		return JsonPath.read(jsonString, "$.job.mods.model.name");
	}

	public static String getModelExeName(String jsonString) {
		return JsonPath.read(jsonString, "$.job.mods.model.executable_name");
	}

	public static String getModelWorkingDir(String jsonString) {
		return JsonPath.read(jsonString, "$.job.mods.model.working_directory");
	}

	public static String getModelArgs(String jsonString) {
		return JsonPath.read(jsonString, "$.job.mods.model.args");
	}

	public static String getModelFnamesDepModels(String jsonString) {
		return JsonPath.read(jsonString, "$.job.mods.model.fnames_for_dependent_models");
	}

//	public static String getCaseName(String jsonString) {
//		return JsonPath.read(jsonString, "$.job.mods.case.name");
//	}
//	
//	public static String getCaseModel(String jsonString) {
//		return JsonPath.read(jsonString, "$.job.mods.case.model");
//	}
//	
//	public static String getFileName(String jsonString) {
//		return JsonPath.read(jsonString, "$.job.mods.file.name");
//	}
//	
//	public static String getFileDetailName(String jsonString) {
//		return JsonPath.read(jsonString, "$.job.mods.file.detail.name");
//	}
//	
//	public static String getFileDetail(String jsonString) {
//		return JsonPath.read(jsonString, "$.job.mods.file.");
//	}
//	
//	public static String getFileName(String jsonString) {
//		return JsonPath.read(jsonString, "$.job.mods.file.");
//	}

}
