package uk.ac.cam.cares.jps.ontomatch;

import java.io.BufferedWriter;
import java.io.ByteArrayOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.query.ResourcePathConverter;
import uk.ac.cam.cares.jps.base.util.AsyncPythonHelper;
import uk.ac.cam.cares.jps.base.util.FileUtil;

/**
 * Agent Component that contains: Input and output methods to render
 * alignment(List<Map>) to knowledge graph and back
 *
 * @author shaocong zhang
 * @version 1.0
 * @since 2020-09-08
 */
public class AlignmentIOHelper {
	/**
	 * Alignment knowledge graph(RDF) to JSONArray
	 * 
	 * @param iriOfAlignmentFile
	 * @param threshold
	 * @return JSONArray
	 * @throws Exception
	 */
	public static JSONArray readAlignmentFileAsJSONArray(String iriOfAlignmentFile, double threshold) throws Exception {
		String queryStr = "PREFIX alignment: <http://knowledgeweb.semanticweb.org/heterogeneity/alignment#> "
				+ "SELECT ?entity1 ?entity2 ?measure " + "WHERE {?cell alignment:entity1 ?entity1."
				+ "?cell  alignment:entity2 ?entity2 ." + "?cell alignment:measure ?measure." + "FILTER (?measure >= "
				+ Double.toString(threshold) + " ) " 
				+ "}";
		JSONArray resArr = new JSONArray();
		List<String[]> resultListfromquery = null;
        //load model and query
		OntModel model = JenaHelper.createModel(iriOfAlignmentFile);
		ResultSet resultSet = JenaHelper.query(model, queryStr);
		//format result: to JSONArray
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
		String[] keys = JenaResultSetFormatter.getKeys(result);
		resultListfromquery = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
		for (String[] paras : resultListfromquery) {
			JSONObject resObj = new JSONObject();
			for (int idx = 0; idx < keys.length; idx++) {
				resObj.put(keys[idx], paras[idx]);
			}
			resArr.put(resObj);
		}

		return resArr;
	}

	/***
	 *  Alignment knowledge graph(RDF) to JSONArray, with threshold 0
	 * @param iriOfAlignmentFile
	 * @return JSONArray
	 * @throws Exception
	 */
	public static JSONArray readAlignmentFileAsJSONArray(String iriOfAlignmentFile) throws Exception {
		return readAlignmentFileAsJSONArray(iriOfAlignmentFile, 0.0);
	}

	/***
	 * alignment(List<Map>) to JsonArray
	 * @param mlist(alignment as java List)
	 * @return JSONArray
	 */
	public static JSONArray scoreList2Json(List<Map> mlist) {
		JSONArray jlist = new JSONArray();
		for (int i = 0; i < mlist.size(); i++) {
			Map mcell = mlist.get(i);
			JSONArray ja = new JSONArray();
			ja.put(mcell.get("entity1"));
			ja.put(mcell.get("entity2"));
			ja.put(mcell.get("measure"));
			jlist.put(ja);
		}
		return jlist;
	}

	/**
	 * JSONArray to alignment(List<Map>)
	 * 
	 * @param ja, JSONArray
	 * @return alignment(List<Map>)
	 */
	public static List<Map> Json2ScoreList(JSONArray ja) {
		List<Map> mlist = new ArrayList<Map>();
		for (int idx = 0; idx < ja.length(); idx++) {
			JSONObject jo = ja.getJSONObject(idx);
			Map<String, Object> mcell = new HashMap();
			mcell.put("entity1", jo.getString("entity1"));
			mcell.put("entity2", jo.getString("entity1"));
			mcell.put("measure", jo.getInt("measure"));
			mlist.add(mcell);
		}
		return mlist;
	}

	/***
	 * Alignment(List<Map>) to knowledge graph(RDF)
	 * 
	 * @param alignment
	 * @param onto1
	 * @param onto2
	 * @param addr(local address)
	 * @throws IOException
	 */
	public static void writeAlignment2File(List<Map> alignment, String onto1, String onto2, String aIRI)
			throws IOException {
		String addr = ResourcePathConverter.convertToLocalPath(aIRI);
		QueryBroker b = new QueryBroker();
		try {
			Model model = ModelFactory.createDefaultModel();
			ByteArrayOutputStream baos = new ByteArrayOutputStream();
			BufferedWriter writer = new BufferedWriter(new FileWriter(addr));
			model.write(writer);
		} catch (Exception e) {
			e.printStackTrace();
		}
		// write header: onto1 address, onto2 address
		String alignmentHeaderUpdateStr = "PREFIX a: <http://knowledgeweb.semanticweb.org/heterogeneity/alignment#> "
				+ "INSERT DATA{\r\n" + "  <" + aIRI + "> a a:Alignment.\r\n" + "  <" + aIRI + "> a:onto1  <" + aIRI
				+ "#onto1>.\r\n" + "  <" + aIRI + "> a:onto2  <" + aIRI + "#onto2>.\r\n" + "  <" + aIRI
				+ "#onto1> a  a:ontology.\r\n" + "  <" + aIRI + "#onto1> a:location  \"" + onto1 + "\".\r\n" + "  <"
				+ aIRI + "#onto2> a  a:ontology.\r\n" + "  <" + aIRI + "#onto2> a:location  \"" + onto2 + "\".\r\n"
				+ "}";
		b.updateFile(aIRI, alignmentHeaderUpdateStr);
		//write each pair of aligned entity: entity1 IRI, entity2 IRI, measure(score)
		for (int idx = 0; idx < alignment.size(); idx++) {
			Map matched = alignment.get(idx);
			String nodeName = aIRI + "#cell" + Integer.toString(idx + 1);
			String updateStr = getCellUpdateStr(matched, nodeName);
			b.updateFile(aIRI, updateStr);
		}
	}

	/**
	 * construct one set of sparql string to insert one matched pair of alignment
	 * file
	 * 
	 * @param match
	 * @param nodeIRI
	 * @return String of update sparql
	 */
	private static String getCellUpdateStr(Map match, String nodeIRI) {
		String entity1 = (String) match.get("entity1");
		String entity2 = (String) match.get("entity2");
		double measure = (double) match.get("measure");
		String alignmentCellUpdateStr = "PREFIX a: <http://knowledgeweb.semanticweb.org/heterogeneity/alignment#> "
				+ "INSERT DATA{\r\n" + "  <" + nodeIRI + "> a a:Cell.\r\n" + "  <" + nodeIRI + "> a:entity1 \""
				+ entity1 + "\".\r\n" + "  <" + nodeIRI + "> a:entity2 \"" + entity2 + "\".\r\n" + "  <" + nodeIRI
				+ "> a:measure \"" + Double.toString(measure) + "\".\r\n"// XSD.float
				+ "  <" + nodeIRI + "> a:relation \"=\".\r\n" + "}";
		return alignmentCellUpdateStr;
	}
}
