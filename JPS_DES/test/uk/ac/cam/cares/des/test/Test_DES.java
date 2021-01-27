package uk.ac.cam.cares.des.test;

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.List;

import org.json.JSONObject;
import org.web3j.crypto.Credentials;
import org.web3j.crypto.WalletUtils;
import org.web3j.protocol.Web3j;
import org.web3j.protocol.core.DefaultBlockParameterName;
import org.web3j.protocol.core.methods.response.EthGetBalance;
import org.web3j.protocol.core.methods.response.TransactionReceipt;
import org.web3j.protocol.core.methods.response.Web3ClientVersion;
import org.web3j.protocol.http.HttpService;
import org.web3j.tx.Transfer;
import org.web3j.utils.Convert;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.annotate.MetaDataQuery;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.des.BlockchainWrapper;
import uk.ac.cam.cares.jps.des.FrontEndCoordination;
import uk.ac.cam.cares.jps.des.WeatherIrradiationRetriever;


public class Test_DES extends TestCase{
	
	private static String ENIRI="http://www.theworldavatar.com/kb/sgp/singapore/singaporeelectricalnetwork/SingaporeElectricalNetwork.owl#SingaporeElectricalNetwork";
	private String DISIRI="http://www.theworldavatar.com/kb/sgp/singapore/District-001.owl#District-001";
	
	/**
	 * Periodic call to run the (Forecast+DESpython wrapper)
	 * Every four hours, so six calls in a day this would be called
	 * 
	 */
	public void testStartCoordinationDESScenariobase() throws IOException  {
		

		JSONObject jo = new JSONObject();
	
		jo.put("electricalnetwork", ENIRI);
		jo.put("district", DISIRI);
		jo.put("temperaturesensor", "http://www.theworldavatar.com/kb/sgp/singapore/SGTemperatureSensor-001.owl#SGTemperatureSensor-001");
    	jo.put("irradiationsensor","http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationSensor-001.owl#SGSolarIrradiationSensor-001");
    	jo.put("windspeedsensor","http://www.theworldavatar.com/kb/sgp/singapore/SGWindSpeedSensor-001.owl#SGWindSpeedSensor-001");
		
		System.out.println(jo.toString());
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_DES/DESCoordination", jo.toString());
		System.out.println(resultStart);
		System.out.println("finished execute");

	}
	
	/**
	 * Calls upon the FrontEnd Coordination agent that would call the latest DES run (Forecast+DESpython wrapper)
	 * And afterwards blockchain wrapper
	 */
	public void testStartDESScenariobaseshowingresult() throws IOException  { //must have at least 1 directory with complete running first to make it success
		

		JSONObject jo = new JSONObject();
	
		jo.put("electricalnetwork", ENIRI);
		jo.put("district", DISIRI);
		
		System.out.println(jo.toString());
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_DES/showDESResult", jo.toString());
		System.out.println(resultStart);
		System.out.println("finished execute");

	}
	/**
	 * Calls and runs the Blockchain transaction with test values
	 */
	public void testBlockchainWrapperDirectCall() throws IOException{
		JSONObject jo = new JSONObject();
		jo.put("industrial", "2.311116263469459966e+01");
		jo.put("commercial", "5.000000000000000000e+01");
		jo.put("residential","8.826121920185781278e+00");
		jo.put("gridsupply","4.409266691007480290e+01");
		jo.put("solar","3.784461764480557235e+01");
		System.out.println(new BlockchainWrapper().calculateTrade(jo));
	}
	/** calls the last modified directory linked with Service__DES
	 * 
	 */
	public String getLastModifiedDirectory() {
    	String agentiri = "http://www.theworldavatar.com/kb/agents/Service__DESAgent.owl#Service";
		List<String> lst = null;
    	System.out.println(lst);
    	String resultfromfuseki = MetaDataQuery.queryResources(null,null,null,agentiri, null, null,null,lst);
		 String[] keys = JenaResultSetFormatter.getKeys(resultfromfuseki);
		 List<String[]> listmap = JenaResultSetFormatter.convertToListofStringArrays(resultfromfuseki, keys);
    	return listmap.get(0)[0];
    }
	/**
	 * Calls and runs the Blockchain transaction with test values (thru TOMCAT)
	 */
	public void testBlockchainWrapperAgentCall() throws IOException{
		JSONObject jo = new JSONObject();
		jo.put("industrial", "2.311116263469459966e+01");
		jo.put("commercial", "5.000000000000000000e+01");
		jo.put("residential","8.826121920185781278e+00");
		jo.put("gridsupply","4.409266691007480290e+01");
		jo.put("solar","3.784461764480557235e+01");
		jo.put("directory",getLastModifiedDirectory());
	    System.out.println(jo.toString());
		String v = AgentCaller.executeGetWithJsonParameter("JPS_DES/GetBlock", jo.toString());
		System.out.println(v);
	}
	

	/**
	 * Calls and runs the hourly weather retriever, that uses OCR
	 */
	public void testWeatherIrradiationDirect() {
		String baseUrl = "C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\DESTest\\solar2";
		String irioftempS="http://www.theworldavatar.com/kb/sgp/singapore/SGTemperatureSensor-001.owl#SGTemperatureSensor-001";
	    String iriofirrS="http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationSensor-001.owl#SGSolarIrradiationSensor-001";
	    String iriofwindS="http://www.theworldavatar.com/kb/sgp/singapore/SGWindSpeedSensor-001.owl#SGWindSpeedSensor-001";
	    
		try {
			WeatherIrradiationRetriever.readWritedatatoOWL(baseUrl,irioftempS,iriofirrS,iriofwindS);
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	/**
	 * Calls and runs the hourly weather retriever, that uses OCR (thru TOMCAT)
	 */
	public void testIrradiationRetreiverAgentCall() throws Exception {
		JSONObject jo = new JSONObject();
		
		jo.put("folder", QueryBroker.getLocalDataPath()+"/JPS_DES");
		jo.put("tempsensor", "http://www.theworldavatar.com/kb/sgp/singapore/SGTemperatureSensor-001.owl#SGTemperatureSensor-001");
		jo.put("speedsensor", "http://www.theworldavatar.com/kb/sgp/singapore/SGWindSpeedSensor-001.owl#SGWindSpeedSensor-001");
		jo.put("irradiationsensor", "http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationSensor-001.owl#SGSolarIrradiationSensor-001");
		jo.put("jpscontext", "base");

		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_DES/GetIrradiationandWeatherData", jo.toString());
		System.out.println(resultStart);
	}

	/**
	 * Finds the latest directory, as part of the coordinate agent. 
	 */
	public void testfindlatestdirectory() {
		 String dir="C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\base\\localhost_8080\\data";
		 File baseUrl=new File(dir);
		System.out.println("date latest directory= "+ new FrontEndCoordination().getLastModifiedDirectory());
	}
	/** test BlockchainWrapper directly
	 * 
	 * @throws IOException
	 */
	public static void testBlockchainInteraction() throws IOException {
		Web3j web3 = Web3j.build(new HttpService("https://rinkeby.infura.io/v3/1f23f6038dde496ea158547e3ba1e76b"));
		Web3ClientVersion web3ClientVersion = web3.web3ClientVersion().send();
		String clientVersion = web3ClientVersion.getWeb3ClientVersion();
		System.out.println("Connected to Ethereum Client Version: " + clientVersion);
		try {
			//Get balance
			EthGetBalance ethGetBalance=web3.ethGetBalance("0x1eD35d5845F8162B40df26c34562cFabd4892017", DefaultBlockParameterName.LATEST).sendAsync().get();
			java.math.BigInteger wei = ethGetBalance.getBalance();
			System.out.println(wei);
			Credentials credentials = WalletUtils.loadCredentials("Caesar1!", "C:\\Users\\LONG01\\TOMCAT\\webapps\\JPS_DES##1.0.0\\resources\\residential.json");
			TransactionReceipt transactionReceipt = Transfer.sendFunds(
			        web3, credentials, "0x9e64A50EfA603BCD127001b689635fca4669ba9d",
			        BigDecimal.valueOf(1.0), Convert.Unit.ETHER).send();
			System.out.println(transactionReceipt);
		}catch (Exception ex) {
			System.out.println(ex);
		}
	
	}

	
	
}
