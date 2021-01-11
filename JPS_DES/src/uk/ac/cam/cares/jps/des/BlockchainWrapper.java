package uk.ac.cam.cares.jps.des;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.MathContext;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.web3j.crypto.Credentials;
import org.web3j.crypto.WalletUtils;
import org.web3j.protocol.Web3j;
import org.web3j.protocol.core.methods.response.TransactionReceipt;
import org.web3j.protocol.core.methods.response.Web3ClientVersion;
import org.web3j.protocol.http.HttpService;
import org.web3j.tx.Transfer;
import org.web3j.utils.Convert;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;
@WebServlet(urlPatterns = {"/GetBlock" })
public class BlockchainWrapper extends JPSHttpServlet{
	private static String ElectricPublicKey = "0xCB37bDCAfb98463d5bfB573781f022Cd1D2EDB81";
	private static String SolarPublicKey = "0xAf70f1C1D6B1c0C28cbDCa6b49217Aa6FA17b6A8";
	private static String addrOfI = "industrial.json";
	private static String addrOfC = "commercial.json";
	private static String addrOfR = "residential.json";
	private static final long serialVersionUID = 1L;
	protected JSONObject processRequestParameters(JSONObject requestParams,HttpServletRequest request) {

		JSONObject result=new JSONObject();
		JSONObject graData =new JSONObject();
		graData = provideJSONResult(requestParams.getString("directory"));
		JSONObject jo = determineValue (graData);
		try {
			result = calculateTrade(jo);
			graData.put("txHash", result.get("txHash"));
			graData.put("sandr", result.get("sandr"));
			
		
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return graData;
 
	}
	/** creates transaction between sender and receiver. 
	 * 
	 * @param sender String address of Ethereum account
	 * @param recipient String address of Ethereum account
	 * @param moneyEth double ether to be transacted. 
	 * @return
	 * @throws IOException
	 * @throws Exception
	 */
	public String dotransact(String sender, String recipient, double moneyEth) throws IOException, Exception {
		Web3j web3 = Web3j.build(new HttpService("https://rinkeby.infura.io/v3/1f23f6038dde496ea158547e3ba1e76b"));
		Web3ClientVersion web3ClientVersion = web3.web3ClientVersion().send();
		//use Transfer class to send ether
		//check value of moneyEth. if moneyEth is too small, there's a UnsupportedOperationException error thrown. 
		if (moneyEth < 0) {
			System.out.println("Value too small, transaction not completed");
			moneyEth = 0;
		}else if (moneyEth < Math.pow(10, -1)) {
			moneyEth = 0;
		}
		Credentials credentials = WalletUtils.loadCredentials("Caesar1!",AgentLocator.getCurrentJpsAppDirectory(this) + "\\resources\\"+sender); //password
		TransactionReceipt transactionReceipt = Transfer.sendFunds(web3,  credentials, recipient , new BigDecimal(moneyEth, MathContext.DECIMAL64), Convert.Unit.SZABO).send();
		return  transactionReceipt.getTransactionHash();
		
	}
    
	/**
	 * Derive and estimate the values to be transacted over the blockchain, to the closest half an hour. 
     *
	 * @param basefold
	 * @return
	 * @throws JSONException
	 */
    public JSONObject determineValue (JSONObject basefold) throws JSONException {

		JSONObject jo = new JSONObject();
    	try {
		SimpleDateFormat sdf = new SimpleDateFormat("HH:mm");
		String date = sdf.format(new Date());
		Date date2 = sdf.parse(date);
		//figure out time in which 
		String[] tim = (String[]) basefold.get("timer");
		for (int i = 0; i< tim.length; i++) {
			Date date1;
				date1 = sdf.parse(tim[i]);
				// TODO Auto-generated catch block
			long difference = date2.getTime() - date1.getTime();
			difference = difference/60000;
			if (difference < 29) {
				//need to figure out the difference gradient
				jo = deriveValue(i, false, basefold);
				break;
			}
			else if (difference < 60){
				//get the next one. 
				jo = deriveValue(i, true, basefold);
				break;
			}
		}
    	
    	}catch (Exception ex) {
    		ex.printStackTrace();
    	}

		return jo;
    }
	/**
     * helper function to determineValue()s
	 * 
	 * @param index
	 * @param inbetween
	 * @param basefold
	 * @return
	 */
    public static JSONObject deriveValue(int index, boolean inbetween, JSONObject basefold) {

    	JSONObject jo = new JSONObject();
    	String[] types = {"solar", "gridsupply", "industrial", "commercial", "residential"};
    	List<String> l = Arrays.asList(types);
    	if (inbetween) {
    		for (String i: l ) {
    			String[] j =  (String[]) basefold.get(i );
    			jo.put(i,j[index +1]);
    		}
        }else {
        	for (String i: l ) {
        		String[] j  =  (String[]) basefold.get(i);
    			double getAvg = ( Double.parseDouble(j[index]) + Double.parseDouble(j[index+1])) /2;
    			jo.put(i,Double.toString(getAvg));
    		}
        }
        return jo;
    }
	public JSONObject calculateTrade(JSONObject jo) {
		double totalsolar = Double.parseDouble((String) jo.get("solar"));
		double totalelectric =Double.parseDouble((String) jo.get("gridsupply"));
		double totalindus = Double.parseDouble((String)jo.get("industrial"));
		double totalresid = Double.parseDouble((String) jo.get("residential"));
		double totalcommer = Double.parseDouble((String) jo.get("commercial"));
		List<String> totalList = new ArrayList<String>();
		List<String> whoTowho = new ArrayList<String>();
		JSONObject jS = new JSONObject();
		try {
		if (totalsolar == 0) {
			//if no electricity is generated from the solar powered electricity:
			//give nominal sum -> Not precisely true because the amount of ether that they need to pay is 220 eth per kwh which no one has so downgrade
			double ethIndus = totalindus*220;
			String transactionhash1 = dotransact(addrOfI, ElectricPublicKey,ethIndus);
			double ethComme = totalcommer*220;
			String transactionhash2 = dotransact(addrOfC, ElectricPublicKey,ethComme);
			double ethResid = totalcommer*220;
			String transactionhash3 = dotransact(addrOfR, ElectricPublicKey,ethResid);
			totalList.add(transactionhash1);
			whoTowho.add("Industrial to Grid");
			totalList.add(transactionhash2);
			whoTowho.add("Commercial to Grid");
			totalList.add(transactionhash3);
			whoTowho.add("Residential to Grid");
		}else {
			//when solar is available, get solar
			//again give nominal sum since we don't have enough ether to go around yet. 
			if (totalindus < totalsolar) {
				double ethIndus = totalindus*136.36;
				String transacthash1 = dotransact(addrOfI, SolarPublicKey,ethIndus);
				whoTowho.add("Industrial to Solar");
				totalsolar -= totalindus;

				totalList.add(transacthash1);
				if (totalresid < totalsolar) {
					double ethResid = totalresid*136.36;
					String transacthash2 = dotransact(addrOfR, SolarPublicKey,ethResid);
					whoTowho.add("Residential to Solar");
					totalsolar -= totalresid;
					totalList.add(transacthash2);
					if (totalcommer < totalsolar) {
						double ethComme = totalcommer*136.36;
						String transacthash3 = dotransact(addrOfC, SolarPublicKey,ethComme);
						whoTowho.add("Commercial to Solar");
						totalsolar -= totalcommer;
						totalList.add(transacthash3);
						//this should only occur once electric grid  is negative. 
						if (totalelectric < 0 ) {
							//electric should buy solar
							double ethElectric = totalelectric *100;
							String transacthash4 = dotransact(ElectricPublicKey, SolarPublicKey,ethElectric);
							whoTowho.add("Grid to Solar");
							totalList.add(transacthash4);
													
						}
					}else {
						//
						double ethComme = totalcommer*136.36;
						String transacthashs3 = dotransact(addrOfC, SolarPublicKey,ethComme);
						whoTowho.add("Commercial to Solar");
						totalList.add(transacthashs3);
						totalcommer -= totalsolar;
						ethComme = totalcommer*220;
						String transactionhash2 = dotransact(addrOfC, ElectricPublicKey,ethComme);
						whoTowho.add("Commercial to Grid");
						totalList.add(transactionhash2);
						
					}
				}else {
					double ethResid = totalresid*136.36;
					String transactionhashs2 = dotransact(addrOfC, SolarPublicKey,ethResid);
					whoTowho.add("Residential to Solar");
					totalresid -= totalsolar;
					totalList.add(transactionhashs2);
					
					ethResid = totalresid*220;
					String transactionhash3 = dotransact(addrOfR, ElectricPublicKey,ethResid);
					whoTowho.add("Residential to Grid");
					totalList.add(transactionhash3);
					
					double ethComme = totalcommer*220;
					String transactionhash2 = dotransact(addrOfC, ElectricPublicKey,ethComme);
					whoTowho.add("Commercial to Grid");
					totalList.add(transactionhash2);
				}
			}else {
				double ethIndus = totalindus*136.36;
				String transactionhashs = dotransact(addrOfI, SolarPublicKey,ethIndus);
				whoTowho.add("Industrial to Solar");
				totalList.add(transactionhashs);
				totalindus -= totalsolar;
				ethIndus = totalindus*220;
				String transactionhash1 = dotransact(addrOfI, ElectricPublicKey,ethIndus);
				whoTowho.add("Industrial to Grid");
				totalList.add(transactionhash1);
				double ethComme = totalcommer*220;
				String transactionhash2 = dotransact(addrOfC, ElectricPublicKey,ethComme);
				whoTowho.add("Commercial to Grid");
				totalList.add(transactionhash2);
				double ethResid = totalresid*220;
				String transactionhash3 = dotransact(addrOfR, ElectricPublicKey,ethResid);
				whoTowho.add("Residential to Grid");
				totalList.add(transactionhash3);
				
			}
			jS.put("txHash",totalList.toArray());
			jS.put("sandr",whoTowho.toArray());
		}
	}catch (Exception e) {
			e.printStackTrace();
	}

		return jS;
	}
	/** provides result in the response of a JSON form
	 * 
	 * @param baseUrl
	 * @return
	 */
	public JSONObject provideJSONResult(String baseUrl) {
		String weatherdir = baseUrl + "/WeatherForecast.csv";
		String content = new QueryBroker().readFileLocal(weatherdir);
		List<String[]> weatherResult = MatrixConverter.fromCsvToArray(content);

		String powerdir = baseUrl + "/totgen.csv";
		String content2 = new QueryBroker().readFileLocal(powerdir);
		List<String[]> simulationResult = MatrixConverter.fromCsvToArray(content2);
		JSONObject dataresult = new JSONObject();

		String rhdir = baseUrl + "/rh1.csv";
		String content3 = new QueryBroker().readFileLocal(rhdir);
		List<String[]> rhResult = MatrixConverter.fromCsvToArray(content3);
		
		String timer = baseUrl + "/timer.csv";
		String content4 = new QueryBroker().readFileLocal(timer);
		List<String[]> timerResult = MatrixConverter.fromCsvToArray(content4);
		
		JSONArray temperature = new JSONArray();
		JSONArray irradiation = new JSONArray();

		int sizeofweather = weatherResult.size();
		for (int x = 0; x < sizeofweather; x++) {
			temperature.put(weatherResult.get(x)[0]);
			irradiation.put(weatherResult.get(x)[1]);
		}

		// log to check if it's reading the right one. x

		dataresult.put("temperature", temperature);
		dataresult.put("irradiation", irradiation);
		dataresult.put("gridsupply", simulationResult.get(4));
		dataresult.put("solar", simulationResult.get(3));
		dataresult.put("residential", simulationResult.get(0));
		dataresult.put("industrial", simulationResult.get(2));
		dataresult.put("commercial", simulationResult.get(1));
		dataresult.put("timer",timerResult.get(0));
		dataresult.put("rh1", rhResult.subList(0, 3).toArray());
		dataresult.put("rh2", rhResult.subList(3, 6).toArray());
		dataresult.put("rh3", rhResult.subList(6, rhResult.size()).toArray());

		return dataresult;
	}
	
}
