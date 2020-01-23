package uk.ac.cam.cares.jps.des;

import java.io.IOException;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONArray;
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

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
@WebServlet(urlPatterns = {"/GetBlock" })
public class BlockchainWrapper extends JPSHttpServlet{
	private static String ElectricPublicKey = "0xCB37bDCAfb98463d5bfB573781f022Cd1D2EDB81";
	private static String SolarPublicKey = "0xAf70f1C1D6B1c0C28cbDCa6b49217Aa6FA17b6A8";
	private static String addrOfI = "0x6708a3A3D9CA9624D1C26Ce2a033f7b332a78dde";
	private static String addrOfC = "0xF7312b19628D3B862596DbCBCAC090135555a4aD";
	private static String addrOfR = "0x1eD35d5845F8162B40df26c34562cFabd4892017";
	private static final long serialVersionUID = 1L;
	protected void doGetJPS(HttpServletRequest request, HttpServletResponse res) {
		JSONObject jo = AgentCaller.readJsonParameter(request);

		String baseUrl = jo.optString("baseUrl",  QueryBroker.getLocalDataPath()+"/JPS_DES");
		
		JSONObject result=new JSONObject();
		try {
			calculateTrade(baseUrl, jo);
		
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		AgentCaller.printToResponse(result, res);
 
		logger.info("return the result from forecast agent");		
	}
	public static String dotransact(String sender, String recipient, double moneyEth) throws IOException {
		Web3j web3 = Web3j.build(new HttpService("https://rinkeby.infura.io/v3/1f23f6038dde496ea158547e3ba1e76b"));
		Web3ClientVersion web3ClientVersion = web3.web3ClientVersion().send();
		//use Transfer class to send ether
		Credentials credentials = WalletUtils.loadCredentials("Caesar1!", "resources/keyfile"); //password
		TransactionReceipt transactionReceipt = Transfer.sendFunds(web3,  credentials, recipient , moneyEth, Convert.Unit.ETHER).send();
		return "";
		
	}
	public static void calculateTrade(String baseUrl, JSONObject jo) {
		JSONArray solar = (JSONArray) jo.get("solar");
		double totalsolar = solar.getDouble(0);
		JSONArray elecgrid = (JSONArray) jo.get("gridsupply");
		double totalelectric = elecgrid.getDouble(0);
		JSONArray indus = (JSONArray) jo.get("industrial");
		double totalindus = indus.getDouble(0);
		JSONArray resid = (JSONArray) jo.get("residential");
		double totalresid = resid.getDouble(0);
		JSONArray commerce = (JSONArray) jo.get("commercial");
		double totalcommer = commerce.getDouble(0);
		try {
		if (totalsolar == 0) {
			//if no electricity is generated from the solar powered electricity:
			//give nominal sum -> Not precisely true because the amount of ether that they need to pay is 220 eth per kwh which no one has
			double ethIndus = totalindus*0.0005;
			String transactionhash1 = dotransact(addrOfI, ElectricPublicKey,ethIndus);
			double ethComme = totalcommer*0.0005;
			String transactionhash2 = dotransact(addrOfC, ElectricPublicKey,ethComme);
			double ethResid = totalcommer*0.0005;
			String transactionhash3 = dotransact(addrOfR, ElectricPublicKey,ethResid);
		}else {
			//when solar is available, get solar
			//again give nominal sum since we don't have enough ether to go around yet. 
			if (totalindus < totalsolar) {
				double ethIndus = totalindus*0.0004;
				String transactionhashs = dotransact(addrOfI, SolarPublicKey,ethIndus);
				totalsolar -= totalindus;
				if (totalresid < totalsolar) {
					double ethResid = totalresid*0.0004;
					String transacthashs2 = dotransact(addrOfC, SolarPublicKey,ethResid);
					totalsolar -= totalresid;
					
					if (totalcommer < totalsolar) {
						double ethComme = totalcommer*0.0004;
						String transacthashs3 = dotransact(addrOfC, SolarPublicKey,ethComme);
						totalsolar -= totalcommer;
						//this should only occur once electric grid  is negative. 
						if (totalelectric < 0 ) {
							//electric should buy solar
							double ethElectric = totalelectric *-0.0003;
							String transacthashs4 = dotransact(ElectricPublicKey, SolarPublicKey,ethElectric);
							
						}
					}else {
						//
						double ethComme = totalcommer*0.0004;
						String transacthashs3 = dotransact(addrOfC, SolarPublicKey,ethComme);
						totalcommer -= totalsolar;
						ethComme = totalcommer*0.0005;
						String transactionhash2 = dotransact(addrOfC, ElectricPublicKey,ethComme);
						
					}
				}else {
					double ethResid = totalresid*0.0004;
					String transactionhashs2 = dotransact(addrOfC, SolarPublicKey,ethResid);
					totalresid -= totalsolar;
					
					ethResid = totalresid*0.0005;
					String transactionhash3 = dotransact(addrOfR, ElectricPublicKey,ethResid);
					
					double ethComme = totalcommer*0.0005;
					String transactionhash2 = dotransact(addrOfC, ElectricPublicKey,ethComme);
				}
			}else {
				double ethIndus = totalindus*0.0004;
				String transactionhashs = dotransact(addrOfI, SolarPublicKey,ethIndus);
				totalindus -= totalsolar;
				ethIndus = totalindus*0.0005;
				String transactionhash1 = dotransact(addrOfI, ElectricPublicKey,ethIndus);
				double ethComme = totalcommer*0.0005;
				String transactionhash2 = dotransact(addrOfC, ElectricPublicKey,ethComme);
				double ethResid = totalresid*0.0005;
				String transactionhash3 = dotransact(addrOfR, ElectricPublicKey,ethResid);
				
			}
			
		}
	}catch (Exception e) {
			e.printStackTrace();
	}
		
	}
	public static void main(String[] args) throws IOException {
		Web3j web3 = Web3j.build(new HttpService("https://rinkeby.infura.io/v3/1f23f6038dde496ea158547e3ba1e76b"));
		Web3ClientVersion web3ClientVersion = web3.web3ClientVersion().send();
		String clientVersion = web3ClientVersion.getWeb3ClientVersion();
		System.out.println("Connected to Ethereum Client Version: " + clientVersion);
		try {
			//Get abalance
			EthGetBalance ethGetBalance=web3.ethGetBalance("0x6708a3A3D9CA9624D1C26Ce2a033f7b332a78dde", DefaultBlockParameterName.LATEST).sendAsync().get();
			java.math.BigInteger wei = ethGetBalance.getBalance();
			System.out.println(wei);
//			String pk = "04dab771c776d8345c8877a70f26c03a3bd7927abbc65ceff14e74ee23ab0fe8"; //private key of industrial
//		   Credentials credentials = Credentials.create(pk);
		}catch (Exception ex) {
			System.out.println(ex);
		}
	
	}
}
