package uk.ac.cam.cares.jps.des;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.MathContext;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

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

import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
@WebServlet(urlPatterns = {"/GetBlock" })
public class BlockchainWrapper extends JPSHttpServlet{
	private static String ElectricPublicKey = "0xCB37bDCAfb98463d5bfB573781f022Cd1D2EDB81";
	private static String SolarPublicKey = "0xAf70f1C1D6B1c0C28cbDCa6b49217Aa6FA17b6A8";
	private static String addrOfI = "industrial.json";
	private static String addrOfC = "commercial.json";
	private static String addrOfR = "residential.json";
	private static final long serialVersionUID = 1L;
	protected JSONObject processRequestParameters(JSONObject requestParams,HttpServletRequest request) {

		System.out.println(requestParams.toString());
		JSONObject result=new JSONObject();
		try {
			result = calculateTrade(requestParams);
			System.out.println(result);
		
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return result;
 
	}
	public static String dotransact(String sender, String recipient, double moneyEth) throws IOException, Exception {
		Web3j web3 = Web3j.build(new HttpService("https://rinkeby.infura.io/v3/1f23f6038dde496ea158547e3ba1e76b"));
		Web3ClientVersion web3ClientVersion = web3.web3ClientVersion().send();
		//use Transfer class to send ether
		//check value of moneyEth. if moneyEth is too small, there's a UnsupportedOperationException error thrown. 
		if (moneyEth < 1E-12) {
			return "Value too small, transaction not completed";
		}
		Credentials credentials = WalletUtils.loadCredentials("Caesar1!", "C:\\TOMCAT\\webapps\\JPS_DES##1.0.0\\resources\\"+sender); //password
		TransactionReceipt transactionReceipt = Transfer.sendFunds(web3,  credentials, recipient , new BigDecimal(moneyEth, MathContext.DECIMAL64), Convert.Unit.SZABO).send();
		return  transactionReceipt.getTransactionHash();
		
	}
	public static JSONObject calculateTrade(JSONObject jo) {
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
	public static void main(String[] args) throws IOException {
		Web3j web3 = Web3j.build(new HttpService("https://rinkeby.infura.io/v3/1f23f6038dde496ea158547e3ba1e76b"));
		Web3ClientVersion web3ClientVersion = web3.web3ClientVersion().send();
		String clientVersion = web3ClientVersion.getWeb3ClientVersion();
		System.out.println("Connected to Ethereum Client Version: " + clientVersion);
		try {
			//Get abalance
			EthGetBalance ethGetBalance=web3.ethGetBalance("0x1eD35d5845F8162B40df26c34562cFabd4892017", DefaultBlockParameterName.LATEST).sendAsync().get();
			java.math.BigInteger wei = ethGetBalance.getBalance();
			System.out.println(wei);
			Credentials credentials = WalletUtils.loadCredentials("Caesar1!", "C:\\Users\\LONG01\\TOMCAT\\webapps\\JPS_DES##1.0.0\\resources\\residential.json");
			TransactionReceipt transactionReceipt = Transfer.sendFunds(
			        web3, credentials, "0x9e64A50EfA603BCD127001b689635fca4669ba9d",
			        BigDecimal.valueOf(1.0), Convert.Unit.ETHER).send();
			System.out.println(transactionReceipt);
//			String pk = "04dab771c776d8345c8877a70f26c03a3bd7927abbc65ceff14e74ee23ab0fe8"; //private key of industrial
//		   Credentials credentials = Credentials.create(pk);
		}catch (Exception ex) {
			System.out.println(ex);
		}
	
	}
}
