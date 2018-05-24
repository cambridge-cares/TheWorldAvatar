package uk.ac.cam.cares.jps.men.test;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

//import org.osgeo.proj4j.BasicCoordinateTransform;
//import org.osgeo.proj4j.CRSFactory;
//import org.osgeo.proj4j.CoordinateReferenceSystem;
//import org.osgeo.proj4j.ProjCoordinate;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.men.MenDataProvider;
import uk.ac.cam.cares.jps.men.MenGamsConverter;
import uk.ac.cam.cares.jps.men.MenResult;
import uk.ac.cam.cares.jps.men.entity.FeasibleConnection;
import uk.ac.cam.cares.jps.men.entity.MenCalculationParameters;
import uk.ac.cam.cares.jps.men.entity.Sink;
import uk.ac.cam.cares.jps.men.entity.Source;
import uk.ac.cam.cares.jps.men.entity.Transportation;

public class TestMenDataProvider extends TestCase {
	List<String> plantkb= new ArrayList<String>();
	String Transport_OKB= getTransportationFile();
	
	// based on the Jr_MEN OKB
	String nameOfResourceNetwork = "Jurong_MEN";                               //define the name of the resource network that is to be synthesized 

	public static Map<String, String> NametoCode = new HashMap<>();
	public static Map<String, String> NametoCode2 = new HashMap<>();
	
	private String getTransportationFile() {
		return ".\\testres\\transportation\\Jr_Transportation_simplified.owl"; // location of the owl file that contains information for the transportation system
		
	}
	
	public List<String> getsourcedirectory() {
		File[] files = new File(".\\testres\\chemicalplant").listFiles();
		// If this pathname does not denote a directory, then listFiles() returns null.

		for (File file : files) {
			if (file.isFile() && !file.getName().equals("catalog-v001.xml")) {
				plantkb.add(".\\testres\\chemicalplant\\" + file.getName());
			}
		}
		return plantkb;
	}
	
	public TestMenDataProvider()
	
	{
		
	NametoCode.put("EllbaEasternPteLtd, product=PropyleneOxide", "r2");
	NametoCode.put("ExxonMobilChemicalAsiaPacificPteLtd, product=Ethylene", "r3");
	NametoCode.put("ExxonMobilChemicalAsiaPacificPteLtd, product=Benzene", "r4");
	NametoCode.put("InvistaSingaporePteLtd, product=AdipicAcid", "r5");
	NametoCode.put("LuciteInternationalSingaporePteLtd, product=MethylMethacrylateMonomer", "r6");
	NametoCode.put("MitsuiPhenolsSingaporePteLtd, product=Phenol", "r7");
	NametoCode.put("MitsuiPhenolsSingaporePteLtd, product=BisphenolA", "r8");
	NametoCode.put("PetrochemicalCorporationOfSingaporePteLtd, product=Ethylene", "r9");
	NametoCode.put("PetrochemicalCorporationOfSingaporePteLtd, product=Propylene", "r10");
	NametoCode.put("PetrochemicalCorporationOfSingaporePteLtd, product=Butadiene", "r11");
	NametoCode.put("ShellChemicalsSerayaPteLtd, product=PropyleneOxide", "r12");
	NametoCode.put("SingaporeAcrylicPteLtd, product=AcrylicAcid", "r13");
	NametoCode.put("SingaporeMethylMethylacrylatePteLtd, product=MethylMethacrylateMonomer", "r14");
	NametoCode.put("SumitomoChemicalSingaporePteLtd, product=MethylMethacrylateMonomer", "r15");
	NametoCode.put("JurongAromaticsCorporationPteLtd, product=Benzene", "r16");
	NametoCode.put("ShellEasternPetroleumPteLtd, product=EthyleneOxide", "r17");
	NametoCode.put("EastmanChemicalSingaporePteLtd, product=Oxo-Alcohols", "r18");
	
	
	NametoCode2.put("AsahiKaseiPlasticsSingaporePteLd, raw material=Phenol", "d1");
	NametoCode2.put("CelaneseSingaporePteLtd, raw material=Ethylene", "d2");
	NametoCode2.put("ChevronPhillipsSingaporeChemicalsPteLtd, raw material=Ethylene", "d3");
	NametoCode2.put("DICAlkyphenolSingaporePteLtd, raw material=Phenol", "d4");
	NametoCode2.put("DuPontCompanySingaporePteLtd, raw material=AdipicAcid", "d5");
	NametoCode2.put("EastmanChemicalSingaporePteLtd, raw material=Propylene", "d6");
	NametoCode2.put("EllbaEasternPteLtd, raw material=Benzene", "d7");
	NametoCode2.put("EllbaEasternPteLtd, raw material=Propylene", "d8");
	NametoCode2.put("HuntsmanSingaporePteLtd, raw material=PropyleneOxide", "d9");
	NametoCode2.put("LuciteInternationalSingaporePteLtd, raw material=Ethylene", "d10");
	NametoCode2.put("MitsuiPhenolsSingaporePteLtd, raw material=Benzene", "d11");
	NametoCode2.put("MitsuiPhenolsSingaporePteLtd, raw material=Propylene", "d12");
	NametoCode2.put("ShellChemicalsSerayaPteLtd, raw material=Benzene", "d13");
	NametoCode2.put("ShellChemicalsSerayaPteLtd, raw material=Propylene", "d14");
	NametoCode2.put("SingaporeAcrylicPteLtd, raw material=Propylene", "d15");
	NametoCode2.put("SingaporeGlacialAcrylicPteltd, raw material=Propylene", "d16");
	NametoCode2.put("TeijinPolycarbonateSingaporePteLtd, raw material=BisphenolA", "d17");
	NametoCode2.put("TeijinPolycarbonateSingaporePteLtd, raw material=AcrylicAcid", "d18");
	NametoCode2.put("AsahiKaseiSyntheticRubberSingaporePteLtd, raw material=Butadiene", "d19");
	NametoCode2.put("CCDSingaporePteLtd, raw material=Ethylene", "d20");
	NametoCode2.put("RohmAndHaasChemicalsSingaporePteLtd, raw material=MethylMethacrylateMonomer", "d21");
	NametoCode2.put("SumitomoChemicalAsiaPteLtd, raw material=Butadiene", "d22");
	NametoCode2.put("ShellEasternPetroleumPteLtd, raw material=EthyleneOxide", "d23");
	NametoCode2.put("ShellEasternPetroleumPteLtd, raw material=PropyleneOxide", "d24");
	NametoCode2.put("HuntsmanSingaporePteLtd, raw material=Oxo-Alcohols", "d25");
	NametoCode2.put("ShellMEG, raw material=Oxo-Alcohols", "d26");
	}

	
	public void mapper (){
		getsourcedirectory();
		MenDataProvider provider= new MenDataProvider();
		int numberofsourcedata = provider.addSourceDataFromKB(plantkb, nameOfResourceNetwork).size();
		assertEquals(18, numberofsourcedata);
	}

	
	public void test1createsourcewithoutr1d20 (){
		getsourcedirectory();
		MenDataProvider provider= new MenDataProvider();
		List<Sink> sink = provider.addSinkDataFromKB(plantkb, nameOfResourceNetwork);
		List<Source> source = provider.addSourceDataFromKB(plantkb, nameOfResourceNetwork);
		source=provider.puresourcescategorial(source, sink);
		int numberofsourcedata = source.size();
		assertEquals(17, numberofsourcedata);
	}

	public void test2createsink (){
		getsourcedirectory();
		MenDataProvider provider= new MenDataProvider();
		List<Sink> sink = provider.addSinkDataFromKB(plantkb, nameOfResourceNetwork);
		List<Source> source = provider.addSourceDataFromKB(plantkb, nameOfResourceNetwork);
		sink=provider.puresinkscategorial(sink, source);
		int numberofsinkdata = sink.size();
		assertEquals(26, numberofsinkdata);
	}
	
	public void test3connectsourceandsink (){
		getsourcedirectory();
		MenDataProvider provider= new MenDataProvider();
		List<Sink> sink = provider.addSinkDataFromKB(plantkb, nameOfResourceNetwork);
		List<Source> source = provider.addSourceDataFromKB(plantkb, nameOfResourceNetwork);
		sink=provider.puresinkscategorial(sink, source);
		source=provider.puresourcescategorial(source, sink);
		
		int numberofconnectiondata = provider.connectSinkandSourceWithTransportation(sink,source).size();
		assertEquals(37, numberofconnectiondata);
	}

	public void test4transportationmeans (){
		
		MenDataProvider provider= new MenDataProvider();
		int numberoftransportationmeans = provider.getTransportationMeansInfoFromKB(Transport_OKB).size();
		assertEquals(4, numberoftransportationmeans);
	}

	public void test5aPreprint164Withoutr1d20connectionWithOWLfiles() {
		getsourcedirectory();
		MenDataProvider provider= new MenDataProvider();
		List<Sink> sink1 = provider.addSinkDataFromKB(plantkb, nameOfResourceNetwork);
		List<Source> sources1 = provider.addSourceDataFromKB(plantkb, nameOfResourceNetwork);
		sink1=provider.puresinkscategorial(sink1, sources1);
		 sources1 = provider.puresourcescategorial(sources1,sink1);
		List<FeasibleConnection> connections1 = provider.connectSinkandSourceWithTransportation(sink1,sources1);
		List<Transportation> transportations1 = provider.getTransportationMeansInfoFromKB(Transport_OKB);
		
		System.out.println("created " + sources1.size() + " sources");
		System.out.println(sources1);
		System.out.println("created " + sink1.size() + " sinks");
		System.out.println(sink1);
		
		System.out.println("created " + connections1.size() + " connections");
		for (int x=0;x<connections1.size();x++)
		{			
			String tobeprint = connections1.get(x).toString();
			
			FeasibleConnection con =  connections1.get(x);
			con.getSink().getName();
			con.getSink().getProduct().getName();
			
			
			String orig=connections1.get(x).toString();
			//System.out.println ("number= "+x);
			//System.out.println("orig= "+ orig);
			String tmpname=tobeprint.split("name=")[1];
			String companysource=tmpname.split(", nearSea")[0];
			//System.out.println("companysource= "+companysource );
			//System.out.println("companysource= "+NametoCode.get(companysource));
			String companysink = tobeprint.split("name=")[2].split(", nearSea")[0];
			//System.out.println("companysink= "+companysink );
			//System.out.println("companysink= "+NametoCode2.get(companysink));
			tobeprint=tobeprint.replace(companysource, NametoCode.get(companysource));
			tobeprint=tobeprint.replace(companysink, NametoCode2.get(companysink));
			System.out.println("modified= "+ tobeprint);	
		}
	
		
		System.out.println("created " + transportations1.size() + " transportations");
		System.out.println(transportations1);
		
		MenDataProvider converter = new MenDataProvider();
		MenCalculationParameters parameters = new MenCalculationParameters(50., 1., 1.0, 1.05, false);
		MenResult actual = converter.startCalculation(parameters,Transport_OKB,plantkb);
		assertEquals(6.41811655E9, actual.totalMaterialPurchaseCost, 1.);//using derived= 6.41811655E9
		assertEquals(577381.8713, actual.totalTransportationCost, 1.);  //if using derived= 577381.8713
		assertEquals(802.8382587404967, actual.totalCO2Emission, 1.); // in gramm ;if using derived=802.8382587404967
		
		
		
		double[] annualCostFactors = new double[] {1., 0.1, 0.02, 0.013, 0.012, 0.01}; //reflect the 1/years factor 
		// result Array has one line per factor with columns
		// obj value, totalMaterialPurchaseCost, totalMaterialPurchaseCostIntMarket, totalTransportationCost, totalC02Emission, totalInstallationCost
		double[][] expected = new double[][] {
			{5.987268E9, 5.986443E9, 2.244794E9, 771143., 1082.,       0.},
			{6.009149E9, 6.008846E9, 2.715272E9, 131462.,  230., 1595484.},
			{6.008969E9, 6.008846E9, 2.715272E9,  51949.,  127., 3199500.},
			{6.008946E9, 6.008846E9, 2.715272E9,  47820.,  121., 3498120.},
			{6.008942E9, 6.008846E9, 2.715272E9,  38246.,  109., 4308660.},
			{6.008933E9, 6.008846E9, 2.715272E9,  35798.,  106., 4564620.}
		};
		
		MenGamsConverter converter2 = new MenGamsConverter();
		//MenDataProvider converter = new MenDataProvider();
		for (int i=0; i<annualCostFactors.length; i++) {
			double factor = annualCostFactors[i];
			System.out.println("Annual Cost Factor =" + factor);
			//Parameters parameters = new Parameters(50., 1., factor, 1.05, true);
			 actual = converter2.calculate(sources1, sink1, connections1, transportations1, parameters);
			//MenResult actual = converter.startCalculation(parameters);
			//assertMenResult(expected[i], actual);
		}
	}
	
	
	public void testcheckthevaluefromOWLwith1yearcostfactor() {
		getsourcedirectory();
		MenDataProvider converter = new MenDataProvider();
		double factor = 1.0;
		System.out.println("Annual Cost Factor =" + factor);
		MenCalculationParameters parameters = new MenCalculationParameters(50., 1., factor, 1.05, false);
		MenResult actual = converter.startCalculation(parameters,Transport_OKB,plantkb);
		assertEquals(6.418734E9, actual.objValue, 1000.);
		assertEquals(6.41811655E9, actual.totalMaterialPurchaseCost, 1000.);
		assertEquals(2.2852966E9, actual.totalMaterialPurchaseCostInternationalMarket, 1000.);
		assertEquals(577381.87135, actual.totalTransportationCost, 1.);
		assertEquals(802.8382587, actual.totalCO2Emission, 1.); // in gramm
		assertEquals(0, actual.totalInstallationCost, 1.);

	}
	
	public void testcheckthevaluefromOWLwith10yearcostfactor() {
		getsourcedirectory();
		MenDataProvider converter = new MenDataProvider();

		double factor = 0.1;
		System.out.println("Annual Cost Factor =" + factor);
		MenCalculationParameters parameters = new MenCalculationParameters(50., 1., factor, 1.05, false);
		MenResult actual = converter.startCalculation(parameters,Transport_OKB,plantkb);
		// provider.startCalculation();
		assertEquals(6.63704039E9, actual.objValue, 1000.);
		assertEquals(6.636902E9, actual.totalMaterialPurchaseCost, 1000.);
		assertEquals(3.743276E9, actual.totalMaterialPurchaseCostInternationalMarket, 1000.);
		assertEquals(64485.39744, actual.totalTransportationCost, 1.);
		assertEquals(108.33174, actual.totalCO2Emission, 1.); // in gramm
		assertEquals(683435.5091, actual.totalInstallationCost, 1.);

	}
	
	public void testcheckthevaluefromOWLwith50yearcostfactor() {
	
		getsourcedirectory();
		MenDataProvider converter = new MenDataProvider();

		double factor = 0.02;
		System.out.println("Annual Cost Factor =" + factor);
		MenCalculationParameters parameters = new MenCalculationParameters(50., 1., factor, 1.05, false);
		MenResult actual = converter.startCalculation(parameters,Transport_OKB,plantkb);
		// provider.startCalculation();
		assertEquals(6.636958433E9, actual.objValue, 1000.);
		assertEquals(6.636902E9, actual.totalMaterialPurchaseCost, 1000.);
		assertEquals(3.743276E9, actual.totalMaterialPurchaseCostInternationalMarket, 1000.);
		assertEquals(22539.661189, actual.totalTransportationCost, 1.);
		assertEquals(53.7127, actual.totalCO2Emission, 1.); // in gramm
		assertEquals(1552885.9635, actual.totalInstallationCost, 1.);

	}
	
	public void testcheckthevaluefromOWLwith77yearcostfactor() {
		getsourcedirectory();
		MenDataProvider converter = new MenDataProvider();
		double factor = 0.013;
		System.out.println("Annual Cost Factor =" + factor);
		MenCalculationParameters parameters = new MenCalculationParameters(50., 1., factor, 1.05, false);
		MenResult actual = converter.startCalculation(parameters,Transport_OKB,plantkb);
		assertEquals(6.63694733E9, actual.objValue, 1000.);
		assertEquals(6.636902E9, actual.totalMaterialPurchaseCost, 1000.);
		assertEquals(3.743276E9, actual.totalMaterialPurchaseCostInternationalMarket, 1000.);
		assertEquals(20673.953, actual.totalTransportationCost, 1.);
		assertEquals(51.2832, actual.totalCO2Emission, 1.); // in gramm
		assertEquals(1687831.89558, actual.totalInstallationCost, 1.);

	}
	
	public void testcheckthevaluefromOWLwith83yearcostfactor() {
		
		 getsourcedirectory();
		MenDataProvider converter = new MenDataProvider();

		double factor = 0.012;
		System.out.println("Annual Cost Factor =" + factor);
		MenCalculationParameters parameters = new MenCalculationParameters(50., 1., factor, 1.05, false);
		MenResult actual = converter.startCalculation(parameters,Transport_OKB,plantkb);
		assertEquals(6.63694535E9, actual.objValue, 1000.);
		assertEquals(6.636902E9, actual.totalMaterialPurchaseCost, 1000.);
		assertEquals(3.743276E9, actual.totalMaterialPurchaseCostInternationalMarket, 1000.);
		assertEquals(14038.89056, actual.totalTransportationCost, 1.);
		assertEquals(42.64351, actual.totalCO2Emission, 1.); // in gramm
		assertEquals(2249074.098, actual.totalInstallationCost, 1.);

	}
	
	public void testcheckthevaluefromOWLwith100yearcostfactor() {
	
		 getsourcedirectory();
		MenDataProvider converter = new MenDataProvider();
		double factor = 0.01;
		System.out.println("Annual Cost Factor =" + factor);
		MenCalculationParameters parameters = new MenCalculationParameters(50., 1., factor, 1.05, false);
		MenResult actual = converter.startCalculation(parameters,Transport_OKB,plantkb);
	
		assertEquals(6.63694078E9, actual.objValue, 1000.);
		assertEquals(6.636902E9, actual.totalMaterialPurchaseCost, 1000.);
		assertEquals(3.743276E9, actual.totalMaterialPurchaseCostInternationalMarket, 1000.);
		assertEquals(12660.72143, actual.totalTransportationCost, 1.);
		assertEquals(40.84895, actual.totalCO2Emission, 1.); // in gramm
		assertEquals(2393193.269, actual.totalInstallationCost, 1.);

	}

	public void testcheckthevaluefromOWLwith1yearcostfactortruemarketlowestprice() {
		
		 getsourcedirectory();
		MenDataProvider converter = new MenDataProvider();
		double factor = 1.0;
		System.out.println("Annual Cost Factor =" + factor);
		MenCalculationParameters parameters = new MenCalculationParameters(50., 1., factor, 1.05, true);
		MenResult actual = converter.startCalculation(parameters,Transport_OKB,plantkb);
		assertEquals(6.21513591E9, actual.objValue, 1000.);
		assertEquals(6.2147356E9, actual.totalMaterialPurchaseCost, 1000.);
		assertEquals(2.8730357E9, actual.totalMaterialPurchaseCostInternationalMarket, 1000.);
		assertEquals(374167.89, actual.totalTransportationCost, 1.);
		assertEquals(522.40296, actual.totalCO2Emission, 1.); // in gramm
		assertEquals(0, actual.totalInstallationCost, 1.);

	}
	
	public void testcheckthevaluefromOWLwithcarbontaxvariation1() {
		
		getsourcedirectory();
		MenDataProvider converter = new MenDataProvider();

		double factor = 1.0;
		System.out.println("Annual Cost Factor =" + factor);
		MenCalculationParameters parameters = new MenCalculationParameters(70., 1., factor, 1.05, true);
		MenResult actual = converter.startCalculation(parameters,Transport_OKB,plantkb);
		assertEquals(6.21514636E9, actual.objValue, 1000.);
		assertEquals(6.2147356E9, actual.totalMaterialPurchaseCost, 1000.);
		assertEquals(2.8730357E9, actual.totalMaterialPurchaseCostInternationalMarket, 1000.);
		assertEquals(374167.89, actual.totalTransportationCost, 1.);
		assertEquals(522.40296, actual.totalCO2Emission, 1.); // in gramm
		assertEquals(0, actual.totalInstallationCost, 1.);

	}
	
	public void testcheckthevaluefromOWLwithinterestfactorvariation2() {

		getsourcedirectory();

		MenDataProvider converter = new MenDataProvider();

		double factor = 1.0;
		System.out.println("Annual Cost Factor =" + factor);
		MenCalculationParameters parameters = new MenCalculationParameters(50., 1.3, factor, 1.05, true);
		MenResult actual = converter.startCalculation(parameters,Transport_OKB,plantkb);
		assertEquals(8.079676E9, actual.objValue, 1000.);
		assertEquals(6.2147356E9, actual.totalMaterialPurchaseCost, 1000.);
		assertEquals(2.8730357E9, actual.totalMaterialPurchaseCostInternationalMarket, 1000.);
		assertEquals(374167.89, actual.totalTransportationCost, 1.);
		assertEquals(522.40296, actual.totalCO2Emission, 1.); // in gramm
		assertEquals(0, actual.totalInstallationCost, 1.);

	}
	
	public void testcheckthevaluefromOWLwithinternationalmarketpricefactorvariation3() {
		getsourcedirectory();
		MenDataProvider converter = new MenDataProvider();
		double factor = 1.0;
		System.out.println("Annual Cost Factor =" + factor);
		MenCalculationParameters parameters = new MenCalculationParameters(50., 1., factor, 1.4, true);
		MenResult actual = converter.startCalculation(parameters,Transport_OKB,plantkb);
		assertEquals(7.133475E9, actual.objValue, 1000.);
		assertEquals(7.132858E9, actual.totalMaterialPurchaseCost, 1000.);
		assertEquals(3.0000383E9, actual.totalMaterialPurchaseCostInternationalMarket, 1000.);
		assertEquals(577381.8713, actual.totalTransportationCost, 1.);
		assertEquals(802.83825, actual.totalCO2Emission, 1.); // in gramm
		assertEquals(0, actual.totalInstallationCost, 1.);

	}
	
	
	public void testdistance() {
		MenDataProvider provider = new MenDataProvider();
		//assertEquals(0.7717, provider.distancecalcforWGS84coord(103.720555,1.275694, 103.726241,  1.271702), 1.);
		
		/*CRSFactory factory = new CRSFactory();
		CoordinateReferenceSystem srcCrs = factory.createFromName("EPSG:4326"); //wgs84
		CoordinateReferenceSystem dstCrs = factory.createFromName("EPSG:32648"); //UTM48N

		BasicCoordinateTransform transform = new BasicCoordinateTransform(srcCrs, dstCrs);

		ProjCoordinate srcCoord = new ProjCoordinate(103.720555,  1.275694); //jurongAromatic
		ProjCoordinate dstCoord = new ProjCoordinate();
		
		ProjCoordinate srcCoord2 = new ProjCoordinate(103.726241,  1.271702); //jurongAromatic
		ProjCoordinate dstCoord2 = new ProjCoordinate();
		*/
		assertEquals(0.7717,provider.distancecalcforUTMcoord (357652.95745539985,141038.13894726874,358285.4509346582,140596.4772218702),1.);
	}


}

