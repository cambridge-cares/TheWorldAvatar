package uk.ac.cam.cares.jps.men.test;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


import junit.framework.TestCase;
import uk.ac.cam.cares.jps.men.MenDataProvider;
import uk.ac.cam.cares.jps.men.MenGamsConverter;
import uk.ac.cam.cares.jps.men.MenResult;
import uk.ac.cam.cares.jps.men.entity.FeasibleConnection;
import uk.ac.cam.cares.jps.men.entity.Parameters;
import uk.ac.cam.cares.jps.men.entity.Sink;
import uk.ac.cam.cares.jps.men.entity.Source;
import uk.ac.cam.cares.jps.men.entity.Transportation;

public class TestMenDataProvider extends TestCase {
	List<String> plantkb= new ArrayList<String>();
	
	public List<String> getsourcedirectory() {
		File[] files = new File("D:\\tmp\\new").listFiles();
		// If this pathname does not denote a directory, then listFiles() returns null.

		for (File file : files) {
			if (file.isFile() && !file.getName().equals("catalog-v001.xml")) {
				plantkb.add("D:\\tmp\\new\\" + file.getName());
			}
		}
		return plantkb;
	}
		
	// based on the Jr_MEN OKB
	String nameOfResourceNetwork = "Jurong_MEN";                               //define the name of the resource network that is to be synthesized 

	public static Map<String, String> NametoCode = new HashMap<>();
	public static Map<String, String> NametoCode2 = new HashMap<>();

	String Transport_OKB = ".\\res\\res2\\Jr_Transportation_simplified.owl";                  // location of the owl file that contains information for the transportation system
	
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
	
	public void testcheckthevalueofsink (){
		getsourcedirectory();
		MenDataProvider provider= new MenDataProvider();
		List<Sink> sink = provider.addSinkDataFromKB(plantkb, nameOfResourceNetwork);
		List<Source> source = provider.addSourceDataFromKB(plantkb, nameOfResourceNetwork);
		sink=provider.puresinkscategorial(sink, source);
		source=provider.puresourcescategorial(source, sink);
		String sink1 = sink.get(3).toString();
		assertEquals("Sink[name=CelaneseSingaporePteLtd, raw material=Ethylene, nearSea = false]", sink1);
	}
	
	//double[] annualCostFactors = new double[] {1., 0.1, 0.02, 0.013, 0.012, 0.01};
	/*double[][] expected = new double[][] {
		{5.987268E9, 5.986443E9, 2.244794E9, 771143., 1082.,       0.},
		{6.009149E9, 6.008846E9, 2.715272E9, 131462.,  230., 1595484.},
		{6.008969E9, 6.008846E9, 2.715272E9,  51949.,  127., 3199500.},
		{6.008946E9, 6.008846E9, 2.715272E9,  47820.,  121., 3498120.},
		{6.008942E9, 6.008846E9, 2.715272E9,  38246.,  109., 4308660.},
		{6.008933E9, 6.008846E9, 2.715272E9,  35798.,  106., 4564620.}
	};
	*/
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
		
		MenGamsConverter converter = new MenGamsConverter();
		Parameters parameters = new Parameters(50., 1., 1.0, 1.05, false);
		MenResult actual = converter.calculate(sources1, sink1, connections1, transportations1, parameters);
		assertEquals(6.41811655E9, actual.totalMaterialPurchaseCost, 1.);
		assertEquals(1062291, actual.totalTransportationCost, 1.);
		assertEquals(1472.95734, actual.totalCO2Emission, 1.); // in gramm 
	}
	
	
	public void testcheckthevaluefromOWLwith1yearcostfactor() {
		MenDataProvider provider = new MenDataProvider();
		getsourcedirectory();
		List<Sink> sink1 = provider.addSinkDataFromKB(plantkb, nameOfResourceNetwork);
		List<Source> sources1 = provider.addSourceDataFromKB(plantkb, nameOfResourceNetwork);
		sink1=provider.puresinkscategorial(sink1, sources1);
		 sources1 = provider.puresourcescategorial(sources1,sink1);
		List<FeasibleConnection> connections1 = provider.connectSinkandSourceWithTransportation( sink1,
				sources1);
		List<Transportation> transportations1 = provider.getTransportationMeansInfoFromKB(Transport_OKB);

		MenGamsConverter converter = new MenGamsConverter();

		double factor = 1.0;
		System.out.println("Annual Cost Factor =" + factor);
		Parameters parameters = new Parameters(50., 1., factor, 1.05, false);
		MenResult actual = converter.calculate(sources1, sink1, connections1, transportations1, parameters);
		// provider.startCalculation();
		assertEquals(6.419252488E9, actual.objValue, 1000.);
		assertEquals(6.41811655E9, actual.totalMaterialPurchaseCost, 1000.);
		assertEquals(2.2852966E9, actual.totalMaterialPurchaseCostInternationalMarket, 1000.);
		assertEquals(1062291.006, actual.totalTransportationCost, 1.);
		assertEquals(1472.9573, actual.totalCO2Emission, 1.); // in gramm
		assertEquals(0, actual.totalInstallationCost, 1.);

	}
	
	public void testcheckthevaluefromOWLwith10yearcostfactor() {
		MenDataProvider provider = new MenDataProvider();
		getsourcedirectory();
		List<Sink> sink1 = provider.addSinkDataFromKB(plantkb, nameOfResourceNetwork);
		List<Source> sources1 = provider.addSourceDataFromKB(plantkb, nameOfResourceNetwork);
		sink1=provider.puresinkscategorial(sink1, sources1);
		 sources1 = provider.puresourcescategorial(sources1,sink1);
		List<FeasibleConnection> connections1 = provider.connectSinkandSourceWithTransportation( sink1,
				sources1);
		List<Transportation> transportations1 = provider.getTransportationMeansInfoFromKB(Transport_OKB);

		MenGamsConverter converter = new MenGamsConverter();

		double factor = 0.1;
		System.out.println("Annual Cost Factor =" + factor);
		Parameters parameters = new Parameters(50., 1., factor, 1.05, false);
		MenResult actual = converter.calculate(sources1, sink1, connections1, transportations1, parameters);
		// provider.startCalculation();
		assertEquals(6.637116668E9, actual.objValue, 1000.);
		assertEquals(6.636902E9, actual.totalMaterialPurchaseCost, 1000.);
		assertEquals(3.743276E9, actual.totalMaterialPurchaseCostInternationalMarket, 1000.);
		assertEquals(111673.20983, actual.totalTransportationCost, 1.);
		assertEquals(179.867, actual.totalCO2Emission, 1.); // in gramm
		assertEquals(938520.00, actual.totalInstallationCost, 1.);

	}
	
	public void testcheckthevaluefromOWLwith50yearcostfactor() {
		MenDataProvider provider = new MenDataProvider();
		getsourcedirectory();
		List<Sink> sink1 = provider.addSinkDataFromKB(plantkb, nameOfResourceNetwork);
		List<Source> sources1 = provider.addSourceDataFromKB(plantkb, nameOfResourceNetwork);
		sink1=provider.puresinkscategorial(sink1, sources1);
		 sources1 = provider.puresourcescategorial(sources1,sink1);
		List<FeasibleConnection> connections1 = provider.connectSinkandSourceWithTransportation(sink1,
				sources1);
		List<Transportation> transportations1 = provider.getTransportationMeansInfoFromKB(Transport_OKB);

		MenGamsConverter converter = new MenGamsConverter();

		double factor = 0.02;
		System.out.println("Annual Cost Factor =" + factor);
		Parameters parameters = new Parameters(50., 1., factor, 1.05, false);
		MenResult actual = converter.calculate(sources1, sink1, connections1, transportations1, parameters);
		// provider.startCalculation();
		assertEquals(6.63698897E9, actual.objValue, 1000.);
		assertEquals(6.636902E9, actual.totalMaterialPurchaseCost, 1000.);
		assertEquals(3.743276E9, actual.totalMaterialPurchaseCostInternationalMarket, 1000.);
		assertEquals(32160.305989, actual.totalTransportationCost, 1.);
		assertEquals(76.3304, actual.totalCO2Emission, 1.); // in gramm
		assertEquals(2542535.99763, actual.totalInstallationCost, 1.);

	}
	
	public void testcheckthevaluefromOWLwith77yearcostfactor() {
		MenDataProvider provider = new MenDataProvider();
		getsourcedirectory();
		List<Sink> sink1 = provider.addSinkDataFromKB(plantkb, nameOfResourceNetwork);
		List<Source> sources1 = provider.addSourceDataFromKB(plantkb, nameOfResourceNetwork);
		sink1=provider.puresinkscategorial(sink1, sources1);
		 sources1 = provider.puresourcescategorial(sources1,sink1);
		List<FeasibleConnection> connections1 = provider.connectSinkandSourceWithTransportation( sink1,
				sources1);
		List<Transportation> transportations1 = provider.getTransportationMeansInfoFromKB(Transport_OKB);

		MenGamsConverter converter = new MenGamsConverter();

		double factor = 0.013;
		System.out.println("Annual Cost Factor =" + factor);
		Parameters parameters = new Parameters(50., 1., factor, 1.05, false);
		MenResult actual = converter.calculate(sources1, sink1, connections1, transportations1, parameters);
		// provider.startCalculation();
		assertEquals(6.63697066E9, actual.objValue, 1000.);
		assertEquals(6.636902E9, actual.totalMaterialPurchaseCost, 1000.);
		assertEquals(3.743276E9, actual.totalMaterialPurchaseCostInternationalMarket, 1000.);
		assertEquals(28031.705989, actual.totalTransportationCost, 1.);
		assertEquals(70.9544, actual.totalCO2Emission, 1.); // in gramm
		assertEquals(2841155.99593, actual.totalInstallationCost, 1.);

	}
	
	public void testcheckthevaluefromOWLwith83yearcostfactor() {
		MenDataProvider provider = new MenDataProvider();
		getsourcedirectory();
		List<Sink> sink1 = provider.addSinkDataFromKB(plantkb, nameOfResourceNetwork);
		List<Source> sources1 = provider.addSourceDataFromKB(plantkb, nameOfResourceNetwork);
		sink1=provider.puresinkscategorial(sink1, sources1);
		 sources1 = provider.puresourcescategorial(sources1,sink1);
		List<FeasibleConnection> connections1 = provider.connectSinkandSourceWithTransportation( sink1,
				sources1);
		List<Transportation> transportations1 = provider.getTransportationMeansInfoFromKB(Transport_OKB);

		MenGamsConverter converter = new MenGamsConverter();

		double factor = 0.012;
		System.out.println("Annual Cost Factor =" + factor);
		Parameters parameters = new Parameters(50., 1., factor, 1.05, false);
		MenResult actual = converter.calculate(sources1, sink1, connections1, transportations1, parameters);
		assertEquals(6.63696735E9, actual.objValue, 1000.);
		assertEquals(6.636902E9, actual.totalMaterialPurchaseCost, 1000.);
		assertEquals(3.743276E9, actual.totalMaterialPurchaseCostInternationalMarket, 1000.);
		assertEquals(18457.285989, actual.totalTransportationCost, 1.);
		assertEquals(58.4872, actual.totalCO2Emission, 1.); // in gramm
		assertEquals(3651695.99593, actual.totalInstallationCost, 1.);

	}
	
	public void testcheckthevaluefromOWLwith100yearcostfactor() {
		MenDataProvider provider = new MenDataProvider();
		getsourcedirectory();
		List<Sink> sink1 = provider.addSinkDataFromKB(plantkb, nameOfResourceNetwork);
		List<Source> sources1 = provider.addSourceDataFromKB(plantkb, nameOfResourceNetwork);
		sink1=provider.puresinkscategorial(sink1, sources1);
		 sources1 = provider.puresourcescategorial(sources1,sink1);
		List<FeasibleConnection> connections1 = provider.connectSinkandSourceWithTransportation(sink1,
				sources1);
		List<Transportation> transportations1 = provider.getTransportationMeansInfoFromKB(Transport_OKB);

		MenGamsConverter converter = new MenGamsConverter();
		double factor = 0.01;
		System.out.println("Annual Cost Factor =" + factor);
		Parameters parameters = new Parameters(50., 1., factor, 1.05, false);
		MenResult actual = converter.calculate(sources1, sink1, connections1, transportations1, parameters);
	
		assertEquals(6.6369600E9, actual.objValue, 1000.);
		assertEquals(6.636902E9, actual.totalMaterialPurchaseCost, 1000.);
		assertEquals(3.743276E9, actual.totalMaterialPurchaseCostInternationalMarket, 1000.);
		assertEquals(16009.615989, actual.totalTransportationCost, 1.);
		assertEquals(55.3, actual.totalCO2Emission, 1.); // in gramm
		assertEquals(3907655.99593, actual.totalInstallationCost, 1.);

	}

	public void testcheckthevaluefromOWLwith1yearcostfactortruemarketlowestprice() {
		MenDataProvider provider = new MenDataProvider();
		getsourcedirectory();
		List<Sink> sink1 = provider.addSinkDataFromKB(plantkb, nameOfResourceNetwork);
		List<Source> sources1 = provider.addSourceDataFromKB(plantkb, nameOfResourceNetwork);
		sink1=provider.puresinkscategorial(sink1, sources1);
		 sources1 = provider.puresourcescategorial(sources1,sink1);
		List<FeasibleConnection> connections1 = provider.connectSinkandSourceWithTransportation( sink1,
				sources1);
		List<Transportation> transportations1 = provider.getTransportationMeansInfoFromKB(Transport_OKB);

		MenGamsConverter converter = new MenGamsConverter();

		double factor = 1.0;
		System.out.println("Annual Cost Factor =" + factor);
		Parameters parameters = new Parameters(50., 1., factor, 1.05, true);
		MenResult actual = converter.calculate(sources1, sink1, connections1, transportations1, parameters);
		assertEquals(6.21537554E9, actual.objValue, 1000.);
		assertEquals(6.2147356E9, actual.totalMaterialPurchaseCost, 1000.);
		assertEquals(2.8730357E9, actual.totalMaterialPurchaseCostInternationalMarket, 1000.);
		assertEquals(598291.006, actual.totalTransportationCost, 1.);
		assertEquals(832.637356, actual.totalCO2Emission, 1.); // in gramm
		assertEquals(0, actual.totalInstallationCost, 1.);

	}
	
	public void testcheckthevaluefromOWLwithcarbontaxvariation1() {
		MenDataProvider provider = new MenDataProvider();
		getsourcedirectory();
		List<Sink> sink1 = provider.addSinkDataFromKB(plantkb, nameOfResourceNetwork);
		List<Source> sources1 = provider.addSourceDataFromKB(plantkb, nameOfResourceNetwork);
		sink1=provider.puresinkscategorial(sink1, sources1);
		 sources1 = provider.puresourcescategorial(sources1,sink1);
		List<FeasibleConnection> connections1 = provider.connectSinkandSourceWithTransportation(sink1,
				sources1);
		List<Transportation> transportations1 = provider.getTransportationMeansInfoFromKB(Transport_OKB);

		MenGamsConverter converter = new MenGamsConverter();

		double factor = 1.0;
		System.out.println("Annual Cost Factor =" + factor);
		Parameters parameters = new Parameters(70., 1., factor, 1.05, true);
		MenResult actual = converter.calculate(sources1, sink1, connections1, transportations1, parameters);
		assertEquals(6.2153922E9, actual.objValue, 1000.);
		assertEquals(6.2147356E9, actual.totalMaterialPurchaseCost, 1000.);
		assertEquals(2.8730357E9, actual.totalMaterialPurchaseCostInternationalMarket, 1000.);
		assertEquals(598291.006, actual.totalTransportationCost, 1.);
		assertEquals(832.63734856, actual.totalCO2Emission, 1.); // in gramm
		assertEquals(0, actual.totalInstallationCost, 1.);

	}
	
	public void testcheckthevaluefromOWLwithinterestfactorvariation2() {
		MenDataProvider provider = new MenDataProvider();
		getsourcedirectory();
		List<Sink> sink1 = provider.addSinkDataFromKB(plantkb, nameOfResourceNetwork);
		List<Source> sources1 = provider.addSourceDataFromKB(plantkb, nameOfResourceNetwork);
		sink1=provider.puresinkscategorial(sink1, sources1);
		 sources1 = provider.puresourcescategorial(sources1,sink1);
		List<FeasibleConnection> connections1 = provider.connectSinkandSourceWithTransportation( sink1,
				sources1);
		List<Transportation> transportations1 = provider.getTransportationMeansInfoFromKB(Transport_OKB);

		MenGamsConverter converter = new MenGamsConverter();

		double factor = 1.0;
		System.out.println("Annual Cost Factor =" + factor);
		Parameters parameters = new Parameters(50., 1.3, factor, 1.05, true);
		MenResult actual = converter.calculate(sources1, sink1, connections1, transportations1, parameters);
		assertEquals(8.0799882E9, actual.objValue, 1000.);
		assertEquals(6.2147356E9, actual.totalMaterialPurchaseCost, 1000.);
		assertEquals(2.8730357E9, actual.totalMaterialPurchaseCostInternationalMarket, 1000.);
		assertEquals(598291.006, actual.totalTransportationCost, 1.);
		assertEquals(832.63734856, actual.totalCO2Emission, 1.); // in gramm
		assertEquals(0, actual.totalInstallationCost, 1.);

	}
	
	public void testcheckthevaluefromOWLwithinternationalmarketpricefactorvariation3() {
		MenDataProvider provider = new MenDataProvider();
		getsourcedirectory();
		List<Sink> sink1 = provider.addSinkDataFromKB(plantkb, nameOfResourceNetwork);
		List<Source> sources1 = provider.addSourceDataFromKB(plantkb, nameOfResourceNetwork);
		sink1=provider.puresinkscategorial(sink1, sources1);
		 sources1 = provider.puresourcescategorial(sources1,sink1);
		List<FeasibleConnection> connections1 = provider.connectSinkandSourceWithTransportation( sink1,
				sources1);
		List<Transportation> transportations1 = provider.getTransportationMeansInfoFromKB(Transport_OKB);

		MenGamsConverter converter = new MenGamsConverter();

		double factor = 1.0;
		System.out.println("Annual Cost Factor =" + factor);
		Parameters parameters = new Parameters(50., 1., factor, 1.4, true);
		MenResult actual = converter.calculate(sources1, sink1, connections1, transportations1, parameters);
		assertEquals(7.133994138E9, actual.objValue, 1000.);
		assertEquals(7.13285826E9, actual.totalMaterialPurchaseCost, 1000.);
		assertEquals(3.0000383E9, actual.totalMaterialPurchaseCostInternationalMarket, 1000.);
		assertEquals(1062291.006, actual.totalTransportationCost, 1.);
		assertEquals(1472.95734, actual.totalCO2Emission, 1.); // in gramm
		assertEquals(0, actual.totalInstallationCost, 1.);

	}
	
	
	public void testdistance() {
		MenDataProvider provider = new MenDataProvider();
		assertEquals(0.31165, provider.distancecalcforWGS84coord(1.273108, 103.713892, 1.2717503, 103.7159824), 1.);
	}


}

