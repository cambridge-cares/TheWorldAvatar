package uk.ac.cam.cares.jps.men.test;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.men.MenDataProvider;

public class TestMenDataProvider extends TestCase {

	String Jr_MEN_OKB = ".\\res\\Jr_MEN.owl";                  // location of the owl file that contains information for the mass exchange network
	String Transport_OKB = ".\\res\\Jr_Transportation_simplified.owl";                  // location of the owl file that contains information for the transportation system
	String OKB = ".\\res\\JurongChemicalPlant_trimmed.owl";                      // define file location of the ontological knowledge base 
	//x source10 sink 4
	//String Jr_MEN_OKB2 = ".\\res\\Jr_MEN2.owl"; 
	
	// based on the Jr_MEN OKB
	String nameOfResourceNetwork = "Jurong_MEN";                               //define the name of the resource network that is to be synthesized 
	
	public void test1createsource (){
		
		MenDataProvider provider= new MenDataProvider();
		int numberofsourcedata = provider.addSourceDataFromKB(Jr_MEN_OKB, OKB, nameOfResourceNetwork).size();
		assertEquals(18, numberofsourcedata);
	}

	public void test2createsink (){
	
		MenDataProvider provider= new MenDataProvider();
		int numberofsinkdata = provider.addSinkDataFromKB(Jr_MEN_OKB, OKB, nameOfResourceNetwork).size();
		assertEquals(26, numberofsinkdata);
	}
	
	public void test3connectsourceandsink (){
		
		MenDataProvider provider= new MenDataProvider();
		int numberofconnectiondata = provider.connectSinkandSourceWithTransportation(Transport_OKB,provider.addSinkDataFromKB(Jr_MEN_OKB, OKB, nameOfResourceNetwork),provider.addSourceDataFromKB(Jr_MEN_OKB, OKB, nameOfResourceNetwork)).size();
		assertEquals(37, numberofconnectiondata);
	}

	public void test4transportationmeans (){
		
		MenDataProvider provider= new MenDataProvider();
		int numberoftransportationmeans = provider.getTransportationMeansInfoFromKB(Transport_OKB).size();
		assertEquals(4, numberoftransportationmeans);
	}
	
	/*public void test5createsinkfromotherJrMEN (){
		
		MenDataProvider provider= new MenDataProvider();
		int numberofsinkdata = provider.addSinkDataFromKB(Jr_MEN_OKB2, OKB, nameOfResourceNetwork).size();
		assertEquals(25, numberofsinkdata);
	}*/

}


