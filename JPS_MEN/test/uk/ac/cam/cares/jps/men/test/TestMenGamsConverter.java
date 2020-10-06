package uk.ac.cam.cares.jps.men.test;

import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.men.MenGamsConverter;
import uk.ac.cam.cares.jps.men.MenResult;
import uk.ac.cam.cares.jps.men.entity.FeasibleConnection;
import uk.ac.cam.cares.jps.men.entity.MenCalculationParameters;
import uk.ac.cam.cares.jps.men.entity.Product;
import uk.ac.cam.cares.jps.men.entity.Sink;
import uk.ac.cam.cares.jps.men.entity.Source;
import uk.ac.cam.cares.jps.men.entity.Transportation;

public class TestMenGamsConverter extends TestCase {
	
	// Preprint 164, p20, Table 8: Parameters for calculating costs and CO2 emissions
	// format: transportation name, transportation cost (dollar per t * km), installation cost (dollar per km), emission (gramm C02 per t * km)
	String TRANSPORTATIONS_AS_STRING = "Trucks,0.1,0.0,138.0"
			+ ",Ships,0.015,0.0,31.0"
			+ ",LandPipelines,0.0017,85320.0,10.0"
			+ ",SeaPipelines,0.0017,620000.0,10.0";
	
	/**
	 * only sinks d7,d11,d13 (and thus sources r4 and r16), no near sea plants
	 * international market is considered, 
	 * r4 and r16 have same price 1364
	 * in the result only trucks are used for transport
	 */
	public void test1Preprint164SimpleNetworkWithSamePriceAndInternationalMarket() {
		
		// Preprint 164, p20, Table 7: source information in the material network
		// format: source name, amount (kt per year), price (dollar per t) 
		String sourcesAsString = "r2,250,1750,r3,1900,1254,r4,580,1364,r5,115,1700"
				+ ",r6,120,2010,r7,300,1510,r8,210,2072,r9,1010,1254,r10,1000,1124"
				+ ",r11,55,2161,r12,181,1750,r13,73,1988,r14,200,2010,r15,223,2010"
				+ ",r16,438,1364,r17,65,1605,r18,150,3190";
		// Preprint 164, p20, Table 7: sink information in the material network
		// format: sink name, amount (kt per year) 
		String sinksAsString = "d7,412.5,d11,411.5,d13,292.5";
		// Preprint 164, p18, Table 6: Distances between the potential connectable companies
		// format: source name, sink name, distance (km)
		String distancesAsString = "r4,d7,8,r16,d7,10.4,r4,d11,5,r16,d11,5.3,r4,d13,8,r16,d13,10.4";
		
		List<Source> sources = createSources(sourcesAsString);
		List<Sink> sinks = createSinks(sinksAsString);
		List<FeasibleConnection> connections = createFeasibleConnections(sources, sinks, distancesAsString);
		
		List<Transportation> transportations = createTransportations(TRANSPORTATIONS_AS_STRING);

		MenGamsConverter converter = new MenGamsConverter();
		MenResult result = converter.calculate(sources, sinks, connections, transportations, createDefaultParameters());
		assertEquals(1.5296237E9, result.totalMaterialPurchaseCost, 1.);
		// first solution, objective function value is very close to the minimum
		//assertEquals(12662, result.totalTransportationCost, 1.);
		// second solution, objective function value is smaller than for first solution
		assertEquals(709655, result.totalTransportationCost, 1.);
		assertEquals(979., result.totalCO2Emission, 1.); // in gramm 
	}
	
	/**
	 * only sinks d7,d11,d13 (and thus sources r4 and r16), no near sea plants
	 * international market is considered, 
	 * r4 and r16 have different price 1364 vs 1023
	 * in the result only trucks are used for transport
	 */
	public void test2Preprint164SimpleNetworkWithDifferentPriceAndInternationalMarket() {
		
		// Preprint 164, p20, Table 7: source information in the material network
		// format: source name, amount (kt per year), price (dollar per t) 
		String sourcesAsString = "r2,250,1750,r3,1900,1254,r4,580,1364,r5,115,1700"
				+ ",r6,120,2010,r7,300,1510,r8,210,2072,r9,1010,1254,r10,1000,1124"
				+ ",r11,55,2161,r12,181,1750,r13,73,1988,r14,200,2010,r15,223,2010"
				+ ",r16,438,1023,r17,65,1605,r18,150,3190";
		// Preprint 164, p20, Table 7: sink information in the material network
		// format: sink name, amount (kt per year) 
		String sinksAsString = "d7,412.5,d11,411.5,d13,292.5";
		// Preprint 164, p18, Table 6: Distances between the potential connectable companies
		// format: source name, sink name, distance (km)
		String distancesAsString = "r4,d7,8,r16,d7,10.4,r4,d11,5,r16,d11,5.3,r4,d13,8,r16,d13,10.4";
		
		List<Source> sources = createSources(sourcesAsString);
		List<Sink> sinks = createSinks(sinksAsString);
		List<FeasibleConnection> connections = createFeasibleConnections(sources, sinks, distancesAsString);
		
		List<Transportation> transportations = createTransportations(TRANSPORTATIONS_AS_STRING);

		//calculateMaterialPurchaseCostDirectly(sources, sinks, connections);
		
		MenGamsConverter converter = new MenGamsConverter();
		MenResult result = converter.calculate(sources, sinks, connections, transportations, createDefaultParameters());
		assertEquals(1.3802657E9, result.totalMaterialPurchaseCost, 1.);
		// first solution, objective function value is very close to the minimum
		//assertEquals(744830, result.totalTransportationCost, 1.);
		// second solution, objective function value is smaller than for first solution
		assertEquals(709655, result.totalTransportationCost, 1.);
		assertEquals(979., result.totalCO2Emission, 1.); // in gramm 
	}
	
	/**
	 * only sinks d7,d11,d13 (and thus sources r4 and r16), 
	 * all sinks and sources are near sea
	 * as a result trucks and ships are used for transportation
	 */
	public void test3Preprint164SimpleNetworkWithAllNearSea() {
		
		// Preprint 164, p20, Table 7: source information in the material network
		// format: source name, amount (kt per year), price (dollar per t) 
		String sourcesAsString = "r2,250,1750,r3,1900,1254,r4,580,1364,r5,115,1700"
				+ ",r6,120,2010,r7,300,1510,r8,210,2072,r9,1010,1254,r10,1000,1124"
				+ ",r11,55,2161,r12,181,1750,r13,73,1988,r14,200,2010,r15,223,2010"
				+ ",r16,438,1023,r17,65,1605,r18,150,3190";
		// Preprint 164, p20, Table 7: sink information in the material network
		// format: sink name, amount (kt per year) 
		String sinksAsString = "d7,412.5,d11,411.5,d13,292.5";
		// Preprint 164, p18, Table 6: Distances between the potential connectable companies
		// format: source name, sink name, distance (km)
		String distancesAsString = "r4,d7,8,r16,d7,10.4,r4,d11,5,r16,d11,5.3,r4,d13,8,r16,d13,10.4";
		
		List<Source> sources = createSources(sourcesAsString);
		List<Sink> sinks = createSinks(sinksAsString);
		List<FeasibleConnection> connections = createFeasibleConnections(sources, sinks, distancesAsString);
		List<Transportation> transportations = createTransportations(TRANSPORTATIONS_AS_STRING);

		// add for all sources and sinks near sea information
		for (Source source : sources) {
			source.setNearSea(true);
		}
		for (Sink sink : sinks) {
			sink.setNearSea(true);
		}
		
		MenGamsConverter converter = new MenGamsConverter();
		//Parameters parameters = createDefaultParameters();
		MenCalculationParameters parameters = new MenCalculationParameters(50., 1., .01, 1.05, false);
		MenResult result = converter.calculate(sources, sinks, connections, transportations, parameters);
		assertEquals(1.3802657E9, result.totalMaterialPurchaseCost, 1.);
		// first solution, objective function value is very close to the minimum
		//assertEquals(744830, result.totalTransportationCost, 1.);
		// second solution, objective function value is smaller than for first solution
		assertEquals(106448, result.totalTransportationCost, 1.);
		assertEquals(219., result.totalCO2Emission, 1.); // in gramm 
	}
	
	/**
	 *  All sinks, sources and connections as in preprint 164 but without source d20. 
	 *  The near sea information is derived from fig. 9 of preprint 164.<br>
	 *  <br>
	 *  Result (with the help of e2aADD constraint in GAMS code):
	 *  4 transportations with ships, all others with trucks, 
	 *  in addition 3 times purchase from internation market
	 */
	public void test4bPreprint164RemovedConnectionsTod20WithNearSeaInformation() {
		
		// Preprint 164, p20, Table 7: source information in the material network
		// format: source name, amount (kt per year), price (dollar per t) 
		String sourcesAsString = "r1,500,555,r2,250,1750,r3,1900,1254,r4,580,1364,r5,115,1700"
				+ ",r6,120,2010,r7,300,1510,r8,210,2072,r9,1010,1254,r10,1000,1124"
				+ ",r11,55,2161,r12,181,1750,r13,73,1988,r14,200,2010,r15,223,2010"
				+ ",r16,438,1023,r17,65,1605,r18,150,3190";
		// Preprint 164, p20, Table 7: sink information in the material network
		// format: sink name, amount (kt per year) 
		String sinksAsString = "d1,35.3,d2,68.4,d3,400,d4,10,d5,38.8"
				+ ",d6,87.5,d7,412.5,d8,181,d9,28.2,d10,33.6"
				+ ",d11,411.5,d12,205.8,d13,292.5,d14,131.1,d15,42.6"
				+ ",d16,35,d17,202,d18,39.2,d19,10.3"
				+ ",d21,12,d22,8.3,d23,9.9,d24,285.7,d25,27.8,d26,574.8";
		// Preprint 164, p18, Table 6: Distances between the potential connectable companies
		// format: source name, sink name, distance (km)
		String distancesAsString = "r2,d9,1.5,r2,d24,4,r3,d2,4,r3,d3,5.3,r3,d10,3.8,r4,d7,8,r4,d11,5,r4,d13,8,r5,d5,2"
				+ ",r6,d21,3.5,r7,d1,1,r7,d4,1.5,r8,d17,0.5,r9,d2,4,r9,d3,1.5,r9,d10,4,r10,d6,3.2,r10,d8,2.2,r10,d12,3.1,r10,d14,2.2,r10,d15,3,r10,d16,3"
				+ ",r11,d19,8,r11,d22,3,r12,d9,1.5,r12,d24,4,r13,d18,0.5,r14,d21,3.5,r15,d21,3.5"
				+ ",r16,d7,10.4,r16,d11,5.3,r16,d13,10.4,r17,d23,8,r18,d25,7,r18,d26,4";
		
		List<Source> sources = createSources(sourcesAsString);
		List<Sink> sinks = createSinks(sinksAsString);
		List<FeasibleConnection> connections = createFeasibleConnections(sources, sinks, distancesAsString);
		List<Transportation> transportations = createTransportations(TRANSPORTATIONS_AS_STRING);

		addNearSeaInformation(sources, "r2,r12,r17,r18", sinks, "d23,d24,d26");
		
		MenGamsConverter converter = new MenGamsConverter();
		MenCalculationParameters parameters = new MenCalculationParameters(50., 1., 1., 1.05, false);
		MenResult result = converter.calculate(sources, sinks, connections, transportations, parameters);
		assertEquals(6.0323151E9, result.totalMaterialPurchaseCost, 1.);
		assertEquals(1064863, result.totalTransportationCost, 1.);
		assertEquals(1488., result.totalCO2Emission, 1.); // in gramm 
		
		
//		----    156 VARIABLE MassIntMarket.L  mass purchased from the international market for demand j
//
//		j_6_d7_P    98500.000,    j_23_d25_P  27800.000,    j_24_d26_P 424800.000
		
		
//		----    156 VARIABLE TC.L  transportation cost of transport material from i to j through transportation type tt
//
//        						tt_Trucks    tt_Ships
//
//		i_1_r2_P  .j_22_d24_P               15000.000
//		i_2_r3_P  .j_1_d2_P     27360.000
//		i_2_r3_P  .j_9_d10_P    12768.000
//		i_3_r4_P  .j_6_d7_P    230000.000
//		i_3_r4_P  .j_12_d13_P  234000.000
//		i_4_r5_P  .j_4_d5_P      7760.000
//		i_6_r7_P  .j_0_d1_P      3530.000
//		i_6_r7_P  .j_3_d4_P      1500.000
//		i_7_r8_P  .j_16_d17_P   10100.000
//		i_8_r9_P  .j_2_d3_P     60000.000
//		i_9_r10_P .j_5_d6_P     28000.000
//		i_9_r10_P .j_7_d8_P     39820.001
//		i_9_r10_P .j_11_d12_P   63797.998
//		i_9_r10_P .j_13_d14_P   28842.001
//		i_9_r10_P .j_14_d15_P   12780.000
//		i_9_r10_P .j_15_d16_P   10500.000
//		i_10_r11_P.j_18_d19_P    8240.000
//		i_10_r11_P.j_20_d22_P    2490.000
//		i_11_r12_P.j_8_d9_P      4230.000
//		i_11_r12_P.j_22_d24_P                2142.000
//		i_12_r13_P.j_17_d18_P    1960.000
//		i_14_r15_P.j_19_d21_P    4200.000
//		i_15_r16_P.j_6_d7_P     27559.999
//		i_15_r16_P.j_10_d11_P  218095.008
//		i_16_r17_P.j_21_d23_P                1188.000
//		i_17_r18_P.j_24_d26_P                9000.000		
	}

	/**
	 *  All sinks, sources and connections as in preprint 164 but without source d20. 
	 *  The near sea information is derived from fig. 9 of preprint 164.
	 */
	public void test4aPreprint164RemovedConnectionsTod20WithoutNearSea() {
		
		// Preprint 164, p20, Table 7: source information in the material network
		// format: source name, amount (kt per year), price (dollar per t) 
		String sourcesAsString = "r2,250,1750,r3,1900,1254,r4,580,1364,r5,115,1700"
				+ ",r6,120,2010,r7,300,1510,r8,210,2072,r9,1010,1254,r10,1000,1124"
				+ ",r11,55,2161,r12,181,1750,r13,73,1988,r14,200,2010,r15,223,2010"
				+ ",r16,438,1023,r17,65,1605,r18,150,3190";
		// Preprint 164, p20, Table 7: sink information in the material network
		// format: sink name, amount (kt per year) 
		String sinksAsString = "d1,35.3,d2,68.4,d3,400,d4,10,d5,38.8"
				+ ",d11,411.5,d12,205.8,d13,292.5,d14,131.1,d15,42.6"
				+ ",d16,35,d17,202,d18,39.2,d19,10.3"
				+ ",d6,87.5,d7,412.5,d8,181,d9,28.2,d10,33.6"
				+ ",d21,12,d22,8.3,d23,9.9,d24,285.7,d25,27.8,d26,574.8";
		// Preprint 164, p18, Table 6: Distances between the potential connectable companies
		// format: source name, sink name, distance (km)
		String distancesAsString = "r2,d9,1.5,r2,d24,4,r3,d2,4,r3,d3,5.3,r3,d10,3.8,r4,d7,8,r4,d11,5,r4,d13,8,r5,d5,2"
				+ ",r11,d19,8,r11,d22,3,r12,d9,1.5,r12,d24,4,r13,d18,0.5,r14,d21,3.5,r15,d21,3.5"
				+ ",r6,d21,3.5,r7,d1,1,r7,d4,1.5,r8,d17,0.5,r9,d2,4,r9,d3,1.5,r9,d10,4,r10,d6,3.2,r10,d8,2.2,r10,d12,3.1,r10,d14,2.2,r10,d15,3,r10,d16,3"
				+ ",r16,d7,10.4,r16,d11,5.3,r16,d13,10.4,r17,d23,8,r18,d25,7,r18,d26,4";
		
		List<Source> sources = createSources(sourcesAsString);
		List<Sink> sinks = createSinks(sinksAsString);
		List<FeasibleConnection> connections = createFeasibleConnections(sources, sinks, distancesAsString);
		
		List<Transportation> transportations = createTransportations(TRANSPORTATIONS_AS_STRING);
		
		MenCalculationParameters parameters = new MenCalculationParameters(50., 1., 1., 1.05, false);
		
		calculateMaterialPurchaseCostDirectly(sources, sinks, connections, parameters.internationalMarketPriceFactor, parameters.internationalMarketLowestPrice);
		// the cost for sink d25 differs from GAMS calculation
		// the cost for sink d26 differs from GAMS calculation
		// reason: both have only source r18 which is sold to d25 in this Junit test
		// while d25 buys from international market in GAMS
		// the sum of the cost for sink d25 and d26 is 1.994E9
		// thats the same amount for the GAMS calculation
		
		MenGamsConverter converter = new MenGamsConverter();
		MenResult result = converter.calculate(sources, sinks, connections, transportations, parameters);
		assertEquals(6.0323151E9, result.totalMaterialPurchaseCost, 1.);
		assertEquals(1219733, result.totalTransportationCost, 1.);
		assertEquals(1683., result.totalCO2Emission, 1.); // in gramm 
	}
	
	/**
	 *  All sinks, sources and connections as in preprint 164. 
	 *  The near sea information is derived from fig. 9 of preprint 164.
	 */
	public void test5aPreprint164() {
		
		// Preprint 164, p20, Table 7: source information in the material network
		// format: source name, amount (kt per year), price (dollar per t) 
		String sourcesAsString = "r1,500,555,r2,250,1750,r3,1900,1254,r4,580,1364,r5,115,1700"
				+ ",r6,120,2010,r7,300,1510,r8,210,2072,r9,1010,1254,r10,1000,1124"
				+ ",r11,55,2161,r12,181,1750,r13,73,1988,r14,200,2010,r15,223,2010"
				+ ",r16,438,1023,r17,65,1605,r18,150,3190";
		// Preprint 164, p20, Table 7: sink information in the material network
		// format: sink name, amount (kt per year) 
		String sinksAsString = "d1,35.3,d2,68.4,d3,400,d4,10,d5,38.8"
				+ ",d6,87.5,d7,412.5,d8,181,d9,28.2,d10,33.6"
				+ ",d11,411.5,d12,205.8,d13,292.5,d14,131.1,d15,42.6"
				+ ",d16,35,d17,202,d18,39.2,d19,10.3,d20,283.8"
				+ ",d21,12,d22,8.3,d23,9.9,d24,285.7,d25,27.8,d26,574.8";
		// Preprint 164, p18, Table 6: Distances between the potential connectable companies
		// format: source name, sink name, distance (km)
		String distancesAsString = "r1,d20,6,r2,d9,1.5,r2,d24,4,r3,d2,4,r3,d3,5.3,r3,d10,3.8,r3,d20,1.5,r4,d7,8,r4,d11,5,r4,d13,8,r5,d5,2"
				+ ",r6,d21,3.5,r7,d1,1,r7,d4,1.5,r8,d17,0.5,r9,d2,4,r9,d3,1.5,r9,d10,4,r9,d20,6.3,r10,d6,3.2,r10,d8,2.2,r10,d12,3.1,r10,d14,2.2,r10,d15,3,r10,d16,3"
				+ ",r11,d19,8,r11,d22,3,r12,d9,1.5,r12,d24,4,r13,d18,0.5,r14,d21,3.5,r15,d21,3.5"
				+ ",r16,d7,10.4,r16,d11,5.3,r16,d13,10.4,r17,d23,8,r18,d25,7,r18,d26,4";
		
		List<Source> sources = createSources(sourcesAsString);
		List<Sink> sinks = createSinks(sinksAsString);
		List<FeasibleConnection> connections = createFeasibleConnections(sources, sinks, distancesAsString);
		List<Transportation> transportations = createTransportations(TRANSPORTATIONS_AS_STRING);

		addNearSeaInformation(sources, "r2,r12,r17,r18", sinks, "d23,d24,d26");
		
		MenGamsConverter converter = new MenGamsConverter();
		MenCalculationParameters parameters = new MenCalculationParameters(50., 1., 1., 1.05, false);
		MenResult result = converter.calculate(sources, sinks, connections, transportations, parameters);
		assertEquals(6.1898241E9, result.totalMaterialPurchaseCost, 1.);
		assertEquals(1235143, result.totalTransportationCost, 1.);
		assertEquals(1723., result.totalCO2Emission, 1.); // in gramm 
		
//		----    156 VARIABLE MassIntMarket.L  mass purchased from the international market for demand j
//
//		j_6_d7_P    98500.000,    j_24_d25_P  27800.000,    j_25_d26_P 424800.000

		
		
//		----    156 VARIABLE TC.L  transportation cost of transport material from i to j through transportation type tt
//
//		                        tt_Trucks    tt_Ships
//
//		i_0_r1_P  .j_19_d20_P  170280.000
//		i_1_r2_P  .j_23_d24_P               15000.000
//		i_2_r3_P  .j_1_d2_P     27360.000
//		i_2_r3_P  .j_9_d10_P    12768.000
//		i_3_r4_P  .j_6_d7_P    230000.000
//		i_3_r4_P  .j_12_d13_P  234000.000
//		i_4_r5_P  .j_4_d5_P      7760.000
//		i_6_r7_P  .j_0_d1_P      3530.000
//		i_6_r7_P  .j_3_d4_P      1500.000
//		i_7_r8_P  .j_16_d17_P   10100.000
//		i_8_r9_P  .j_2_d3_P     60000.000
//		i_9_r10_P .j_5_d6_P     28000.000
//		i_9_r10_P .j_7_d8_P     39820.001
//		i_9_r10_P .j_11_d12_P   63797.998
//		i_9_r10_P .j_13_d14_P   28842.001
//		i_9_r10_P .j_14_d15_P   12780.000
//		i_9_r10_P .j_15_d16_P   10500.000
//		i_10_r11_P.j_18_d19_P    8240.000
//		i_10_r11_P.j_21_d22_P    2490.000
//		i_11_r12_P.j_8_d9_P      4230.000
//		i_11_r12_P.j_23_d24_P                2142.000
//		i_12_r13_P.j_17_d18_P    1960.000
//		i_14_r15_P.j_20_d21_P    4200.000
//		i_15_r16_P.j_6_d7_P     27559.999
//		i_15_r16_P.j_10_d11_P  218095.008
//		i_16_r17_P.j_22_d23_P                1188.000
//		i_17_r18_P.j_25_d26_P                9000.000
	}
	
	/**
	 *  All sinks, sources and connections as in preprint 164. 
	 *  The near sea information is derived from fig. 9 of preprint 164.
	 */
	public void test5aPreprint164Withoutr1d20connection() {
		
		// Preprint 164, p20, Table 7: source information in the material network
		// format: source name, amount (kt per year), price (dollar per t) 
		String sourcesAsString = "r1,500,555,r2,250,1750,r3,1900,1254,r4,580,1364,r5,115,1700"
				+ ",r6,120,2010,r7,300,1510,r8,210,2072,r9,1010,1254,r10,1000,1124"
				+ ",r11,55,2161,r12,181,1750,r13,73,1988,r14,200,2010,r15,223,2010"
				+ ",r16,438,1023,r17,65,1605,r18,150,3190";
		// Preprint 164, p20, Table 7: sink information in the material network
		// format: sink name, amount (kt per year) 
		String sinksAsString = "d1,35.3,d2,68.4,d3,400,d4,10,d5,38.8"
				+ ",d6,87.5,d7,412.5,d8,181,d9,28.2,d10,33.6"
				+ ",d11,411.5,d12,205.8,d13,292.5,d14,131.1,d15,42.6"
				+ ",d16,35,d17,202,d18,39.2,d19,10.3,d20,283.8"
				+ ",d21,12,d22,8.3,d23,9.9,d24,285.7,d25,27.8,d26,574.8";
		// Preprint 164, p18, Table 6: Distances between the potential connectable companies
		// format: source name, sink name, distance (km)
		String distancesAsString = "r2,d9,1.5,r2,d24,4,r3,d2,4,r3,d3,5.3,r3,d10,3.8,r3,d20,1.5,r4,d7,8,r4,d11,5,r4,d13,8,r5,d5,2"
				+ ",r6,d21,3.5,r7,d1,1,r7,d4,1.5,r8,d17,0.5,r9,d2,4,r9,d3,1.5,r9,d10,4,r9,d20,6.3,r10,d6,3.2,r10,d8,2.2,r10,d12,3.1,r10,d14,2.2,r10,d15,3,r10,d16,3"
				+ ",r11,d19,8,r11,d22,3,r12,d9,1.5,r12,d24,4,r13,d18,0.5,r14,d21,3.5,r15,d21,3.5"
				+ ",r16,d7,10.4,r16,d11,5.3,r16,d13,10.4,r17,d23,8,r18,d25,7,r18,d26,4";
		
		
		List<Source> sources = createSources(sourcesAsString);
		List<Sink> sinks = createSinks(sinksAsString);
		List<FeasibleConnection> connections = createFeasibleConnections(sources, sinks, distancesAsString);
		List<Transportation> transportations = createTransportations(TRANSPORTATIONS_AS_STRING);
		
		//addNearSeaInformation(sources, "r2,r12,r17,r18", sinks, "d23,d24,d26");
		addNearSeaInformation(sources, "r17,r18", sinks, "d23,d24,d26,d6");
	
		System.out.println("created " + sources.size() + " sources");
		System.out.println(sources);
		System.out.println("created " + sinks.size() + " sinks");
		System.out.println(sinks);
		
		MenGamsConverter converter = new MenGamsConverter();
		MenCalculationParameters parameters = new MenCalculationParameters(50., 1., 1., 1.05, false);
		MenResult result = converter.calculate(sources, sinks, connections, transportations, parameters);
		assertEquals(6.41811655E9, result.totalMaterialPurchaseCost, 1.);
		assertEquals(1062291, result.totalTransportationCost, 1.);
		assertEquals(1472.95734, result.totalCO2Emission, 1.); // in gramm 
	}
	
	
	/**
	 *  All sinks, sources and connections as in preprint 164. 
	 *  The near sea information is derived from fig. 9 of preprint 164.
	 *  Six calculation for different annual cost factors.
	 *  The results agree at least in a qualitative sense with table 9 from preprint 164
	 */
	public void test5bPreprint164WithVaryingAnnualCostFactors() {
		
		// Preprint 164, p20, Table 7: source information in the material network
		// format: source name, amount (kt per year), price (dollar per t) 
		String sourcesAsString = "r1,500,555,r2,250,1750,r3,1900,1254,r4,580,1364,r5,115,1700"
				+ ",r6,120,2010,r7,300,1510,r8,210,2072,r9,1010,1254,r10,1000,1124"
				+ ",r11,55,2161,r12,181,1750,r13,73,1988,r14,200,2010,r15,223,2010"
				+ ",r16,438,1023,r17,65,1605,r18,150,3190";
		// Preprint 164, p20, Table 7: sink information in the material network
		// format: sink name, amount (kt per year) 
		String sinksAsString = "d1,35.3,d2,68.4,d3,400,d4,10,d5,38.8"
				+ ",d6,87.5,d7,412.5,d8,181,d9,28.2,d10,33.6"
				+ ",d11,411.5,d12,205.8,d13,292.5,d14,131.1,d15,42.6"
				+ ",d16,35,d17,202,d18,39.2,d19,10.3,d20,283.8"
				+ ",d21,12,d22,8.3,d23,9.9,d24,285.7,d25,27.8,d26,574.8";
		// Preprint 164, p18, Table 6: Distances between the potential connectable companies
		// format: source name, sink name, distance (km)
		String distancesAsString = "r1,d20,6,r2,d9,1.5,r2,d24,4,r3,d2,4,r3,d3,5.3,r3,d10,3.8,r3,d20,1.5,r4,d7,8,r4,d11,5,r4,d13,8,r5,d5,2"
				+ ",r6,d21,3.5,r7,d1,1,r7,d4,1.5,r8,d17,0.5,r9,d2,4,r9,d3,1.5,r9,d10,4,r9,d20,6.3,r10,d6,3.2,r10,d8,2.2,r10,d12,3.1,r10,d14,2.2,r10,d15,3,r10,d16,3"
				+ ",r11,d19,8,r11,d22,3,r12,d9,1.5,r12,d24,4,r13,d18,0.5,r14,d21,3.5,r15,d21,3.5"
				+ ",r16,d7,10.4,r16,d11,5.3,r16,d13,10.4,r17,d23,8,r18,d25,7,r18,d26,4";
		
		List<Source> sources = createSources(sourcesAsString);
		List<Sink> sinks = createSinks(sinksAsString);
		List<FeasibleConnection> connections = createFeasibleConnections(sources, sinks, distancesAsString);
		List<Transportation> transportations = createTransportations(TRANSPORTATIONS_AS_STRING);

		addNearSeaInformation(sources, "r2,r12,r17,r18", sinks, "d23,d24,d26");
		
		
		//calculateMaterialPurchaseCostDirectly(sources, sinks, connections, 1.05);
		
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
		
		MenGamsConverter converter = new MenGamsConverter();
		for (int i=0; i<annualCostFactors.length; i++) {
			double factor = annualCostFactors[i];
			System.out.println("Annual Cost Factor =" + factor);
			MenCalculationParameters parameters = new MenCalculationParameters(50., 1., factor, 1.05, true);
			MenResult actual = converter.calculate(sources, sinks, connections, transportations, parameters);
			assertMenResult(expected[i], actual);
		}
	}
	

	
	
	private void assertMenResult(double[]  expected, MenResult actual) {
		assertEquals(expected[0], actual.objValue, 1000.);
		assertEquals(expected[1], actual.totalMaterialPurchaseCost, 1000.);
		assertEquals(expected[2], actual.totalMaterialPurchaseCostInternationalMarket, 1000.);
		assertEquals(expected[3], actual.totalTransportationCost, 1.);
		assertEquals(expected[4], actual.totalCO2Emission, 1.); // in gramm 
		assertEquals(expected[5], actual.totalInstallationCost, 1.);
	}
	
	private List<Source> createSources(String sources) {
		
		List<Source> result = new ArrayList<Source>();
		
		StringTokenizer tokenizer = new StringTokenizer(sources, ",");
		while (tokenizer.hasMoreTokens()) {	
			// we can use products with the same name here 
			// because the feasible connections are already given by the test case
			// that means we don't need the product name here to identify the feasible connections
			// however, we need the different capacities and prices for the optimization
			Product product = new Product("P");
			Source source = new Source(tokenizer.nextToken(), product);
			// convert from kt to t
			double capacity = 1000 * Double.valueOf(tokenizer.nextToken());
			product.setCapacity(capacity);
			double price = Double.valueOf(tokenizer.nextToken());
			product.setPrice(price);
			result.add(source);
		}
		
		System.out.println("created " + result.size() + " sources");
		System.out.println(result);
		
		return result;
	}
	
	private List<Sink> createSinks(String sinks) {
		
		List<Sink> result = new ArrayList<Sink>();
		
		StringTokenizer tokenizer = new StringTokenizer(sinks, ",");
		while (tokenizer.hasMoreTokens()) {	
			// we can use products with the same name here 
			// because the feasible connections are given by the test case
			// that means we don't need the product name here to identify the feasible connections
			// however, we need the different demands for the optimization
			Product product = new Product("P");
			Sink sink = new Sink(tokenizer.nextToken(), product);
			// convert from kt to t
			double demand = 1000 * Double.valueOf(tokenizer.nextToken());
			product.setCapacity(demand);
			result.add(sink);
		}
		
		System.out.println("created " + result.size() + " sinks");
		System.out.println(result);
		
		return result;
	}
	
	private Source findSource(List<Source> list, String name) {
		for (Source current : list) {
			if (current.getName().equals(name)) {
				return current;
			}
		}
		return null;
	}
	
	private Sink findSink(List<Sink> list, String name) {
		for (Sink current : list) {
			if (current.getName().equals(name)) {
				return current;
			}
		}
		return null;
	}
	
	private List<FeasibleConnection> createFeasibleConnections(List<Source> sources, List<Sink> sinks, String distances) {
		
		List<FeasibleConnection> result = new ArrayList<FeasibleConnection>();
		
		StringTokenizer tokenizer = new StringTokenizer(distances, ",");
		while (tokenizer.hasMoreTokens()) {	
			
			String sourceToken = tokenizer.nextToken();
			Source source = findSource(sources, sourceToken);
			String sinkToken = tokenizer.nextToken();
			Sink sink = findSink(sinks, sinkToken);
			if ((source != null) && (sink != null)) {
				FeasibleConnection connection = new FeasibleConnection(source, sink);
				float distance = Float.valueOf(tokenizer.nextToken());
				connection.setDistance(distance);
				result.add(connection);
			} else {
				System.out.println("error: no feasible connection was created for source = " + sourceToken + ", sink = " + sinkToken);
			}
		}
		
		System.out.println("created " + result.size() + " connections");
		System.out.println(result);
		
		return result;
	}
	
	private List<Transportation> createTransportations(String transportations) {
		
		List<Transportation> result = new ArrayList<Transportation>();
		
		StringTokenizer tokenizer = new StringTokenizer(transportations, ",");
		while (tokenizer.hasMoreTokens()) {	
			Transportation trans = new Transportation(tokenizer.nextToken());
			double value = Double.valueOf(tokenizer.nextToken());
			trans.setTransportationCost(value);
			value = Double.valueOf(tokenizer.nextToken());
			trans.setInstallationCost(value);
			value = Double.valueOf(tokenizer.nextToken());
			trans.setEmission(value);
			result.add(trans);
		}
		
		System.out.println("created " + result.size() + " transportations");
		System.out.println(result);
		
		return result;
	}
	
	/**
	 * The 'near sea' information is not given explicitly in preprint No 164.
	 * But Figure 9 on page 25 shows the optimization result for 100-year life time.
	 * For each connection the figure shows what transportation was selected.
	 * We use this information as follows: <br>
	 * <br>
	 * 1. If a sink is reached by ships (short-sea transportation), then it is tagged as "near sea"<br>
	 * 2. If a source uses ships (short-sea transportation), then it is tagged as "near sea"<br>
	 * 3. For the optimization algorithm, transportation type 'ship' is only allowed, if both source and sink are near sea.<br>
	 * @param sources
	 * @param sinks
	 */
	private void addNearSeaInformation(List<Source> sources, String sourcesNearSea, List<Sink> sinks, String sinksNearSea) {

		StringTokenizer tokenizer = new StringTokenizer(sourcesNearSea, ",");
		while (tokenizer.hasMoreTokens()) {	
			String current = tokenizer.nextToken();
			findSource(sources, current).setNearSea(true);
		}
		
		tokenizer = new StringTokenizer(sinksNearSea, ",");
		while (tokenizer.hasMoreTokens()) {	
			String current = tokenizer.nextToken();
			findSink(sinks, current).setNearSea(true);
		}
	}
	
	private MenCalculationParameters createDefaultParameters() {
		return new MenCalculationParameters(50., 1., 1., 1.05, false);
	}
	
	private double calculateMaterialPurchaseCostDirectly(List<Source> sources, List<Sink> sinks, List<FeasibleConnection> connections, 
			double internationalMarketPriceFactor, boolean internationalMarketLowestPrice) {
		
		double totalCost = 0;
		double totalTransportCost = 0;
		
		// copy sources because the sink capacity is reduced during cost calculation
		List<Source> copiedSources = new ArrayList<Source>();
		for (Source source : sources) {
			Product product = source.getProduct();
			Product copiedProduct = new Product(source.getProduct().getName());
			copiedProduct.setCapacity(product.getCapacity());
			copiedProduct.setPrice(product.getPrice());
			Source copiedSource = new Source(source.getName(), copiedProduct);
			copiedSource.setNearSea(source.isNearSea());
			copiedSources.add(copiedSource);
		}	
		
		// for each sink try to buy from the source with the best price
		for (Sink sink : sinks) {
			
			double costForSink = 0.;
			double transportCost = 0.;
			
			double sinkDemand = sink.getProduct().getCapacity();
			List<FeasibleConnection> orderedConnections = MenGamsConverter.orderByPriceAscending(copiedSources, sink, connections);
			
			for (FeasibleConnection currentConn : orderedConnections) {
				Source copiedSource = findSource(copiedSources, currentConn.getSource().getName());
				Product sourceProduct = copiedSource.getProduct();
				double price = sourceProduct.getPrice();
				double capacity = sourceProduct.getCapacity();
				if (sinkDemand <= capacity) {
					costForSink += sinkDemand * price;
					sourceProduct.setCapacity(capacity - sinkDemand);
					transportCost += sinkDemand * currentConn.getDistance();
					sinkDemand = 0.;
					break;
				} else {
					costForSink += capacity * price;
					sourceProduct.setCapacity(0.);
					transportCost += capacity * currentConn.getDistance();
					sinkDemand -= capacity;
				}
			}
			
			// buy remaining demand from international markets
			if (sinkDemand > 0) {
				//System.out.println("buy from international markets for sink = " + sink + ", remaining demand = " + sinkDemand);
				
				double price = MenGamsConverter.getPriceForInternationalMarket(copiedSources, sink, connections, internationalMarketPriceFactor, internationalMarketLowestPrice);
				costForSink += price * sinkDemand;
			}
						
			System.out.println("cost for sink " + sink.getName() + " = " + costForSink);
			System.out.println("transport cost for sink " + sink.getName() + " = " + transportCost);
			
			totalCost += costForSink;
			totalTransportCost += transportCost;
		}
		
		System.out.println("total cost = " + totalCost);
		
		// only for simple examples with truck transport only
		totalTransportCost = totalTransportCost * 0.1;
		System.out.println("total transportation cost = " + totalTransportCost);
		
		return totalCost;
	}
}
