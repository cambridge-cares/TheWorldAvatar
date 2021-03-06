package uk.ac.cam.cares.jps.virtualsensor.agents.test;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import org.json.JSONArray;
import org.json.JSONObject;
import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.region.Region;
import uk.ac.cam.cares.jps.base.region.Scope;
import uk.ac.cam.cares.jps.virtualsensor.episode.OldEpisodeAgent;
import uk.ac.cam.cares.jps.virtualsensor.sparql.DispSimSparql;

public class EpisodeAgentTest extends TestCase {
	
	String stationiri1 = "http://www.theworldavatar.com/kb/sgp/singapore/WeatherStation-009.owl#WeatherStation-009";
	String stationiri2 = "http://www.theworldavatar.com/kb/sgp/singapore/WeatherStation-006.owl#WeatherStation-006";
	String jsonparamsample2 = "{\"waste\":[\"http://localhost:8080/kb/ships/563030060/Chimney-1.owl#WasteStreamOfChimney-1\",\"http://localhost:8080/kb/ships/563040773/Chimney-1.owl#WasteStreamOfChimney-1\",\"http://localhost:8080/kb/ships/563069130/Chimney-1.owl#WasteStreamOfChimney-1\",\"http://localhost:8080/kb/ships/563040083/Chimney-1.owl#WasteStreamOfChimney-1\",\"http://localhost:8080/kb/ships/563040564/Chimney-1.owl#WasteStreamOfChimney-1\",\"http://localhost:8080/kb/ships/525016710/Chimney-1.owl#WasteStreamOfChimney-1\",\"http://localhost:8080/kb/ships/563032010/Chimney-1.owl#WasteStreamOfChimney-1\",\"http://localhost:8080/kb/ships/563040864/Chimney-1.owl#WasteStreamOfChimney-1\",\"http://localhost:8080/kb/ships/563069680/Chimney-1.owl#WasteStreamOfChimney-1\",\"http://localhost:8080/kb/ships/563023580/Chimney-1.owl#WasteStreamOfChimney-1\",\"http://localhost:8080/kb/ships/563040290/Chimney-1.owl#WasteStreamOfChimney-1\",\"http://localhost:8080/kb/ships/563040915/Chimney-1.owl#WasteStreamOfChimney-1\",\"http://localhost:8080/kb/ships/563021360/Chimney-1.owl#WasteStreamOfChimney-1\",\"http://localhost:8080/kb/ships/563066440/Chimney-1.owl#WasteStreamOfChimney-1\",\"http://localhost:8080/kb/ships/240201000/Chimney-1.owl#WasteStreamOfChimney-1\",\"http://localhost:8080/kb/ships/563037140/Chimney-1.owl#WasteStreamOfChimney-1\",\"http://localhost:8080/kb/ships/542249110/Chimney-1.owl#WasteStreamOfChimney-1\",\"http://localhost:8080/kb/ships/563063780/Chimney-1.owl#WasteStreamOfChimney-1\",\"http://localhost:8080/kb/ships/563069630/Chimney-1.owl#WasteStreamOfChimney-1\",\"http://localhost:8080/kb/ships/563033510/Chimney-1.owl#WasteStreamOfChimney-1\",\"http://localhost:8080/kb/ships/563068490/Chimney-1.owl#WasteStreamOfChimney-1\",\"http://localhost:8080/kb/ships/563063730/Chimney-1.owl#WasteStreamOfChimney-1\",\"http://localhost:8080/kb/ships/563041250/Chimney-1.owl#WasteStreamOfChimney-1\",\"http://localhost:8080/kb/ships/563041068/Chimney-1.owl#WasteStreamOfChimney-1\",\"http://localhost:8080/kb/ships/563040863/Chimney-1.owl#WasteStreamOfChimney-1\",\"http://localhost:8080/kb/ships/563066990/Chimney-1.owl#WasteStreamOfChimney-1\",\"http://localhost:8080/kb/ships/563040088/Chimney-1.owl#WasteStreamOfChimney-1\",\"http://localhost:8080/kb/ships/563026440/Chimney-1.owl#WasteStreamOfChimney-1\",\"http://localhost:8080/kb/ships/563015610/Chimney-1.owl#WasteStreamOfChimney-1\",\"http://localhost:8080/kb/ships/636018529/Chimney-1.owl#WasteStreamOfChimney-1\",\"http://localhost:8080/kb/ships/538005714/Chimney-1.owl#WasteStreamOfChimney-1\",\"http://localhost:8080/kb/ships/244830813/Chimney-1.owl#WasteStreamOfChimney-1\",\"http://localhost:8080/kb/ships/563067570/Chimney-1.owl#WasteStreamOfChimney-1\",\"http://localhost:8080/kb/ships/563019660/Chimney-1.owl#WasteStreamOfChimney-1\",\"http://localhost:8080/kb/ships/563004925/Chimney-1.owl#WasteStreamOfChimney-1\",\"http://localhost:8080/kb/ships/563044320/Chimney-1.owl#WasteStreamOfChimney-1\",\"http://localhost:8080/kb/ships/563041385/Chimney-1.owl#WasteStreamOfChimney-1\",\"http://localhost:8080/kb/ships/563041046/Chimney-1.owl#WasteStreamOfChimney-1\",\"http://localhost:8080/kb/ships/563040561/Chimney-1.owl#WasteStreamOfChimney-1\",\"http://localhost:8080/kb/ships/563040756/Chimney-1.owl#WasteStreamOfChimney-1\",\"http://localhost:8080/kb/ships/563051840/Chimney-1.owl#WasteStreamOfChimney-1\",\"http://localhost:8080/kb/ships/563018030/Chimney-1.owl#WasteStreamOfChimney-1\",\"http://localhost:8080/kb/ships/563021420/Chimney-1.owl#WasteStreamOfChimney-1\"],\"agent\":\"http://www.theworldavatar.com/kb/agents/Service__ComposedEpisode.owl#Service\",\"reactionmechanism\":\"http://www.theworldavatar.com/kb/ontokin/Reduced_PRF_ERC_particle.owl#ReactionMechanism_184144363244001\",\"city\":\"http://dbpedia.org/resource/Singapore\",\"ship\":{\"collection\":{\"items\":[{\"ss\":21.1,\"country\":\"Singapore\",\"mmsi\":563030060,\"heading\":511,\"imo\":0,\"lon\":103.86467,\"ship_mmsi\":563030060,\"al\":27,\"type\":\"Pilot\",\"dest\":\"\",\"gt\":0,\"cu\":87.8,\"r\":2,\"dw\":0,\"draught\":0,\"etaTS\":0,\"tst\":1587630621,\"aw\":9,\"name\":\"PILOT GP61\",\"y\":0,\"sl\":false,\"id\":28412001,\"lat\":1.25984,\"ts\":1587630716},{\"ss\":15.7,\"country\":\"Singapore\",\"mmsi\":563040773,\"imo\":0,\"lon\":103.86873,\"ship_mmsi\":563040773,\"al\":0,\"type\":\"Other type\",\"dest\":\"\",\"gt\":0,\"cu\":237.8,\"r\":2,\"dw\":0,\"draught\":0,\"etaTS\":0,\"aw\":0,\"name\":\"CAST EXPLORER\",\"y\":0,\"sl\":false,\"id\":28414611,\"lat\":1.27345,\"ts\":1587631937},{\"ss\":12.9,\"country\":\"Singapore\",\"mmsi\":563069130,\"heading\":511,\"imo\":0,\"lon\":103.87015,\"ship_mmsi\":563069130,\"al\":1022,\"type\":\"HSC\",\"dest\":\"\",\"gt\":0,\"cu\":273.9,\"r\":2,\"dw\":0,\"draught\":0,\"etaTS\":0,\"tst\":1587630622,\"aw\":126,\"name\":\"PRESIDENT FIVE\",\"y\":0,\"sl\":false,\"id\":28411844,\"lat\":1.26263,\"ts\":1587630652},{\"ss\":5.7,\"country\":\"Singapore\",\"mmsi\":563040083,\"heading\":511,\"imo\":0,\"lon\":103.86884,\"ship_mmsi\":563040083,\"al\":0,\"type\":\"Pleasure craft\",\"dest\":\"\",\"gt\":0,\"cu\":231.5,\"r\":2,\"dw\":0,\"draught\":0,\"etaTS\":0,\"tst\":1587630475,\"aw\":0,\"name\":\"I-MARLIN\",\"y\":0,\"sl\":false,\"id\":28411876,\"lat\":1.27345,\"ts\":1587630659},{\"ss\":0.7,\"country\":\"Singapore\",\"mmsi\":563040564,\"imo\":0,\"lon\":103.8668,\"ship_mmsi\":563040564,\"al\":10,\"type\":\"Pleasure craft\",\"dest\":\"\",\"gt\":0,\"cu\":53.1,\"r\":2,\"dw\":0,\"draught\":0,\"etaTS\":0,\"aw\":10,\"name\":\"YORK 90\",\"y\":0,\"sl\":false,\"id\":28413874,\"lat\":1.27066,\"ts\":1587631534},{\"ss\":0.6,\"country\":\"Indonesia\",\"mmsi\":525016710,\"heading\":511,\"imo\":0,\"lon\":103.8736,\"ship_mmsi\":525016710,\"al\":21,\"type\":\"Cargo ship\",\"dest\":\"\",\"gt\":0,\"cu\":168,\"r\":2,\"dw\":0,\"draught\":0,\"etaTS\":0,\"tst\":1587630614,\"aw\":6,\"name\":\"DIJAYA-10\",\"y\":0,\"sl\":false,\"id\":28411837,\"lat\":1.26742,\"ts\":1587630707},{\"ss\":0.3,\"country\":\"Singapore\",\"mmsi\":563032010,\"imo\":0,\"lon\":103.86685,\"ship_mmsi\":563032010,\"al\":33,\"type\":\"Tug\",\"dest\":\"\",\"gt\":0,\"cu\":357,\"r\":2,\"dw\":0,\"draught\":0,\"etaTS\":0,\"aw\":6,\"name\":\"MV PRINCESS\",\"y\":0,\"sl\":false,\"id\":28414277,\"lat\":1.27007,\"ts\":1587630682},{\"ss\":0.3,\"country\":\"Singapore\",\"mmsi\":563040864,\"imo\":0,\"lon\":103.85478,\"ship_mmsi\":563040864,\"al\":10,\"type\":\"Pleasure craft\",\"dest\":\"\",\"gt\":0,\"cu\":360,\"r\":2,\"dw\":0,\"draught\":0,\"etaTS\":0,\"aw\":10,\"name\":\"SC 1865 E\",\"y\":0,\"sl\":false,\"id\":28413502,\"lat\":1.27046,\"ts\":1587630739},{\"ss\":0.2,\"country\":\"Singapore\",\"mmsi\":563069680,\"heading\":511,\"imo\":0,\"lon\":103.86345,\"ship_mmsi\":563069680,\"al\":1022,\"type\":\"Cargo ship\",\"dest\":\"\",\"gt\":0,\"cu\":0,\"r\":2,\"dw\":0,\"draught\":0,\"etaTS\":0,\"tst\":1587630601,\"aw\":126,\"name\":\"SUN BIS\",\"y\":0,\"sl\":false,\"id\":28412036,\"lat\":1.26997,\"ts\":1587630728},{\"ss\":0.2,\"country\":\"Singapore\",\"mmsi\":563023580,\"imo\":0,\"lon\":103.87122,\"ship_mmsi\":563023580,\"al\":14,\"type\":\"Dredging or UW ops\",\"dest\":\"\",\"gt\":0,\"cu\":66.2,\"r\":2,\"dw\":0,\"draught\":0,\"etaTS\":0,\"aw\":4,\"name\":\"TTB STINGER\",\"y\":0,\"sl\":false,\"id\":28414466,\"lat\":1.27389,\"ts\":1587631774},{\"ss\":0.2,\"country\":\"Singapore\",\"mmsi\":563040290,\"imo\":0,\"lon\":103.86498,\"ship_mmsi\":563040290,\"al\":10,\"type\":\"Pleasure craft\",\"dest\":\"\",\"gt\":0,\"cu\":13.7,\"r\":2,\"dw\":0,\"draught\":0,\"etaTS\":0,\"aw\":10,\"name\":\"CKL XI\",\"y\":0,\"sl\":false,\"id\":28413209,\"lat\":1.26896,\"ts\":1587631177},{\"ss\":0.2,\"country\":\"Singapore\",\"mmsi\":563040915,\"imo\":0,\"lon\":103.86676,\"ship_mmsi\":563040915,\"al\":10,\"type\":\"Pleasure craft\",\"dest\":\"\",\"gt\":0,\"cu\":0,\"r\":2,\"dw\":0,\"draught\":0,\"etaTS\":0,\"aw\":10,\"name\":\"AKM 7\",\"y\":0,\"sl\":false,\"id\":28413492,\"lat\":1.27014,\"ts\":1587631037},{\"ss\":0.2,\"country\":\"Singapore\",\"mmsi\":563021360,\"imo\":0,\"lon\":103.86549,\"ship_mmsi\":563021360,\"al\":10,\"type\":\"Passenger ship\",\"dest\":\"\",\"gt\":0,\"cu\":189,\"r\":2,\"dw\":0,\"draught\":0,\"etaTS\":0,\"aw\":6,\"name\":\"SC 4500 H\",\"y\":0,\"sl\":false,\"id\":28413097,\"lat\":1.26983,\"ts\":1587630704},{\"ss\":0.1,\"country\":\"Singapore\",\"mmsi\":563066440,\"heading\":511,\"imo\":0,\"lon\":103.86565,\"ship_mmsi\":563066440,\"al\":1022,\"type\":\"Unknown\",\"dest\":\"\",\"gt\":0,\"cu\":0,\"r\":2,\"dw\":0,\"draught\":0,\"etaTS\":0,\"tst\":1587633833,\"aw\":126,\"name\":\"VICTORIA EXPRESS\",\"y\":0,\"sl\":false,\"id\":28414962,\"lat\":1.2698,\"ts\":1587633838},{\"ss\":0.1,\"country\":\"Greece\",\"mmsi\":240201000,\"heading\":125,\"imo\":9325453,\"lon\":103.86867,\"ship_mmsi\":240201000,\"al\":50,\"type\":\"Passenger (Cruise) Ship\",\"dest\":\"SINGAPORE\",\"gt\":498,\"cu\":275,\"r\":2,\"dw\":185,\"draught\":43,\"etaTS\":1586419200,\"tst\":1587630531,\"aw\":11,\"name\":\"PAN ORAMA II\",\"y\":2004,\"sl\":false,\"id\":28411892,\"lat\":1.2675,\"ts\":1587630715},{\"ss\":0.1,\"country\":\"Singapore\",\"mmsi\":563037140,\"heading\":511,\"imo\":0,\"lon\":103.85457,\"ship_mmsi\":563037140,\"al\":50,\"type\":\"Passenger ship\",\"dest\":\"\",\"gt\":0,\"cu\":39.7,\"r\":2,\"dw\":0,\"draught\":0,\"etaTS\":0,\"tst\":1587630622,\"aw\":6,\"name\":\"GOLDEN STENA\",\"y\":0,\"sl\":false,\"id\":28411930,\"lat\":1.27065,\"ts\":1587630713},{\"ss\":0.1,\"country\":\"Niue\",\"mmsi\":542249110,\"heading\":511,\"imo\":0,\"lon\":103.85407,\"ship_mmsi\":542249110,\"al\":24,\"type\":\"Cargo ship\",\"dest\":\"\",\"gt\":0,\"cu\":360,\"r\":2,\"dw\":0,\"draught\":0,\"etaTS\":0,\"tst\":1587630621,\"aw\":6,\"name\":\"SEA GULL\",\"y\":0,\"sl\":false,\"id\":28411872,\"lat\":1.27049,\"ts\":1587630706},{\"ss\":0.1,\"country\":\"Singapore\",\"mmsi\":563063780,\"heading\":511,\"imo\":0,\"lon\":103.86499,\"ship_mmsi\":563063780,\"al\":15,\"type\":\"Pleasure craft\",\"dest\":\"\",\"gt\":0,\"cu\":0,\"r\":2,\"dw\":0,\"draught\":0,\"etaTS\":0,\"tst\":1587630548,\"aw\":5,\"name\":\"AH PENG\",\"y\":0,\"sl\":false,\"id\":28411954,\"lat\":1.26937,\"ts\":1587630713},{\"ss\":0.1,\"country\":\"Singapore\",\"mmsi\":563069630,\"heading\":511,\"imo\":0,\"lon\":103.86577,\"ship_mmsi\":563069630,\"al\":15,\"type\":\"Passenger ship\",\"dest\":\"\",\"gt\":0,\"cu\":0,\"r\":2,\"dw\":0,\"draught\":0,\"etaTS\":0,\"tst\":1587630466,\"aw\":5,\"name\":\"LES SWIFT\",\"y\":0,\"sl\":false,\"id\":28411749,\"lat\":1.27017,\"ts\":1587630637},{\"ss\":0.1,\"country\":\"Singapore\",\"mmsi\":563033510,\"heading\":511,\"imo\":0,\"lon\":103.86653,\"ship_mmsi\":563033510,\"al\":15,\"type\":\"Passenger ship\",\"dest\":\"\",\"gt\":0,\"cu\":107.5,\"r\":2,\"dw\":0,\"draught\":0,\"etaTS\":0,\"tst\":1587633788,\"aw\":4,\"name\":\"SC4721C\",\"y\":0,\"sl\":false,\"id\":28414963,\"lat\":1.27009,\"ts\":1587633793},{\"ss\":0.1,\"country\":\"Singapore\",\"mmsi\":563068490,\"heading\":511,\"imo\":0,\"lon\":103.86699,\"ship_mmsi\":563068490,\"al\":14,\"type\":\"Pleasure craft\",\"dest\":\"\",\"gt\":0,\"cu\":0,\"r\":2,\"dw\":0,\"draught\":0,\"etaTS\":0,\"tst\":1587630558,\"aw\":6,\"name\":\"SEA FALCON 15\",\"y\":0,\"sl\":false,\"id\":28411854,\"lat\":1.26991,\"ts\":1587630710},{\"ss\":0.1,\"country\":\"Singapore\",\"mmsi\":563063730,\"imo\":0,\"lon\":103.86704,\"ship_mmsi\":563063730,\"al\":14,\"type\":\"Pleasure craft\",\"dest\":\"\",\"gt\":0,\"cu\":0,\"r\":2,\"dw\":0,\"draught\":0,\"etaTS\":0,\"aw\":5,\"name\":\"SEA FALCON 6\",\"y\":0,\"sl\":false,\"id\":28413933,\"lat\":1.26965,\"ts\":1587631075},{\"ss\":0.1,\"country\":\"Singapore\",\"mmsi\":563041250,\"imo\":0,\"lon\":103.86443,\"ship_mmsi\":563041250,\"al\":10,\"type\":\"Pleasure craft\",\"dest\":\"\",\"gt\":0,\"cu\":0,\"r\":2,\"dw\":0,\"draught\":0,\"etaTS\":0,\"aw\":10,\"name\":\"HLBS8\",\"y\":0,\"sl\":false,\"id\":28414037,\"lat\":1.26908,\"ts\":1587631100},{\"ss\":0.1,\"country\":\"Singapore\",\"mmsi\":563041068,\"imo\":0,\"lon\":103.86482,\"ship_mmsi\":563041068,\"al\":10,\"type\":\"Pleasure craft\",\"dest\":\"\",\"gt\":0,\"cu\":0,\"r\":2,\"dw\":0,\"draught\":0,\"etaTS\":0,\"aw\":10,\"name\":\"SC 4878 C\",\"y\":0,\"sl\":false,\"id\":28413867,\"lat\":1.26937,\"ts\":1587631105},{\"ss\":0.1,\"country\":\"Singapore\",\"mmsi\":563040863,\"imo\":0,\"lon\":103.85721,\"ship_mmsi\":563040863,\"al\":10,\"type\":\"Pleasure craft\",\"dest\":\"\",\"gt\":0,\"cu\":0,\"r\":2,\"dw\":0,\"draught\":0,\"etaTS\":0,\"aw\":10,\"name\":\"SC1057C\",\"y\":0,\"sl\":false,\"id\":28413495,\"lat\":1.26971,\"ts\":1587631099},{\"ss\":0.1,\"country\":\"Singapore\",\"mmsi\":563066990,\"heading\":511,\"imo\":0,\"lon\":103.86696,\"ship_mmsi\":563066990,\"al\":0,\"type\":\"Cargo ship\",\"dest\":\"\",\"gt\":0,\"cu\":0,\"r\":2,\"dw\":0,\"draught\":0,\"etaTS\":0,\"tst\":1587630568,\"aw\":0,\"name\":\"SEA FALCON 11\",\"y\":0,\"sl\":false,\"id\":28412027,\"lat\":1.26971,\"ts\":1587630726},{\"ss\":0.1,\"country\":\"Singapore\",\"mmsi\":563040088,\"imo\":0,\"lon\":103.86614,\"ship_mmsi\":563040088,\"al\":0,\"type\":\"Pleasure craft\",\"dest\":\"\",\"gt\":0,\"cu\":0,\"r\":2,\"dw\":0,\"draught\":0,\"etaTS\":0,\"aw\":0,\"name\":\"YORK 72\",\"y\":0,\"sl\":false,\"id\":28414051,\"lat\":1.2701,\"ts\":1587630890},{\"ss\":0,\"country\":\"Singapore\",\"mmsi\":563026440,\"imo\":0,\"lon\":103.86713,\"ship_mmsi\":563026440,\"al\":1022,\"type\":\"Port tender\",\"dest\":\"\",\"gt\":0,\"cu\":360,\"r\":2,\"dw\":0,\"draught\":0,\"etaTS\":0,\"aw\":126,\"name\":\"SEASWIFT PRISTINE\",\"y\":0,\"sl\":false,\"id\":28414397,\"lat\":1.27041,\"ts\":1587630983},{\"ss\":0,\"country\":\"Singapore\",\"mmsi\":563015610,\"imo\":0,\"lon\":103.86655,\"ship_mmsi\":563015610,\"al\":1022,\"type\":\"Pleasure craft\",\"dest\":\"\",\"gt\":0,\"cu\":341,\"r\":2,\"dw\":0,\"draught\":0,\"etaTS\":0,\"aw\":126,\"name\":\"LCW 5\",\"y\":0,\"sl\":false,\"id\":28413808,\"lat\":1.27055,\"ts\":1587630699},{\"ss\":0,\"country\":\"Liberia\",\"mmsi\":636018529,\"heading\":257,\"imo\":9357535,\"lon\":103.87367,\"ship_mmsi\":636018529,\"al\":175,\"type\":\"Container Ship\",\"dest\":\"SGSIN PEBGA\",\"gt\":18123,\"cu\":189,\"r\":2,\"dw\":22314,\"draught\":69,\"etaTS\":1587338100,\"tst\":1587630371,\"aw\":28,\"name\":\"MCC SEOUL\",\"y\":2008,\"sl\":false,\"id\":28411960,\"lat\":1.25883,\"ts\":1587630738},{\"ss\":0,\"country\":\"Marshall Islands\",\"mmsi\":538005714,\"heading\":120,\"imo\":9495222,\"lon\":103.87044,\"ship_mmsi\":538005714,\"al\":91,\"type\":\"Offshore Tug/Supply Ship\",\"dest\":\"\",\"gt\":7534,\"cu\":201.4,\"r\":2,\"dw\":4500,\"draught\":73,\"etaTS\":0,\"tst\":1587630501,\"aw\":22,\"name\":\"BOKA PERSEUS\",\"y\":2015,\"sl\":false,\"id\":28411940,\"lat\":1.26602,\"ts\":1587630689},{\"ss\":0,\"country\":\"Netherlands\",\"mmsi\":244830813,\"heading\":87,\"imo\":9344978,\"lon\":103.87395,\"ship_mmsi\":244830813,\"al\":58,\"type\":\"Tug\",\"dest\":\"\",\"gt\":1767,\"cu\":92,\"r\":2,\"dw\":1494,\"draught\":63,\"etaTS\":0,\"tst\":1587630515,\"aw\":15,\"name\":\"ALP IPPON\",\"y\":2007,\"sl\":false,\"id\":28411999,\"lat\":1.27088,\"ts\":1587630700},{\"ss\":0,\"country\":\"Singapore\",\"mmsi\":563067570,\"heading\":511,\"imo\":0,\"lon\":103.86613,\"ship_mmsi\":563067570,\"al\":18,\"type\":\"Passenger ship\",\"dest\":\"\",\"gt\":0,\"cu\":0,\"r\":2,\"dw\":0,\"draught\":0,\"etaTS\":0,\"tst\":1587630617,\"aw\":6,\"name\":\"MSF NATSU\",\"y\":0,\"sl\":false,\"id\":28411939,\"lat\":1.27025,\"ts\":1587630684},{\"ss\":0,\"country\":\"Singapore\",\"mmsi\":563019660,\"heading\":511,\"imo\":0,\"lon\":103.86567,\"ship_mmsi\":563019660,\"al\":15,\"type\":\"Pleasure craft\",\"dest\":\"\",\"gt\":0,\"cu\":0,\"r\":2,\"dw\":0,\"draught\":0,\"etaTS\":0,\"tst\":1587630474,\"aw\":4,\"name\":\"CKL X\",\"y\":0,\"sl\":false,\"id\":28411891,\"lat\":1.26916,\"ts\":1587630720},{\"ss\":0,\"country\":\"Singapore\",\"mmsi\":563004925,\"heading\":511,\"imo\":0,\"lon\":103.86457,\"ship_mmsi\":563004925,\"al\":15,\"type\":\"Passenger ship\",\"dest\":\"\",\"gt\":0,\"cu\":0,\"r\":2,\"dw\":0,\"draught\":0,\"etaTS\":0,\"tst\":1587630491,\"aw\":3,\"name\":\"CKL XV\",\"y\":0,\"sl\":false,\"id\":28411855,\"lat\":1.26882,\"ts\":1587630647},{\"ss\":0,\"country\":\"Singapore\",\"mmsi\":563044320,\"imo\":0,\"lon\":103.85471,\"ship_mmsi\":563044320,\"al\":10,\"type\":\"Pleasure craft\",\"dest\":\"\",\"gt\":0,\"cu\":0,\"r\":2,\"dw\":0,\"draught\":0,\"etaTS\":0,\"aw\":10,\"name\":\"KIM POWER\",\"y\":0,\"sl\":false,\"id\":28413624,\"lat\":1.2704,\"ts\":1587630866},{\"ss\":0,\"country\":\"Singapore\",\"mmsi\":563041385,\"imo\":0,\"lon\":103.865,\"ship_mmsi\":563041385,\"al\":10,\"type\":\"Pleasure craft\",\"dest\":\"\",\"gt\":0,\"cu\":0,\"r\":2,\"dw\":0,\"draught\":0,\"etaTS\":0,\"aw\":10,\"name\":\"SP KING 14\",\"y\":0,\"sl\":false,\"id\":28413425,\"lat\":1.26922,\"ts\":1587631263},{\"ss\":0,\"country\":\"Singapore\",\"mmsi\":563041046,\"imo\":0,\"lon\":103.86536,\"ship_mmsi\":563041046,\"al\":10,\"type\":\"Pleasure craft\",\"dest\":\"\",\"gt\":0,\"cu\":360,\"r\":2,\"dw\":0,\"draught\":0,\"etaTS\":0,\"aw\":10,\"name\":\"PRESTIGE 1\",\"y\":0,\"sl\":false,\"id\":28413508,\"lat\":1.26914,\"ts\":1587630891},{\"ss\":0,\"country\":\"Singapore\",\"mmsi\":563040561,\"imo\":0,\"lon\":103.86348,\"ship_mmsi\":563040561,\"al\":10,\"type\":\"Pleasure craft\",\"dest\":\"\",\"gt\":0,\"cu\":360,\"r\":2,\"dw\":0,\"draught\":0,\"etaTS\":0,\"aw\":10,\"name\":\"YORK 206\",\"y\":0,\"sl\":false,\"id\":28413642,\"lat\":1.26988,\"ts\":1587631020},{\"ss\":0,\"country\":\"Singapore\",\"mmsi\":563040756,\"imo\":0,\"lon\":103.86673,\"ship_mmsi\":563040756,\"al\":10,\"type\":\"Pleasure craft\",\"dest\":\"\",\"gt\":0,\"cu\":360,\"r\":2,\"dw\":0,\"draught\":0,\"etaTS\":0,\"aw\":10,\"name\":\"SC4433H\",\"y\":0,\"sl\":false,\"id\":28413971,\"lat\":1.26966,\"ts\":1587631262},{\"ss\":0,\"country\":\"Singapore\",\"mmsi\":563051840,\"imo\":0,\"lon\":103.85695,\"ship_mmsi\":563051840,\"al\":10,\"type\":\"Pleasure craft\",\"dest\":\"\",\"gt\":0,\"cu\":0,\"r\":2,\"dw\":0,\"draught\":0,\"etaTS\":0,\"aw\":10,\"name\":\"FUJI II\",\"y\":0,\"sl\":false,\"id\":28413915,\"lat\":1.26902,\"ts\":1587631468},{\"ss\":0,\"country\":\"Singapore\",\"mmsi\":563018030,\"imo\":0,\"lon\":103.86718,\"ship_mmsi\":563018030,\"al\":0,\"type\":\"Pleasure craft\",\"dest\":\"\",\"gt\":0,\"cu\":360,\"r\":2,\"dw\":0,\"draught\":0,\"etaTS\":0,\"aw\":0,\"name\":\"SEASWIFT DRAGON\",\"y\":0,\"sl\":false,\"id\":28413424,\"lat\":1.2702,\"ts\":1587631150},{\"ss\":0,\"country\":\"Singapore\",\"mmsi\":563021420,\"imo\":0,\"lon\":103.85448,\"ship_mmsi\":563021420,\"al\":0,\"type\":\"Tug\",\"dest\":\"\",\"gt\":0,\"cu\":0,\"r\":2,\"dw\":0,\"draught\":0,\"etaTS\":0,\"aw\":0,\"name\":\"SEA EFA SC4356J\",\"y\":0,\"sl\":false,\"id\":28414620,\"lat\":1.27045,\"ts\":1587632193}]}},\"region\":{\"uppercorner\":{\"uppery\":\"143305.896\",\"upperx\":\"11563323.926\"},\"srsname\":\"EPSG:3857\",\"lowercorner\":{\"lowery\":\"140107.739\",\"lowerx\":\"11560879.832\"}},\"stationiri\":[\"http://www.theworldavatar.com/kb/sgp/singapore/WeatherStation-008.owl#WeatherStation-008\",\"http://www.theworldavatar.com/kb/sgp/singapore/WeatherStation-006.owl#WeatherStation-006\"],\"building\":[\"http://www.theworldavatar.com/kb/sgp/singapore/buildings/GEOJSON_MARINA_BAY.owl#BuildingB5a9d9379-6f95-4180-b60d-424e65f97c54\",\"http://www.theworldavatar.com/kb/sgp/singapore/buildings/GEOJSON_MARINA_BAY.owl#BuildingBe1dd54a2-a189-4397-91d0-0e4e5f392d15\",\"http://www.theworldavatar.com/kb/sgp/singapore/buildings/GEOJSON_MARINA_BAY.owl#BuildingBc8c74eb8-999c-462e-aca7-c1016cf0b691\",\"http://www.theworldavatar.com/kb/sgp/singapore/buildings/GEOJSON_MARINA_BAY.owl#BuildingB6f2c6795-e81b-4eaf-ae6c-ce2600f18d03\",\"http://www.theworldavatar.com/kb/sgp/singapore/buildings/GEOJSON_MARINA_BAY.owl#BuildingB03a83459-5a60-403e-832d-9ad7bff65050\",\"http://www.theworldavatar.com/kb/sgp/singapore/buildings/GEOJSON_MARINA_BAY.owl#BuildingB1531935d-4d8d-443c-b181-45e0aea55ce3\",\"http://www.theworldavatar.com/kb/sgp/singapore/buildings/GEOJSON_MARINA_BAY.owl#BuildingB0ff4a61c-7900-4c7e-b9b8-594b1effb241\",\"http://www.theworldavatar.com/kb/sgp/singapore/buildings/GEOJSON_MARINA_BAY.owl#BuildingBa235bf41-dd66-4cca-a2e4-c78bad29294d\",\"http://www.theworldavatar.com/kb/sgp/singapore/buildings/GEOJSON_MARINA_BAY.owl#BuildingB6d0ba1aa-5646-4f9c-93d0-e94c4d82e8fc\",\"http://www.theworldavatar.com/kb/sgp/singapore/buildings/GEOJSON_MARINA_BAY.owl#BuildingBff21937e-42ea-47ba-bd22-ab227e114312\",\"http://www.theworldavatar.com/kb/sgp/singapore/buildings/GEOJSON_MARINA_BAY.owl#BuildingB9b6772d5-46da-49f1-8840-9b650a08148b\",\"http://www.theworldavatar.com/kb/sgp/singapore/buildings/GEOJSON_MARINA_BAY.owl#BuildingB625a5a7c-572c-4965-95db-88b3ae909dd5\",\"http://www.theworldavatar.com/kb/sgp/singapore/buildings/GEOJSON_MARINA_BAY.owl#BuildingB62bcca89-5010-4fc1-a96e-cf7613d4f5a0\",\"http://www.theworldavatar.com/kb/sgp/singapore/buildings/GEOJSON_MARINA_BAY.owl#BuildingB9f041c09-756c-4651-95f0-0ac4fd8a0bce\",\"http://www.theworldavatar.com/kb/sgp/singapore/buildings/GEOJSON_MARINA_BAY.owl#BuildingB1d096f5f-1b58-4042-886f-f95ccab7c914\",\"http://www.theworldavatar.com/kb/sgp/singapore/buildings/GEOJSON_MARINA_BAY.owl#BuildingB429d0da7-ddb0-4ff0-a386-a322dc24311c\",\"http://www.theworldavatar.com/kb/sgp/singapore/buildings/GEOJSON_MARINA_BAY.owl#BuildingB163ac8e9-1bc8-454d-99ea-354f9b399183\",\"http://www.theworldavatar.com/kb/sgp/singapore/buildings/GEOJSON_MARINA_BAY.owl#BuildingB85aa0918-fad0-4967-8f6e-adf26b7be123\",\"http://www.theworldavatar.com/kb/sgp/singapore/buildings/GEOJSON_MARINA_BAY.owl#BuildingB5410b5e4-ed2f-4a54-83c0-34e4e8526d07\",\"http://www.theworldavatar.com/kb/sgp/singapore/buildings/GEOJSON_MARINA_BAY.owl#BuildingBd9921568-81d1-44f1-b652-ed08f8a67a1c\",\"http://www.theworldavatar.com/kb/sgp/singapore/buildings/GEOJSON_MARINA_BAY.owl#BuildingBe1edcd60-fd71-4972-a030-fa4774bd128d\",\"http://www.theworldavatar.com/kb/sgp/singapore/buildings/GEOJSON_MARINA_BAY.owl#BuildingB7e19b6b5-3601-432a-9e39-42b2278dce99\",\"http://www.theworldavatar.com/kb/sgp/singapore/buildings/GEOJSON_MARINA_BAY.owl#BuildingB3801fb15-da0b-41da-a6f5-3cab0d4907a3\",\"http://www.theworldavatar.com/kb/sgp/singapore/buildings/GEOJSON_MARINA_BAY.owl#BuildingB759df653-c25b-4446-a51a-3051f2276b25\",\"http://www.theworldavatar.com/kb/sgp/singapore/buildings/GEOJSON_MARINA_BAY.owl#BuildingBea6f0e08-c8ca-4b88-879f-a8870387546f\"]}";
	String episodeIRI = "http://www.theworldavatar.com/kb/agents/Service__Episode.owl#Service";

	public void testAgentCall() {
		JSONObject jo = new JSONObject();
		String sim_iri = "http://www.theworldavatar.com/kb/ontodispersionsim/OntoDispersionSim.owl#sim5";
		jo.put(DispSimSparql.SimKey, sim_iri);
		AgentCaller.executeGetWithURLAndJSON("http://localhost:8080/JPS_VIRTUALSENSOR/EpisodeAgent", jo.toString());
	}
	
	public void testepisodeweatherinput() {
		String dataPath = QueryBroker.getLocalDataPath();
		String filename="mcwind_input_singapore_20191118.txt";
		List<String>stniri= new ArrayList<String>();
		stniri.add(stationiri1);//sentosa
		stniri.add(stationiri2);//ubin
		
//		new EpisodeAgent().createWeatherInput(dataPath,filename,stniri);
	}
	
	public void testEpisodeEmissionInput() {
		JSONObject jo = new JSONObject();
		Region.putRegion(jo, 2);
		System.out.println("json="+jo.toString());
		String dataPath = QueryBroker.getLocalDataPath();
		String result = AgentCaller.executeGetWithURLAndJSON("http://www.theworldavatar.com:80/JPS_POSTGRESQL/getEntitiesWithinRegion", jo.toString());
		System.out.println("result of the ship= "+result);
	    JSONObject shipdata = new JSONObject(result);
	    new OldEpisodeAgent().createEmissionInput(dataPath,"points_singapore_2019.csv",shipdata);
	}
	
	/**
	 * testdirectcallemission requires the emissions to be calculated first and present in your 
	 * local owl files, so it'll fail if you don't have the emissions calculated for those ships
	 */
	public void testdirectcallemission() {
		String dataPath = QueryBroker.getLocalDataPath();
		JSONObject input= new JSONObject (jsonparamsample2);
		JSONObject shipdata=input.getJSONObject("ship");
		new OldEpisodeAgent().createEmissionInput(dataPath,"points_singapore_2019.csv",shipdata);
	}
	
	public void testEpisodeControlEmissionInput() throws IOException {
		JSONObject jo = new JSONObject();
		Region.putRegion(jo, 2);
		Scope sc = new Scope(jo.getJSONObject(Region.keyRegion));
		sc.transform(Region.getTargetCRSName(episodeIRI, Region.SINGAPORE_IRI));
		String dataPath = QueryBroker.getLocalDataPath();
		String result = AgentCaller.executeGetWithURLAndJSON("http://www.theworldavatar.com:80/JPS_POSTGRESQL/getEntitiesWithinRegion", jo.toString());
		System.out.println("result of the ship= "+result);
		JSONObject shipdata = new JSONObject(result);
//		new EpisodeAgent().createControlEmissionFile(shipdata,dataPath,"cctapm_meta_PSE.inp",sc);
	}
	
	public void testEpisodeReceptorInput() {
		String dataPath = QueryBroker.getLocalDataPath();
		String filename="receptor_input.txt";
		JSONObject jo = new JSONObject();
		Region.putRegion(jo, 2);
		Scope sc = new Scope(jo.getJSONObject(Region.keyRegion));
		sc.transform(Region.getTargetCRSName("episode", Region.SINGAPORE_IRI));
		new OldEpisodeAgent().createReceptorFile(dataPath,filename,sc);
	}
	
	// need to modify these tests to use the new weather stations
	public void testControlWeather() throws IOException {
		String dataPath = QueryBroker.getLocalDataPath();
		List<String>stniri=new ArrayList<String>();
		stniri.add("http://www.theworldavatar.com/kb/sgp/singapore/WeatherStation-008.owl#WeatherStation-008");
		stniri.add("http://www.theworldavatar.com/kb/sgp/singapore/WeatherStation-006.owl#WeatherStation-006");
		JSONObject jo = new JSONObject();
		Region.putRegion(jo, 2);
		Scope sc = new Scope(jo.getJSONObject(Region.keyRegion));
		sc.transform(Region.getTargetCRSName("episode", Region.SINGAPORE_IRI));
//		new EpisodeAgent().createControlWeatherORCityChemFile(dataPath, "run_file.asc",stniri,sc) ;
	}

	public void testControlCityChem() throws IOException {
		String dataPath = QueryBroker.getLocalDataPath();
		List<String>stniri=new ArrayList<String>();
		stniri.add("http://www.theworldavatar.com/kb/sgp/singapore/WeatherStation-008.owl#WeatherStation-008");
		stniri.add("http://www.theworldavatar.com/kb/sgp/singapore/WeatherStation-006.owl#WeatherStation-006");
		JSONObject jo = new JSONObject();
		Region.putRegion(jo, 2);
		Scope sc = new Scope(jo.getJSONObject(Region.keyRegion));
		sc.transform(Region.getTargetCRSName("episode", "Singapore"));
//		new EpisodeAgent().createControlWeatherORCityChemFile(dataPath, "citychem_restart.txt", stniri, sc) ;
	}
	
	public void testControlTopology() throws IOException {
		String dataPath = QueryBroker.getLocalDataPath();
		List<String>srtm=new ArrayList<String>();
        srtm = Region.getSRTM(Region.SINGAPORE_IRI);
		JSONObject jo = new JSONObject();
		Region.putRegion(jo, 2);
		Scope sc = new Scope(jo.getJSONObject(Region.keyRegion));
		sc.transform(Region.getTargetCRSName("episode", "Singapore"));
		new OldEpisodeAgent().createControlTopologyFile(srtm, dataPath, "aermap.inp", sc) ;
	}
	
	public void testlastdirectory() {
		OldEpisodeAgent a= new OldEpisodeAgent();
		String agent="http://www.theworldavatar.com/kb/agents/Service__Episode.owl#Service";
		
		String res=a.getPreviousHourDatapath(agent, Region.SINGAPORE_IRI);
		System.out.println("result= "+res);
	}

    public void testvalidateInput() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException {
        OldEpisodeAgent ea = new OldEpisodeAgent();
        Method validateInput = ea.getClass().getDeclaredMethod("validateInput",JSONObject.class);
        validateInput.setAccessible(true);

        // create a complete set of inputs that should pass the test
        String keyUppercorner = "uppercorner";
        String keyLowercorner = "lowercorner";
        String keyUpperx = "upperx";
        String keyUppery = "uppery";
        String keyLowerx = "lowerx";
        String keyLowery = "lowery";
        String keyRegion = "region";
        String keyCity = "city";
        String keyAgent = "agent";
        String keyStationIRI = "stationiri";
        String keyAirStationIRI = "airStationIRI";
        String keySrsname = "srsname";
        String agentiri = "http://www.theworldavatar.com/kb/agents/Service__Episode.owl#Service";
        String airstationiri = "http://www.theworldavatar.com/kb/sgp/singapore/AirQualityStation-002.owl"
                + "#AirQualityStation-002";
        double xmin=11560879/*.832*/;
        double ymin=140107/*.739*/;
        double xmax=11563323/*.926*/;
        double ymax=143305/*.896*/;

        JSONObject jo= new JSONObject();
        JSONArray station= new JSONArray();
        station.put(stationiri1);
        station.put(stationiri2);
        jo.put(keyStationIRI, station);
        jo.put(keyAirStationIRI, airstationiri);

        JSONObject scope = new JSONObject();
        JSONObject low = new JSONObject();
        JSONObject up = new JSONObject();
        up.put(keyUpperx, xmax);
        up.put(keyUppery, ymax);
        low.put(keyLowerx, xmin);
        low.put(keyLowery, ymin);
        scope.put(keyLowercorner, low);
        scope.put(keyUppercorner, up);
        scope.put(keySrsname,"EPSG:3857");
        jo.put(keyRegion,scope);
        jo.put(keyAgent, agentiri);
        jo.put(keyCity,Region.SINGAPORE_IRI);

        assertTrue(checkInput(ea,validateInput,jo));

//      Now each key in the input is removed, toggled to empty and to the wrong type to trigger an exception
//      Begin with the region key
        jo.remove(keyRegion);
        assertFalse(checkInput(ea,validateInput,jo));
        jo.put(keyRegion,"");
        assertFalse(checkInput(ea,validateInput,jo));
        jo.put(keyRegion,"abc");
        assertFalse(checkInput(ea,validateInput,jo));
        jo.put(keyRegion,scope);
        assertTrue(checkInput(ea,validateInput,jo));

//      air station key
        jo.remove(keyAirStationIRI);
        assertFalse(checkInput(ea,validateInput,jo));
        jo.put(keyAirStationIRI,"");
        assertFalse(checkInput(ea,validateInput,jo));
        jo.put(keyAirStationIRI, "abc");
        assertFalse(checkInput(ea,validateInput,jo));
        jo.put(keyAirStationIRI,airstationiri);
        assertTrue(checkInput(ea,validateInput,jo));

//      Agent key
        jo.remove(keyAgent);
        assertFalse(checkInput(ea,validateInput,jo));
        jo.put(keyAgent,"");
        assertFalse(checkInput(ea,validateInput,jo));
        jo.put(keyAgent,"abc");
        assertFalse(checkInput(ea,validateInput,jo));
        jo.put(keyAgent,agentiri);
        assertTrue(checkInput(ea,validateInput,jo));

//      City IRI. 
        jo.remove(keyCity);
        assertFalse(checkInput(ea,validateInput,jo));
        jo.put(keyCity,"");
        assertFalse(checkInput(ea,validateInput,jo));
        jo.put(keyCity,"abc");
        assertFalse(checkInput(ea,validateInput,jo));
        jo.put(keyCity,Region.SINGAPORE_IRI);
        assertTrue(checkInput(ea,validateInput,jo));

//      Coordinates
        up.remove(keyUpperx);
        assertFalse(checkInput(ea,validateInput,jo));
        up.put(keyUpperx,"");
        assertFalse(checkInput(ea,validateInput,jo));
        up.put(keyUpperx,"abc");
        assertFalse(checkInput(ea,validateInput,jo));
        up.put(keyUpperx,xmax);
        assertTrue(checkInput(ea,validateInput,jo));

        up.remove(keyUppery);
        assertFalse(checkInput(ea,validateInput,jo));
        up.put(keyUppery, "");
        assertFalse(checkInput(ea,validateInput,jo));
        up.put(keyUppery, "abc");
        assertFalse(checkInput(ea,validateInput,jo));
        up.put(keyUppery, ymax);
        assertTrue(checkInput(ea,validateInput,jo));

        low.remove(keyLowerx);
        assertFalse(checkInput(ea,validateInput,jo));
        low.put(keyLowerx, "");
        assertFalse(checkInput(ea,validateInput,jo));
        low.put(keyLowerx, "abc");
        assertFalse(checkInput(ea,validateInput,jo));
        low.put(keyLowerx, xmin);
        assertTrue(checkInput(ea,validateInput,jo));

        low.remove(keyLowery);
        assertFalse(checkInput(ea,validateInput,jo));
        low.put(keyLowery, "");
        assertFalse(checkInput(ea,validateInput,jo));
        low.put(keyLowery, "abc");
        assertFalse(checkInput(ea,validateInput,jo));
        low.put(keyLowery, ymin);
        assertTrue(checkInput(ea,validateInput,jo));

        scope.remove(keySrsname);
        assertFalse(checkInput(ea,validateInput,jo));
        scope.put(keySrsname,"");
        assertFalse(checkInput(ea,validateInput,jo));
        scope.put(keySrsname,"abc");
        assertFalse(checkInput(ea,validateInput,jo));
        scope.put(keySrsname,"EPSG:3857");
        assertTrue(checkInput(ea,validateInput,jo));
    }

    private boolean checkInput(OldEpisodeAgent ea,Method validateInput,JSONObject jo) throws IllegalAccessException, IllegalArgumentException {
//      This method is used in the testvalidateInput method, returns true if all inputs are present
        boolean valid=false;
        try {
            validateInput.invoke(ea, jo);
            valid=true;//this is not the best way to do this because this is not the output of the method
        } catch (InvocationTargetException e) {
            // this is a workaround as the JPS servlet will give a runtime exception
            assertEquals("javax.ws.rs.BadRequestException", e.getCause().getCause().getStackTrace()[14].getClassName());
            valid=false;
        }
        return valid;
    }
}
