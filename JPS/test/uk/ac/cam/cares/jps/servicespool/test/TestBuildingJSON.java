package uk.ac.cam.cares.jps.servicespool.test;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.Test;

public class TestBuildingJSON {
	public String IRIs  = "[\r\n" + 
			"    \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings1.owl#BuildingGUID_35ED6FD1-5876-4F03-996D-8C885FD11057\",\r\n" + 
			"    \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings1.owl#BuildingGUID_ABE4C2D7-C6E0-4B95-9A3C-E4457B796F3C\",\r\n" + 
			"    \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings1.owl#BuildingGUID_C1C1997A-43EC-4F7C-B62B-5F115CAD5117\",\r\n" + 
			"    \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings1.owl#BuildingGUID_240BBC7B-CE2C-420D-89E5-CFD6A2C0C044\",\r\n" + 
			"    \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings1.owl#BuildingGUID_4611BF50-156F-458A-92EC-A11EEF98471F\",\r\n" + 
			"    \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings0.owl#BuildingGUID_94405F3C-FB53-4EC8-93A1-5F95FEC74CBD\",\r\n" + 
			"    \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings3.owl#BuildingGUID_E09C7CD0-2088-4F9F-AA4E-C0ACB582D77F\",\r\n" + 
			"    \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings2.owl#BuildingGUID_81AB074F-EEB6-4006-87B6-6BB5E80E692B\",\r\n" + 
			"    \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings1.owl#BuildingGUID_7CA23259-87DB-4FF2-913B-DA4D82A5D3AE\",\r\n" + 
			"    \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings2.owl#BuildingGUID_9D576B3B-9D3C-4E50-812D-FDCA5F55198A\",\r\n" + 
			"    \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings1.owl#BuildingGUID_73624930-7901-4F47-84FF-C1D0FC2E8991\",\r\n" + 
			"    \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings4.owl#BuildingGUID_B42C1246-F744-4AC3-80EE-1768B1729924\",\r\n" + 
			"    \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings4.owl#BuildingGUID_BD5DD2CF-FFE1-4F39-9704-70D673C4744B\",\r\n" + 
			"    \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings1.owl#BuildingGUID_0FC3F2FF-A572-4AAE-B7C2-EA3BE1FA1C12\",\r\n" + 
			"    \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings1.owl#BuildingGUID_E4705286-9AFA-433E-9105-4F953FA50FE1\",\r\n" + 
			"    \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings1.owl#BuildingGUID_952A0A82-9CEB-4F21-836D-3845D068012A\",\r\n" + 
			"    \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings1.owl#BuildingGUID_6FA9E00C-A79E-408C-9AD4-2880E3A60972\",\r\n" + 
			"    \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings2.owl#BuildingGUID_43E9C3A8-46CF-46A1-9CF1-5DC457A9EF50\",\r\n" + 
			"    \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings1.owl#BuildingGUID_DDA5FD88-34C5-4C44-A485-CA001E91C7FD\",\r\n" + 
			"    \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings2.owl#BuildingGUID_2DDCBBF1-041C-40EE-AE1C-96C90F63DAF2\",\r\n" + 
			"    \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings1.owl#BuildingGUID_00564872-9958-4788-997C-1E86F79AF802\",\r\n" + 
			"    \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings3.owl#BuildingGUID_3C20D25F-1A9B-4C9B-B55F-7097636D6CAF\",\r\n" + 
			"    \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings2.owl#BuildingGUID_5638ACFF-363C-409E-996B-2F423D46A9E7\",\r\n" + 
			"    \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings3.owl#BuildingGUID_97612518-F080-48FE-B48C-D3600B96BB1E\",\r\n" + 
			"    \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings4.owl#BuildingGUID_3FA056EB-3F9A-48B0-95A2-7C8CC2319B7E\"\r\n" + 
			"]";
	
	@Test
	public void test() {

		
		try {
			JSONObject result = new JSONObject();
			JSONArray irisInJSON = new JSONArray(IRIs);
			result.put("BldIRI", irisInJSON);
			System.out.println(result.toString());
		} catch (JSONException e) {
 			e.printStackTrace();
		}
	}

}
