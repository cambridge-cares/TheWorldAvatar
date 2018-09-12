package uk.ac.cam.cares.jps.servicespool;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.semantic.QueryWarehouse;
 
@WebServlet("/CoordianteRefConvertor")
public class CoordianteRefConvertor extends HttpServlet {
	private static final long serialVersionUID = 1L;
       
 
    public CoordianteRefConvertor() {
        super();
    }

	
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

		String value = request.getParameter("value");
		System.out.println("============= Convertor ==============");
		System.out.println(value);
		System.out.println("======================================");
		Model model = ModelFactory.createDefaultModel();
		RDFDataMgr.read(model, new ByteArrayInputStream(value.getBytes("UTF-8")), Lang.RDFJSON);
		JSONObject region = QueryWarehouse.getRegionCoordinates(model);
		
		System.out.println("================region==============");
		System.out.println(region.toString());
		System.out.println("======================================");


		response.getWriter().write(region.toString());
	
	}

	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		doGet(request, response);
	}

}
