package uk.ac.cam.cares.jps.powerplant;

import java.io.IOException;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
/*Author ZHOUXIAOCHI*/


@WebServlet("/PowerPlantWrapperAgent")
public class PowerPlantWrapper extends HttpServlet {
	private static final long serialVersionUID = 1L;
       

    public PowerPlantWrapper() {
        super();
    }


	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
 

		try {
			response.getWriter().append("http://www.theworldavatar.com/Plant-001.owl") ;
		} catch (IOException e) {
			e.printStackTrace();
		}
	}


	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		doGet(request, response);
	}

}
