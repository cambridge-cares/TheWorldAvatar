package uk.ac.cam.cares.jps.discovery.util;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletResponse;

import uk.ac.cam.cares.jps.base.discovery.IAgentCommunication;
import uk.ac.cam.cares.jps.base.discovery.TypeString;
import uk.ac.cam.cares.jps.discovery.client.DiscoveryProvider;

public class JPSBaseServlet extends HttpServlet {

	private static final long serialVersionUID = -1981289825404800946L;
	private IAgentCommunication discoveryProvider = null;
	
	protected IAgentCommunication getDiscoveryProvider() {
		if (discoveryProvider == null) {
			discoveryProvider = new DiscoveryProvider();
		}
		return discoveryProvider;
	}	
	
	protected void print(HttpServletResponse resp, List<TypeString> list) throws IOException {
		resp.setContentType("text/plain");
		resp.setCharacterEncoding("UTF-8");
		
		PrintWriter out = resp.getWriter();
		int size = list.size();
		for (int i=0; i<size; i++) {	
			String message = list.get(i).getValue();
			if (i < size-1) {
				message += " ";
			}
			out.print(message);
		}
	}
	
	protected void print(HttpServletResponse resp, String message) throws IOException {
		resp.setContentType("text/plain");
		resp.setCharacterEncoding("UTF-8");
		PrintWriter out = resp.getWriter();
		out.print(message);
	}
}
