import java.net.Socket;
import java.net.URL;
import java.net.MalformedURLException;
import java.net.UnknownHostException;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.BufferedReader;
import java.io.InputStreamReader;

public class SimpleHttpClient {
    public static void main(String[ ] args) {
    // usage
	if (args.length < 1) {
	    System.err.println("Usage: SimpleHttpClient <url>");
	    return;
	}

	try {
	    // Parse the URL.
	    URL url = new URL(args[0]);
	    String host = url.getHost();
	    String path = url.getPath();
	    int port = url.getPort();
	    if (port < 0) port = 80;

	    // Send the request.
	    String request = "GET " + path + " HTTP/1.1\n";
	    request += "host: " + host;
	    request += "\n\n";

	    Socket sock = new Socket(host, port);
	    PrintWriter writer = new PrintWriter(sock.getOutputStream());
	    writer.print(request);
	    writer.flush();

	    // Read and print the response.
	    BufferedReader reader = 
		new BufferedReader(new InputStreamReader(sock.getInputStream()));
	    String next_record = null;
	    while ((next_record = reader.readLine()) != null) 
		System.out.println(next_record);
	    sock.close();
	}
	catch(MalformedURLException e) {
	    throw new RuntimeException("Please try again. Bad URL.\n" + e);
	}
	catch(UnknownHostException e) {
	    throw new RuntimeException("Please try again. Unknown host.\n" + e);
	}
	catch(IOException e) {
	    throw new RuntimeException("Please try again. Something's wrong.\n" + e);
	}
    }
}