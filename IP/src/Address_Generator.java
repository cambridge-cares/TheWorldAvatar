import java.net.InetAddress;
import java.net.UnknownHostException;

public class Address_Generator
{
	//Constructor
	public Address_Generator(){}
	public static void main(String args[])
	{
		try
		{
			InetAddress localaddr = InetAddress.getLocalHost();
			
			System.out.println ("Local IP Address : " + localaddr );
			System.out.println ("Local hostname   : " + localaddr.getHostName());
		}
		catch (UnknownHostException e)
		{
			System.err.println ("Can't detect localhost : " + e);
		}
		
	}

	/** Converts a byte_array of octets into a string */
	public static String byteToStr( byte[] byte_arr )
	{
		StringBuffer internal_buffer = new StringBuffer();

		// Keep looping, and adding octets to the IP Address
		for (int index = 0; index < byte_arr.length -1; index++)
		{
			internal_buffer.append ( String.valueOf(byte_arr[index]) + ".");
		}
		
		// Add the final octet, but no trailing '.'
		internal_buffer.append ( String.valueOf (byte_arr.length) );

		return internal_buffer.toString();
	}
}

