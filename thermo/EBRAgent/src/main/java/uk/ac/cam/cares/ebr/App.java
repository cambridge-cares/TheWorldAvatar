package uk.ac.cam.cares.ebr;

/**
 * Hello world!
 *
 */
public class App 
{	
    public static void main( String[] args ) throws Exception
    {
        System.out.println( "Hello World!" );
        
        FederatedQuery.runFederatedSPARQLOnDbpediaWikipedia();
      
    }
}