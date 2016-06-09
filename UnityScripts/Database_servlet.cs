using System;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Xml;
// ip : 169.254.208.213


/* ======================================================================================================================
 * Notes：
 * 1. Please be reminded that the xml file attached to this servlet must be put into the same folder as this servlet
 * 2. The IP address set here is 10.25.188.56, such IP address is for connection within the company only
 * 
 * ZHOU XIAOCHI 2016-06-09 
 * ======================================================================================================================
 * */



namespace Database_servlet 
{
    class Program
    {
        private static byte[] result = new byte[1024];
        private static int myProt = 8885;   //port number 
        static Socket serverSocket; 

        public static XmlNodeList levelsList;


        static void Main(string[] args)
        {
            // IP address of the server   
            IPAddress ip = IPAddress.Parse("10.25.188.58");
            serverSocket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
            serverSocket.Bind(new IPEndPoint(ip, myProt));  //Bind the IP address and port
            serverSocket.Listen(10);    //Set the maximum number of clients
            Console.WriteLine("Listener Started", serverSocket.LocalEndPoint.ToString());
     
            Thread myThread = new Thread(ListenClientConnect); // Start a thread to receive data from clients
            myThread.Start();
            Console.ReadLine();
        }

        
        private static void ListenClientConnect()
        {
            while (true)
            {
                Socket clientSocket = serverSocket.Accept();
            //  clientSocket.Send(Encoding.ASCII.GetBytes("Connected"));
                Thread receiveThread = new Thread(ReceiveMessage);
                receiveThread.Start(clientSocket);
            }
        }

        private static void ReceiveMessage(object clientSocket)
        {
            Socket myClientSocket = (Socket)clientSocket;
            while (true)
            {
                try
                {
                   
                    String request;
                   
                    int receiveNumber = myClientSocket.Receive(result);
                    Console.WriteLine(Encoding.ASCII.GetString(result, 0, receiveNumber));
                    request = Encoding.ASCII.GetString(result, 0, receiveNumber);


                    if (request != null)
                    {
                        XmlDocument xmlDoc = new XmlDocument(); // xmlDoc is the new xml document.
                        xmlDoc.Load("BiodieselPlant.xml"); // load the file.
                        XmlElement root = xmlDoc.DocumentElement;
                        levelsList = root.GetElementsByTagName("substance:molecularFormula"); // array of the level nodes.
                        
                        Console.WriteLine(levelsList.Item(0).InnerXml);
                        myClientSocket.Send(Encoding.ASCII.GetBytes(levelsList.Item(0).InnerXml)); // send back the query result
                    }
                   

                }
                catch (Exception ex)
                {
                    Console.WriteLine(ex.Message);
                    myClientSocket.Shutdown(SocketShutdown.Both);
                    myClientSocket.Close();
                    break;
                }
            }
        }
    }
}
