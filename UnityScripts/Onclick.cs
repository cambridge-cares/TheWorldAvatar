using UnityEngine;
using UnityEngine.UI;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using System.IO;
using System.Collections;
using System.Net.Sockets;
using System.Net;

public class Onclick : MonoBehaviour
{
    public Text text;
    public TextAsset BiodieselPlant;
    public GameObject panel;
    private static byte[] result = new byte[1024];
    static Socket clientSocket;
    static IPAddress ip;
    //  List<Dictionary<string, string>> levels = new List<Dictionary<string, string>>();
    //  Dictionary<string, string> obj;


    // Use this for initialization
    void Start()
    {
        //  panel.SetActive(false);
        //
        ip = IPAddress.Parse("10.25.188.58");
        clientSocket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        try
        {
            clientSocket.Connect(new IPEndPoint(ip, 8885)); //configure the ip address and the port number
            Debug.Log("Connected");

        }
        catch
        {

            return;
        }

    }

    // Update is called once per frame
    void Update()
    {

    }




    void OnMouseDown()
    {
        // this object was clicked - do something
        //  Destroy(this.gameObject);
        // getXML();

       
   
        try
        {
            string sendMessage = "substance:molecularFormula";
            clientSocket.Send(Encoding.ASCII.GetBytes(sendMessage));
            Debug.Log("Message Sent");
           
        }
        catch
        {
            clientSocket.Shutdown(SocketShutdown.Both);
            clientSocket.Close();
        }

        int receiveLength = clientSocket.Receive(result);
        Debug.Log(Encoding.ASCII.GetString(result, 0, receiveLength));
        panel.SetActive(true);
        text.text = Encoding.ASCII.GetString(result, 0, receiveLength);
    }
    /*
    void getXML()
    {
       
        panel.SetActive(true);



        XmlDocument xmlDoc = new XmlDocument(); // xmlDoc is the new xml document.
        xmlDoc.LoadXml(BiodieselPlant.text); // load the file.
        XmlElement root = xmlDoc.DocumentElement;
        XmlNodeList levelsList = root.GetElementsByTagName("substance:molecularFormula"); // array of the level nodes.
        text.text = levelsList.Item(3).InnerXml;
        Debug.Log(levelsList.Item(3).InnerXml);

        
        }
        */

}  