using UnityEngine;
using System.Collections;
using System.Net.Sockets;
using System.Net;
using System.Text;











public class LoadBundle : MonoBehaviour {

 
    private static byte[] result = new byte[1024];
    static Socket clientSocket;
    static IPAddress ip;

    
    IEnumerator Start()
    {
        ip = IPAddress.Parse("10.25.188.58");
        clientSocket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        try
        {
            clientSocket.Connect(new IPEndPoint(ip, 8885)); //configure the ip address and the port number
            Debug.Log("Connected");
        }
        catch
        {
        }
        try
        {
            string sendMessage = "geometry";
            clientSocket.Send(Encoding.ASCII.GetBytes(sendMessage));
        }
        catch
        {
            clientSocket.Shutdown(SocketShutdown.Both);
            clientSocket.Close();
        }
        int receiveLength = clientSocket.Receive(result);
        string url = Encoding.ASCII.GetString(result, 0, receiveLength);
        Debug.Log("received" + url);
        WWW www = WWW.LoadFromCacheOrDownload(url, 0);

        // Wait for download to complete
        yield return www;
        // Load and retrieve the AssetBundle
        AssetBundle bundle = www.assetBundle;

        AssetBundleRequest request = bundle.LoadAssetAsync("Structure.fbx", typeof(GameObject));

        // Wait for completion
        yield return request;
        
        GameObject obj = Instantiate(request.asset) as GameObject;
        // Unload the AssetBundles compressed contents to conserve memory
        bundle.Unload(false);

        // Frees the memory from the web stream
        www.Dispose();
    }

    // Update is called once per frame
    void Update () {
	
	}
}
