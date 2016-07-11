using UnityEngine;
using System.Collections;
using UnityEngine.UI;
using System.Net.Sockets;
using System.Net;

public class GoogleMap : MonoBehaviour
{
    public enum MapType
    {
        RoadMap,
        Satellite,
        Terrain,
        Hybrid
    }
     
    public GoogleMapLocation centerLocation;
    public GoogleMapLocation markerLocation1;
    public GoogleMapLocation markerLocation2;
    public GoogleMapLocation markerLocation3;
    public int zoom = 16;
    public MapType mapType;
    public int size = 4096;
    public bool doubleResolution = true;
    public int multipler = 1000000;
    public GameObject flag;
    public GameObject marker1;
    public GameObject marker2;
    public GameObject marker3;
    public GameObject structure;
    public GameObject plant;
    // Use this for initialization


         private static byte[] result = new byte[1024];
    static Socket clientSocket;
    static IPAddress ip;

    
    IEnumerator Start()
    {
        GameObject flag = GameObject.Find("Flag");
      

        Debug.Log(flag.transform.position.y);
        /*

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
        */

        WWW www = WWW.LoadFromCacheOrDownload("http://jparksimulator.com/biodieselplant", 1);

        yield return www;
        AssetBundle bundle = www.assetBundle;
        AssetBundleRequest request = bundle.LoadAssetAsync("Structure.fbx", typeof(GameObject));
        yield return request;
        GameObject obj_1 = Instantiate(request.asset) as GameObject;
        AssetBundleRequest request_2 = bundle.LoadAssetAsync("Plant.FBX", typeof(GameObject));
        yield return request_2;
        GameObject obj_2 = Instantiate(request_2.asset) as GameObject;

        bundle.Unload(false);
        www.Dispose();

        GameObject[] obj = GameObject.FindObjectsOfType(typeof(GameObject)) as GameObject[];

        foreach (GameObject gameobject in obj)
        {
            if (gameobject.name.Contains("E-")|| gameobject.name.Contains("P-")||gameobject.name.Contains("R-")||gameobject.name.Contains("T-")|| gameobject.name.Contains("V-"))
            {
                gameobject.AddComponent<BoxCollider>();
                BoxCollider boxcollider = gameobject.GetComponent<BoxCollider>();
                boxcollider.isTrigger = true;
                gameobject.AddComponent<onClick>();
            }



        }

        

        centerLocation.latitude = 1.2581851f;
        centerLocation.longitude = 103.677982f;
        markerLocation1.latitude = 1.2392201f;
        markerLocation1.longitude = 103.6874937f;
        markerLocation2.latitude = 1.2383376f;
        markerLocation2.longitude = 103.6885246f;
        markerLocation3.latitude = 1.3040654f;
        markerLocation3.longitude = 103.773342f;
        StartCoroutine(LoadMap());
        Text text = GameObject.Find("Text").GetComponent<Text>();
        marker1 = GameObject.Find("Marker1");
        marker2 = GameObject.Find("Marker2");
        marker3 = GameObject.Find("Marker3");


        float x1, y1;
        float x2, y2;
        float x3, y3;

        x1 = -(markerLocation1.longitude - centerLocation.longitude) * 725 * (Mathf.Pow(2, (zoom - 16))) * 10000;
        y1 = -(markerLocation1.latitude - centerLocation.latitude) * 725 * (Mathf.Pow(2, (zoom - 16))) * 10000;

        x2 = -(markerLocation2.longitude - centerLocation.longitude) * 725 * (Mathf.Pow(2, (zoom - 16))) * 10000;
        y2 = -(markerLocation2.latitude - centerLocation.latitude) * 725 * (Mathf.Pow(2, (zoom - 16))) * 10000;

        x3 = -(markerLocation3.longitude - centerLocation.longitude) * 725 * (Mathf.Pow(2, (zoom - 16))) * 10000;
        y3 = -(markerLocation3.latitude - centerLocation.latitude) * 725 * (Mathf.Pow(2, (zoom - 16))) * 10000;

     //   marker1.transform.position = new Vector3(x1, 0, y1);
      //  marker2.transform.position = new Vector3(x2, 0, y2);
      //  marker3.transform.position = new Vector3(x3, 0, y3);




        plant = GameObject.Find("Plant(Clone)");
        structure = GameObject.Find("Structure(Clone)");
        plant.transform.localScale = new Vector3(0.25f, 0.25f, 0.25f);
        plant.transform.position = plant.transform.position * 0.25f;
        plant.transform.Rotate(new Vector3(0, 43.02f, 0));
        structure.transform.localScale = new Vector3(0.25f, 0.25f, 0.25f);
        structure.transform.position = structure.transform.position * 0.25f;
        structure.transform.rotation = new Quaternion(0, 0, 0,0);
        structure.transform.Rotate(new Vector3(270.019f, 43.02f, 0));
        structure.transform.localPosition = structure.transform.localPosition + new Vector3(-46.173f, 0, 43.104f);
        structure.transform.localPosition = structure.transform.localPosition + new Vector3(-376f, 0, -523f);
        plant.transform.localPosition = plant.transform.localPosition + new Vector3(-376f, 0, -523f);



         GameObject plant2 =     (GameObject)Instantiate(plant,      new Vector3(-571, 0, -364),plant.transform.rotation);
         GameObject structure2 = (GameObject)Instantiate(structure,  new Vector3(-645.7f, 67.6f, -385.7f), structure.transform.rotation);
       
         GameObject plant3 = (GameObject)Instantiate(plant2,         new Vector3(-731.2f, 0, -197.9f), plant2.transform.rotation);
         GameObject structure3= (GameObject)Instantiate(structure2,  new Vector3(-811, 66.3f, -224.2f), structure2.transform.rotation);


    }


    //  void Start()
    //   {


    //  }
     // Update is called once per frame
    void Update()
    {


          



        if (Input.GetMouseButtonDown(2))
        {
            RaycastHit hit;
            Ray ray = Camera.main.ScreenPointToRay(Input.mousePosition);
            if (Physics.Raycast(ray, out hit))
            {
                float x, y; float abs_x, abs_y;
                x = hit.point.x; abs_x = Mathf.Abs(x);
                y = hit.point.z; abs_y = Mathf.Abs(y);
                float direction_x = x / abs_x;
                float direction_y = y / abs_y;


                GameObject[] obj_2 = UnityEngine.SceneManagement.SceneManager.GetActiveScene().GetRootGameObjects();
                ArrayList models_2 = new ArrayList();

                foreach (GameObject gameobject in obj_2)
                {
                    if (gameobject.name.Contains("Clone")|| gameobject.name.Contains("Mark"))
                    {
                        models_2.Add(gameobject);
                    }
                }

                if (abs_x >= 40000 || abs_y >= 40000)
                {

                    if (abs_x >= 40000)
                    {
                    centerLocation.longitude += (direction_x * - 0.0001f * multipler);
                        foreach (GameObject model in models_2)
                        {
                            model.transform.position = model.transform.position + new Vector3(-direction_x * multipler * 725 * (Mathf.Pow(2,zoom - 16)) ,0, 0);
                        }
                    }
                     
                    if (abs_y >= 40000)
                    { 
                    centerLocation.latitude += (direction_y *  - 0.0001f * multipler);
                        foreach (GameObject model in models_2)
                        {
                            model.transform.position = model.transform.position + new Vector3(0, 0, - direction_y * multipler * 725 * (Mathf.Pow(2, (zoom - 16))));
                        }
                    }


                    StartCoroutine(LoadMap());
                }




            }
        }


        if (Input.GetKey(KeyCode.LeftAlt))
        {

         GameObject[] obj = UnityEngine.SceneManagement.SceneManager.GetActiveScene().GetRootGameObjects();
         ArrayList models = new ArrayList();
         ArrayList scales = new ArrayList();

            foreach (GameObject gameobject in obj)
        {
            if (gameobject.name.Contains("Clone")|| gameobject.name.Contains("Mark"))
            {
                models.Add(gameobject);
                scales.Add(gameobject.transform.localScale);
            }
        }

            GameObject reference_obj  ;
            reference_obj = models[1] as GameObject;
            if (Input.GetAxis("Mouse ScrollWheel") < 0)
            {
                zoom = zoom - 1;
                StartCoroutine(LoadMap());
                foreach (GameObject model in models)
                {
                    model.transform.localScale = model.transform.localScale * 0.5f ;
                    model.transform.position = model.transform.position * 0.5f;
                }
             
                Debug.Log("Scrolled backward and zoom is " + zoom);
            }
            else if (Input.GetAxis("Mouse ScrollWheel") > 0 )
            {
                zoom = zoom + 1;
                StartCoroutine(LoadMap());
                foreach (GameObject model in models)
                {
                    model.transform.localScale = model.transform.localScale * 2;
                    model.transform.position = model.transform.position * 2;
                }
                Debug.Log("Scrolled forward and zoom is " + zoom);
            }

            float x1, y1;
            float x2, y2;
            float x3, y3;

            x1 = -(markerLocation1.longitude - centerLocation.longitude) * 725 * (Mathf.Pow(2, (zoom - 16))) * 10000;
            y1 = -(markerLocation1.latitude - centerLocation.latitude) * 725 * (Mathf.Pow(2, (zoom - 16))) * 10000;

            x2 = -(markerLocation2.longitude - centerLocation.longitude) * 725 * (Mathf.Pow(2, (zoom - 16))) * 10000;
            y2 = -(markerLocation2.latitude - centerLocation.latitude) * 725 * (Mathf.Pow(2, (zoom - 16))) * 10000;

            x3 = -(markerLocation3.longitude - centerLocation.longitude) * 725 * (Mathf.Pow(2, (zoom - 16))) * 10000;
            y3 = -(markerLocation3.latitude - centerLocation.latitude) * 725 * (Mathf.Pow(2, (zoom - 16))) * 10000;

        //    marker1.transform.position = new Vector3(x1, 0, y1);
         //   marker2.transform.position = new Vector3(x2, 0, y2);
          //  marker3.transform.position = new Vector3(x3, 0, y3);


        }
        // duplicate the biodisel plants and move them to the corresponding location


    }

    IEnumerator LoadMap()
    {
        var url = "http://maps.googleapis.com/maps/api/staticmap";
        var qs = "";

        if (centerLocation.address != "")
            qs += "center=" + WWW.UnEscapeURL(centerLocation.address);
        else
        {
            qs += "center=" + WWW.UnEscapeURL(string.Format("{0},{1}", centerLocation.latitude, centerLocation.longitude));
        }

        qs += "&zoom=" + zoom.ToString();

        qs += "&size=" + WWW.UnEscapeURL(string.Format("{0}x{0}", size));
        qs += "&scale=" + (doubleResolution ? "2" : "1");
        qs += "&maptype=" + mapType.ToString().ToLower();
        var usingSensor = false;
#if UNITY_IPHONE
		usingSensor = Input.location.isEnabledByUser && Input.location.status == LocationServiceStatus.Running;
#endif
        qs += "&sensor=" + (usingSensor ? "true" : "false");


        var req = new WWW(url + "?" + qs + "&key=AIzaSyCUhcopsyWu4QpNhrS85ETo0XtLaSLUrbk");
        yield return req;
        GetComponent<Renderer>().material.mainTexture = req.texture;
    }

 
}