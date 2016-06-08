using UnityEngine;
using UnityEngine.UI;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using System.IO;
using System.Collections;

public class Onclick : MonoBehaviour
{
    public Text text;
    public TextAsset BiodieselPlant;
    public GameObject panel;
    List<Dictionary<string, string>> levels = new List<Dictionary<string, string>>();
    Dictionary<string, string> obj;


    // Use this for initialization
    void Start()
    {
      //  panel.SetActive(false);
    }

    // Update is called once per frame
    void Update()
    {

    }
    void OnMouseDown()
    {
        // this object was clicked - do something
        //  Destroy(this.gameObject);
        getXML();
      
    }

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


}  