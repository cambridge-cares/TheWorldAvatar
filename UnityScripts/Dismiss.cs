using UnityEngine;
using System.Collections;
using UnityEngine.UI;
public class Dismiss : MonoBehaviour {
    public GameObject panel2;
	// Use this for initialization
	void Start () {
        Debug.Log("Mouse Down");
    }
	
	// Update is called once per frame
	void Update () {
	
	}

    void OnMouseDown()
    {
       panel2.SetActive(false);
        Debug.Log("Mouse Down");

    }

}
