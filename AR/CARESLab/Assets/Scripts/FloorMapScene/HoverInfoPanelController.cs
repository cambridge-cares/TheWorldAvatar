using UnityEngine;

/// <summary>
/// Show text on <see cref="InfoDisplayPanel"/> when user hover the pointer on top of the current gameobject. 
/// </summary>
public class HoverInfoPanelController : MonoBehaviour
{
    [SerializeField]
    private GameObject displayPanel;

    [SerializeField]
    private Vector3 panelOffset = new Vector3(-0.2f, 0f, -0.1f);

    [SerializeField]
    private Vector3 panelScale = new Vector3(0.003f, 0.003f, 0.003f);

    [SerializeField]
    private GameObject canvas;

    private GameObject panelInstance;

    /// <summary>
    /// Show the info display pannel. Use as callback for <see href="https://learn.microsoft.com/en-us/dotnet/api/microsoft.mixedreality.toolkit.ui.pressablebutton?view=mixed-reality-toolkit-unity-2020-dotnet-2.8.0" langword="Pressable Button"/> when hover.
    /// </summary>
    public void ShowDisplayPanel()
    {
        if (panelInstance == null)
        {
            panelInstance = Instantiate(displayPanel);
            Transform rootTransform = gameObject.transform;
            while (rootTransform.parent != null)
            {
                rootTransform = rootTransform.parent;
            }
            if (canvas != null)
            {
                panelInstance.transform.SetParent(canvas.transform, false);
            }

            panelInstance.transform.position = panelOffset + transform.position;
            Debug.Log("Panel Position: " + panelInstance.transform.position + "; obj position: " + transform.position);
            panelInstance.transform.localScale = panelScale;

            panelInstance.transform.Find("Canvas/Horizontal/Title").GetComponent<BoxCollider>().enabled = false;

            InfoDisplayPanel InfoDisplayPanel = panelInstance.GetComponent<InfoDisplayPanel>();
            InfoDisplayPanel.SetTitleText(gameObject.name);
            InfoDisplayPanel.SetBodyText("Click to see status.");
            InfoDisplayPanel.DismissButton.SetActive(false);
        }
        else
        {
            panelInstance.transform.position = panelOffset + transform.position;
            panelInstance.SetActive(true);
        }

        Debug.Log(gameObject.name + " hovered");
    }

    /// <summary>
    /// Hide the info display pannel. Use as callback for <see href="https://learn.microsoft.com/en-us/dotnet/api/microsoft.mixedreality.toolkit.ui.pressablebutton?view=mixed-reality-toolkit-unity-2020-dotnet-2.8.0" langword="Pressable Button"/> when hover exits.
    /// </summary>
    public void HideDisplayPanel()
    {
        panelInstance?.SetActive(false);
        Debug.Log(gameObject.name + " hided");
    }
}
