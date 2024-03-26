using TMPro;
using UnityEngine;

/// <summary>
/// Script to control the behavior of InfoDisplayPanel gameobject
/// </summary>
public class InfoDisplayPanel : MonoBehaviour
{
    /// <summary>
    /// Dismiss button
    /// </summary>
    [SerializeField]
    public GameObject DismissButton;

    /// <summary>
    /// Placeholder to maintain the position of the title when dismiss button is disabled.
    /// </summary>
    [SerializeField]
    public GameObject PlaceholderPlane;

    /// <summary>
    /// Spinner animation when information is loading.
    /// </summary>
    [SerializeField]
    public GameObject Spinner;

    /// <summary>
    /// Textbox to display information. See <see cref="TextMeshPro"/>.
    /// </summary>
    [SerializeField]
    public GameObject BodyText;

    /// <summary>
    /// Textbox to display title. See <see cref="TextMeshPro"/>.
    /// </summary>
    [SerializeField]
    public GameObject TitleText;

    /// <summary>
    /// Dismiss the current panel
    /// </summary>
    public void DoSelfDestroy()
    {
        Destroy(gameObject);
    }

    /// <summary>
    /// Set body text
    /// </summary>
    public void SetBodyText(string text)
    {
        BodyText.GetComponent<TMP_Text>().text = text;
    }

    /// <summary>
    /// Set title text
    /// </summary>
    public void SetTitleText(string text)
    {
        TitleText.GetComponent<TMP_Text>().text = text;
    }
}

