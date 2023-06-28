<#macro emailLayout>
<html>
<head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
</head>
<body>
    <div style="margin:auto;max-width:600px;border-radius: 3px;border: 1px;border-style: solid;border-color: gray;">
        <table align="center" width="100%" style="margin: 10px;">
            <tbody>
                <tr>
                    <td style="text-align: center">
                        <img src="https://intranet.cares.cam.ac.uk/wp-content/uploads/2020/08/CARES-small.png" alt="cambridge-cares-logo" width="50%" border="0" style="pointer-events: none;">
                    </td>
                </tr>
            </tbody>
        </table>
        <table align="center" width="100%" style="border-radius: 3px;background-color: rgb(244, 243, 243);">
            <tbody>
                <tr>
                    <td style="color: rgb(11, 11, 11);padding-left: 35px;padding-right: 35px;padding-top: 25px;padding-bottom: 25px;font-size: 14px;font-family: sans-serif;">
                        <#nested>
                    </td>
                </tr>
            </tbody>
        </table>
    </div>
</body>
</html>
</#macro>
