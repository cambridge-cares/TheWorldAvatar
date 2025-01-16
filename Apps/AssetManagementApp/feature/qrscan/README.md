This module is a standalone qr code scan module. It depends on `core/ui` module for common themes, dimens and drawable. 

It sends out deeplink `android-app://uk.ac.cam.cares.jps.app/info_page?{uri}` with uri for the qr code scanned result. Check `feature/assetinfo` to see how this is consumed.