from downloader.downloaders import Downloader
from utils.conf_utils import load_conf
testconf = load_conf('../confs/data/installed_pv.cfg')
d = Downloader(testconf)
r = d.download_tsinstance()
print(r)