from downloader.downloaders import ApiDownloader
from utils.conf_utils import load_conf
testconf = load_conf('../confs/data/dwelling_unit_API.cfg')
d = ApiDownloader(testconf)
r = d.download_tsinstance()
print(r)