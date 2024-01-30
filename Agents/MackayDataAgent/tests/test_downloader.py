import pytest
from downloader.downloaders import Downloader
from utils.conf_utils import load_confs_from_dir

# Check api downloaderworks for each point
def test_api_downloaders():
    testconfs = load_confs_from_dir('../confs/data')
    for testconf in testconfs:
        d = Downloader(testconf)
        r = d.download_tsinstance()
        assert len(r.times)>0
