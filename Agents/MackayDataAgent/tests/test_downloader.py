import pytest
from downloader.downloaders import Downloader
from utils.conf_utils import load_confs_from_dir,conf_to_dict

# Check api downloaderworks for each point
def test_api_downloaders_from_conf():
    testconfs = load_confs_from_dir('../confs/data')
    for testconf in testconfs:
        print(conf_to_dict(testconf))
        d = Downloader(**(conf_to_dict(testconf)))
        r = d.download_tsinstance()
        print(r)
        assert len(r.times)>0

