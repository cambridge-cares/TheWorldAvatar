CALL activate.bat
setlocal
cd /d %~dp0
scrapy crawl ships
setlocal
cd /d sg_hk
scrapy crawl sg_ship
setlocal
cd /d %~dp0
python merge.py
python update_database.py

