# Download SBM public use files (2016) listed at https://www.cms.gov/CCIIO/Resources/Data-Resources/sbm-puf.html
from bs4 import BeautifulSoup
from urllib.request import urlopen
from urllib.request import urlretrieve
import urllib.parse
import os, sys

html = urlopen("https://www.cms.gov/CCIIO/Resources/Data-Resources/sbm-puf.html")
soup = BeautifulSoup(html.read(), "lxml");
for link in soup.find_all("a"):
	if link["href"].endswith(".xls"):
		url = link["href"]
		saveurl = "https:" + url.split(':')[-1]
		statename = url.split('/')[-2]
		filename = "data-original/sbm-puf/" + statename + "/" + url.split('/')[-1]
		os.makedirs(os.path.dirname(filename), exist_ok=True)
		#query = urllib.parse.quote(saveurl)
		try:
			urlretrieve(saveurl, filename)
		except:
			print(saveurl)
