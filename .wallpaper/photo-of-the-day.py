#!/usr/bin/env python3

from bs4 import BeautifulSoup
import urllib.request as request
import os

with request.urlopen('http://www.nationalgeographic.com/photography/photo-of-the-day/') as url:
    soup = BeautifulSoup(url.read(), 'lxml')

    img = soup.findAll(attrs={'property':'og:image'})
    if len(img) > 0:
        print (img[0]['content'])
        target = os.path.join(os.path.expanduser('~'), '.wallpaper/wallpaper.jpg')
        request.urlretrieve(img[0]['content'], target)

        desc = soup.findAll(attrs={'name':'description'})[0]['content']
        print (desc)
        os.system('notify-send "{}"'.format(desc))
