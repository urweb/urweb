#!/usr/bin/python3

import sys
import json
import time
import subprocess
import urllib.request
import urllib.parse
import os

def main():
    prefix = 'http://localhost:8080/'

    with open('/tmp/endpoints.json') as json_data:
        data = json.load(json_data)
        endpoints = data['endpoints']
        for ep in endpoints:
            path = ep['url']
            src = urllib.parse.urljoin(prefix, path)
            if ep['method'] == 'GET':
                contents = urllib.request.urlopen(src).read()
                # it's okay that we can retrieve it, enough for us right now
            else:
                # TODO: add support for parameters?
                post_fields = {'Nam': 'X', 'Msg': 'message', 'Sameday': 'on'}     # Set POST fields here
                request = urllib.request.Request(src, urllib.parse.urlencode(post_fields).encode())
                contents = urllib.request.urlopen(request).read().decode()

if __name__ == '__main__':
    main()
