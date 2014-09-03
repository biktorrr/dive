#!/usr/bin/python

import urllib
import json
from xml.dom import minidom
from pprint import pprint


"""
Get stuff from KB
	
	http://www.kbresearch.nl/tpxslt/?xml=http://jsru.kb.nl/sru/sru?x-collection=ANP&operation=
	searchRetrieve&startRecord=6&maximumRecords=1&recordSchema=dc&query=tatcher&xsl=
	http://www.kbresearch.nl/xportal/full.xsl
	
"""



MAXREC = 100
OUTPUT_DIR = '.'
BASEURL = 'http://jsru.kb.nl/sru/sru'
PARAMS = '?x-collection=ANP&operation=searchRetrieve&recordSchema=dc&x-fields=content&recordSchema=dc&maximumRecords=' + str(MAXREC)
QUERY = '&query=' 
KEYWORD = 'drooglegging'

DC = 'http://purl.org/dc/elements/1.1/'

# take url, retrieve results, for every identifier found, then retrieve the
# json NER results and enter them in the DOM
def run():
        print 'starting doing stuff'
        combiurl = BASEURL+PARAMS+QUERY+KEYWORD
        print combiurl
        resp = urllib.urlopen(combiurl)
        if resp:
                dom = minidom.parse(resp)
                for node in dom.getElementsByTagNameNS(DC, 'identifier'):
                        identifier = node.firstChild.nodeValue
                        NERBASE = "http://tomcat.kbresearch.nl/links/ir?id="
                        nerurl= NERBASE+identifier
                        nerResult = urllib.urlopen(nerurl)
                        print nerurl
                        data = json.load(nerResult)
                        if 'links' in data:
                                resultElt = dom.createElement("nerResults")
                                for link in data['links']:
                                        elt = dom.createElement("nerResult")
                                        txt = dom.createTextNode(link['name'])
                                        elt.appendChild(txt)
                                        elt.setAttribute("relation", link['relation'])
                                        elt.setAttribute("linkType", link['linkType'])
                                        resultElt.appendChild(elt)
                                node.parentNode.appendChild(resultElt)
      
                        else:
                                print 'nope'
                return dom.toxml()
                        
def saveToFile(fileName, string):
        f = open(fileName,'w')
        f.write(string.encode('utf-8'))
        f.close()

# gogo 
result = run()
saveToFile("result"+ KEYWORD + str(MAXREC) + ".xml", result)



