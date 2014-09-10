#!/usr/bin/python

import urllib
import json
from xml.dom import minidom, getDOMImplementation
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
PARAMS = '?x-collection=ANP&operation=searchRetrieve&recordSchema=dc&x-fields=content&recordSchema=dc&maximumRecords=' 
QUERY = '&query=' 
DEFKEYWORD = 'drooglegging'

DC = 'http://purl.org/dc/elements/1.1/'
SRW = 'http://www.loc.gov/zing/srw/'
# take url, retrieve results, for every identifier found, then retrieve the
# json NER results and enter them in the DOM
def run(maxrec, keyword):
        #print 'starting doing stuff'
        combiurl = BASEURL+PARAMS+ str(maxrec)+ QUERY+keyword
        print combiurl
        resp = urllib.urlopen(combiurl)
        if resp:
                dom = minidom.parse(resp)
                for node in dom.getElementsByTagNameNS(DC, 'identifier'):
                        identifier = node.firstChild.nodeValue
                      

                        # get the types TODO: doesn t work
                        TPTABASE = "http://tomcat.kbresearch.nl/tpta2/analyse?url="
                        tptaurl= TPTABASE+identifier
                        print tptaurl
                        tptaurlresult = urllib.urlopen(tptaurl)
                        dom2 = minidom.parse(tptaurlresult)

                        entitieslist = dom2.getElementsByTagName('entities')
                        if len(entitieslist) > 0:
                                for ne in entitieslist:
                                        print ne.nodeName + ": "+ ne.firstChild.NodeValue
                        '''
                        NERBASE = "http://tomcat.kbresearch.nl/links/ir?id="
                        nerurl= NERBASE+identifier
                        nerResult = urllib.urlopen(nerurl)
                        #print nerurl
                        data = json.load(nerResult)
                        if 'links' in data:
                                resultElt = dom.createElement("nerResults")
                                for link in data['links']:
                                        try:
                                                elt = dom.createElement("nerResult")
                                                txt = dom.createTextNode(link['name'])
                                                elt.appendChild(txt)
                                                elt.setAttribute("relation", link['relation'])
                                                elt.setAttribute("linkType", link['linkType'])
                                                resultElt.appendChild(elt)
                                        except KeyError, e:
                                                print "Key error reason: %s" %str(e)
                                node.parentNode.appendChild(resultElt)
                                print 'yup'      
                        else:
                                print 'nope'
                        '''
                return dom
                        
def getRecordsForKeywords(n, keywordFile):
        impl = getDOMImplementation()
        doc = impl.createDocument(None, "root", None) 
        keywords = open(keywordFile)
        for keyword in keywords:
                print "Querying for " + str(n)+ " records using keyword: " + keyword
                dom = run(n, keyword)
                for record in dom.getElementsByTagNameNS(SRW, 'record'):
                        doc.firstChild.appendChild(record)
        print "Done" 
        return doc.toxml() 
                


def saveToFile(fileName, string):
        f = open(fileName,'w')
        f.write(string.encode('utf-8'))
        f.close()




### gogo ###

result = getRecordsForKeywords(20, "keywords_videos.txt")        
saveToFile("result20.xml", result)

#result = run(MAXREC,DEFKEYWORD).toxml()
#saveToFile("result"+ KEYWORD + str(MAXREC) + ".xml", result)



