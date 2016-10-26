#!/usr/bin/python

import urllib
import json
from xml.dom import minidom, getDOMImplementation
from pprint import pprint
import rdflib
from rdflib import Graph, Literal, BNode, Namespace, RDF, URIRef
from rdflib.namespace import DC, FOAF, RDF, RDFS, DCTERMS

import codecs


DIVEStr = "http://purl.org/collections/nl/dive/"
DIVE = Namespace(DIVEStr)
OA = Namespace("http://www.w3.org/ns/oa#")
SEM = Namespace("http://semanticweb.cs.vu.nl/2009/11/sem/")
DC = 'http://purl.org/dc/elements/1.1/'
SRW = 'http://www.loc.gov/zing/srw/'


MAXREC = 100
OUTPUT_DIR = '.'
PARAMS = '?x-collection=ANP&operation=searchRetrieve&recordSchema=dc&x-fields=content&recordSchema=dc&maximumRecords=' 
QUERY = '&query=' 
DEFKEYWORD = 'drooglegging'
PREFIX = 'http://purl.org/collections/nl/dive/kb/'
TPTABASE = "http://tomcat.kbresearch.nl/tpta2/analyse?url="


# take url, retrieve results, for every identifier found, then retrieve the
# json NER results and enter them in the DOM

def run(inputFileName):
        print 'Parsing input file...',
        resp = open(inputFileName)
        if resp:
                dom = minidom.parse(resp)
                g = rdflib.Graph()   # add other graphs (for annotations?)
                print 'done.'
                print 'Iterating through elements:',
                countert=0
                tot=len(dom.getElementsByTagName("srw_dc:dcx"))
                for node in dom.getElementsByTagName("srw_dc:dcx"):
                        #counter
                        if countert%100 == 0:
                                print "\t" + str(countert) + "/" + str(tot)
                                
                        countert=countert+1
                                
                        oneTTLS = ''
                        identifier = node.getElementsByTagName("dcx:recordIdentifier")[0].childNodes[0].nodeValue
                        URIString = PREFIX + identifier
                        #print URIString
                        dcid = node.getElementsByTagName("dc:identifier")[0].childNodes[0].nodeValue
                        dctitle = node.getElementsByTagName("dc:title")[0].childNodes[0].nodeValue
                        dcdate = node.getElementsByTagName("dc:date")[0].childNodes[0].nodeValue
                        placeholder = dcid + ":image"
                        source=placeholder

                        # ADD all info about the Media Object
                        MOURI = URIRef(URIString) # media object uri      
                        g.add((MOURI, RDF.type, DIVE.MediaObject))
                        g.add((MOURI, DCTERMS.identifier, Literal(dcid)))
                        g.add((MOURI, RDFS.label, Literal(dctitle)))
                        g.add((MOURI, DCTERMS.title, Literal(dctitle)))
                        g.add((MOURI, DIVE.source, URIRef(source)))
                        g.add((MOURI, DIVE.placeholder, URIRef(placeholder)))
                        g.add((MOURI, DIVE.datestamp, Literal(dcdate)))

                        # ADD all info about the Media Object
                        description = getDescription(dcid)
                        g.add((MOURI, DCTERMS.description, Literal(description)))

                        # build one event
                        evtlabel = description[:100]
                        EVURI = URIRef(PREFIX + "evt-" + identifier)
                        g.add((EVURI, RDF.type, SEM.Event))
                        g.add((EVURI, RDFS.label, Literal(evtlabel, lang="nl")))
                        g.add((EVURI, DIVE.depictedBy, MOURI))
                        g.add((EVURI, DIVE.hasTimeStamp, Literal(dcdate)))

                        # get the NE results and add them to the graph
                        
                        tptaurl= TPTABASE+dcid+":ocr"
                       # print tptaurl
                        doNERs(tptaurl, g, MOURI, EVURI)
        print "Done"
        return g


def getDescription(identif):
        result = ""
        url = identif + ":ocr"
        urlresult = urllib.urlopen(url)
        dom3 = minidom.parse(urlresult)
        for par in dom3.getElementsByTagName("p"):
                result = result + par.childNodes[0].nodeValue + "\n\n"
        return result
        

def doNERs(tptaurl, g, MOURI, EVURI):
        tptaurlresult = urllib.urlopen(tptaurl)
        dom2 = minidom.parse(tptaurlresult)
        entities = dom2.getElementsByTagName('entities')
        total = len(entities)
        if total >0:                        
                        try:
                                for ne in entities:
                                        if (ne.nodeType == ne.ELEMENT_NODE):
                                                netype = ne.nodeName
                                                content= ne.firstChild.nodeValue
                                               # print netype, content
                                                if netype == "organisation":
                                                       EURI = URIRef(DIVEStr + "kb-org-" + urllib.quote_plus(content))
                                                       g.add((EURI, RDF.type, SEM.Actor))
                                                       g.add((EVURI, SEM.hasActor, EURI))
                                                elif netype == "location":
                                                       EURI = URIRef(DIVEStr + "kb-loc-" + urllib.quote_plus(content))
                                                       g.add((EURI, RDF.type, SEM.Place))
                                                       g.add((EVURI, SEM.hasPlace, EURI))
                                                elif netype == "person":
                                                       EURI = URIRef(DIVEStr + "kb-per-" + urllib.quote_plus(content))
                                                       g.add((EURI, RDF.type, DIVE.Person))
                                                       g.add((EVURI, SEM.hasActor, EURI))
                                                elif netype == "other":
                                                       EURI = URIRef(DIVEStr + "kb-oth-" + urllib.quote_plus(content))
                                                       g.add((EURI, RDF.type, DIVE.Entity))
                                                       g.add((EVURI, DIVE.isRelatedTo, EURI))
                                                else :
                                                       EURI = URIRef(DIVEStr + "kb-unk-" + urllib.quote_plus(content))
                                                       g.add((EURI, RDF.type, DIVE.Entity))
                                                       g.add((EVURI, DIVE.isRelatedTo, EURI))

                                                g.add((EURI, RDFS.label, Literal(content ,lang="nl")))
                                                g.add((EURI, DIVE.depictedBy, MOURI))
                        except KeyError, e:
                                print 'e',
                                             

                                
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
        #print doc.toxml('utf-8')
        return doc.toxml('utf-8')
                


def saveToFile(fileName, string):
        f = open(fileName,'w')
        f.write(string)
        f.close()


def saveToTurtle(g, fn):
        g.serialize(fn, format='turtle') 


def run_all():
        ipf = "anp.xml"
        of = "kb_enriched_new1.ttl"
        g = run(ipf)
        print 'Saving to', of
        saveToTurtle(g,of)
        print 'Done'
        

### gogo ###



#result = run(MAXREC,DEFKEYWORD).toxml()
#saveToFile("result"+ KEYWORD + str(MAXREC) + ".xml", result)



