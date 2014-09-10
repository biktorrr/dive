#!/usr/bin/python

import json
import rdflib
import urllib
from rdflib import Graph, Literal, BNode, Namespace, RDF, URIRef
from rdflib.namespace import DC, FOAF, RDF, RDFS, DCTERMS
from xml.dom import minidom, getDOMImplementation

DIVEStr = "http://purl.org/collections/nl/dive/"
DIVE = Namespace(DIVEStr)

OA = Namespace("http://www.w3.org/ns/oa#")
SEM = Namespace("http://semanticweb.cs.vu.nl/2009/11/sem/")

DCN = 'http://purl.org/dc/elements/1.1/'
SRW = 'http://www.loc.gov/zing/srw/'

# For a KB XML file, constructed by the get_kb_data.py script, get all kinds of DIVE triples out.
def getContent(fileName):
   xml_string = open(fileName)
   dom = minidom.parse(xml_string)

   # new graph
   g = rdflib.Graph()   # add other graphs (for annotations?)
   
   for node in dom.getElementsByTagNameNS(SRW, 'record'):
      print('.'),
      uri = node.getElementsByTagNameNS(DCN,'identifier')[0].firstChild.nodeValue
      mouristring = DIVEStr + uri.split("urn=")[1]
      
      MOURI = URIRef(mouristring) # media object uri      
      g.add((MOURI, RDF.type, DIVE.MediaObject)) #type
      g.add((MOURI, DCTERMS.identifier, Literal(uri)))   # id at OI
      g.add((MOURI, DIVE.source, URIRef(uri)))  # actual video URI
      g.add((MOURI, DIVE.placeholder, URIRef(uri)))   # placeholder img URI

      g.add((MOURI, DIVE.datestamp, Literal(node.getElementsByTagNameNS(DCN,'date')[0].firstChild.nodeValue))) #date of bulletin
      g.add((MOURI, DCTERMS.description, Literal(node.getElementsByTagName('content')[0].firstChild.nodeValue, lang="nl")))
      if (len(node.getElementsByTagNameNS(DCN,'abstract'))>0):
         g.add((MOURI, DCTERMS.abstract, Literal(node.getElementsByTagNameNS(DCN,'abstract')[0].firstChild.nodeValue, lang="nl")))
      g.add((MOURI, RDFS.label, Literal(node.getElementsByTagNameNS(DCN,'title')[0].firstChild.nodeValue, lang="nl")))

      # build one event
      EVURI = URIRef(DIVEStr + "entity/evt-" + urllib.quote_plus(uri.split("urn=")[1]))
      g.add((EVURI, RDF.type, SEM.Event))
      g.add((EVURI, RDFS.label, Literal(node.getElementsByTagNameNS(DCN,'title')[0].firstChild.nodeValue, lang="nl")))
      g.add((EVURI, DIVE.depictedBy, MOURI))
      g.add((EVURI, DIVE.hasTimeStamp, Literal(node.getElementsByTagNameNS(DCN,'date')[0].firstChild.nodeValue)))
     #Annotation triples
      ANEVURI = URIRef(DIVEStr + "annotation/evt-" + urllib.quote_plus(uri.split("urn=")[1]))
      g.add((ANEVURI, RDF.type, OA.Annotation))
      g.add((ANEVURI, OA.hasBody, EVURI))
      g.add((ANEVURI, OA.hasTarget, MOURI)) 
      g.add((ANEVURI, DIVE.prov, Literal("assumed event from bulletin")))                 

      
      # link other entities TODO: type???
      for entity in node.getElementsByTagName('nerResult'):
         content = entity.firstChild.nodeValue
         EURI = URIRef(DIVEStr + "entity/" + urllib.quote_plus(content))
         g.add((EURI, RDF.type, DIVE.Entity)) #TODO: type???

         g.add((EURI, RDFS.label, Literal(content ,lang="nl")))
         g.add((EURI, DIVE.depictedBy, MOURI))

         if(entity.hasAttribute("relation")):
            g.add((EURI, DIVE.dbpediaResource, Literal(entity.getAttribute("relation"))))
            g.add((EURI, DIVE.hasExternalLink, Literal(entity.getAttribute("relation"))))

         g.add((EURI, DIVE.relatedEvent, EVURI))
  
         #Annotation triples
         ANURI = URIRef(DIVEStr + "annotation/" +  (urllib.quote_plus(uri.split("urn=")[1]+"-"+ content )))
         g.add((ANURI, RDF.type, OA.Annotation))
         g.add((ANURI, OA.hasBody, EURI))
         g.add((ANURI, OA.hasTarget, MOURI))
         g.add((ANURI, DIVE.prov, Literal("KB NER")))
   return g
      
     
   
                       

fn = "result20.xml"
g = getContent(fn)
print "\ndone. saving..."

of = "C:/Users/vdboer/git/divedata/kb_enriched.ttl"
g.serialize(of, format='turtle') 
