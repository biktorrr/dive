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
      mouristring = DIVEStr + urllib.quote_plus(node.getElementsByTagNameNS(DCN,'title')[0].firstChild.nodeValue)
      
      MOURI = URIRef(mouristring) # media object uri      
      g.add((MOURI, RDF.type, DIVE.MediaObject)) #type
      g.add((MOURI, DCTERMS.identifier, Literal(uri)))   # id at OI
      g.add((MOURI, DIVE.source, URIRef(uri)))  # actual video URI
      g.add((MOURI, DIVE.placeholder, URIRef(uri)))   # placeholder img URI

      g.add((MOURI, DIVE.datestamp, Literal(node.getElementsByTagNameNS(DCN,'date')[0].firstChild.nodeValue))) #date of bulletin
	  modescription = node.getElementsByTagName('content')[0].firstChild.nodeValue
      g.add((MOURI, DCTERMS.description, Literal(modescription, lang="nl")))
      if (len(node.getElementsByTagNameNS(DCN,'abstract'))>0):
         g.add((MOURI, DCTERMS.abstract, Literal(node.getElementsByTagNameNS(DCN,'abstract')[0].firstChild.nodeValue, lang="nl")))
      g.add((MOURI, RDFS.label, Literal(node.getElementsByTagNameNS(DCN,'title')[0].firstChild.nodeValue, lang="nl")))

      # build one event
      evtlabel = node.getElementsByTagNameNS(DCN,'title')[0].firstChild.nodeValue
      EVURI = URIRef(DIVEStr + "entity/evt-" + urllib.quote_plus(evtlabel))
      g.add((EVURI, RDF.type, SEM.Event))
      g.add((EVURI, RDFS.label, Literal(evtlabel, lang="nl")))
	  g.add((EVURI, DCTERMS.description, Literal(modescription, lang="nl")))
      g.add((EVURI, DIVE.depictedBy, MOURI))
      g.add((EVURI, DIVE.hasTimeStamp, Literal(node.getElementsByTagNameNS(DCN,'date')[0].firstChild.nodeValue)))
      
     #Annotation triples
      ANEVURI = URIRef(DIVEStr + "annotation/evt-" + urllib.quote_plus(evtlabel))
      g.add((ANEVURI, RDF.type, OA.Annotation))
      g.add((ANEVURI, OA.hasBody, EVURI))
      g.add((ANEVURI, OA.hasTarget, MOURI)) 
      g.add((ANEVURI, DIVE.prov, Literal("assumed event from bulletin")))                 

      
      # link other entities
      for entity in node.getElementsByTagName('nerResult'):
         content = entity.firstChild.nodeValue
         try:
            #EURI = URIRef(DIVEStr + "entity/kb-" + urllib.quote_plus(content))

            # Give entity type
            rdftype = entity.getAttribute("neType")
            if rdftype == "organisation":
               EURI = URIRef(DIVEStr + "entity/kb-org-" + urllib.quote_plus(content))
               g.add((EURI, RDF.type, SEM.Actor))
               g.add((EVURI, SEM.hasActor, EURI))
            elif rdftype == "location":
               EURI = URIRef(DIVEStr + "entity/kb-loc-" + urllib.quote_plus(content))
               g.add((EURI, RDF.type, SEM.Place))
               g.add((EVURI, SEM.hasPlace, EURI))
            elif rdftype == "person":
               EURI = URIRef(DIVEStr + "entity/kb-per-" + urllib.quote_plus(content))
               g.add((EURI, RDF.type, DIVE.Person))
               g.add((EVURI, SEM.hasActor, EURI))
            elif rdftype == "other":
               EURI = URIRef(DIVEStr + "entity/kb-oth-" + urllib.quote_plus(content))
               g.add((EURI, RDF.type, DIVE.Entity))
               g.add((EVURI, DIVE.isRelatedTo, EURI))
            else :
               EURI = URIRef(DIVEStr + "entity/kb-unk-" + urllib.quote_plus(content))
               g.add((EURI, RDF.type, DIVE.Entity))
               g.add((EVURI, DIVE.isRelatedTo, EURI))

            g.add((EURI, RDFS.label, Literal(content ,lang="nl")))
            g.add((EURI, DIVE.depictedBy, MOURI))

            if(entity.hasAttribute("relation")):
               g.add((EURI, DIVE.dbpediaResource, Literal(entity.getAttribute("relation"))))
               g.add((EURI, DIVE.hasExternalLink, Literal(entity.getAttribute("relation"))))

            g.add((EURI, DIVE.relatedEvent, EVURI))
     
            #Annotation triples
            ANURI = URIRef(DIVEStr + "annotation/" + urllib.quote_plus(evtlabel+"-"+ content ))
            g.add((ANURI, RDF.type, OA.Annotation))
            g.add((ANURI, OA.hasBody, EURI))
            g.add((ANURI, OA.hasTarget, MOURI))
            g.add((ANURI, DIVE.prov, Literal("KB NER")))
         except KeyError, e:
            print '',
            
   return g
      
     
   
                       

fn = "result30.xml"
g = getContent(fn)
print "\ndone. saving..."

#of = "C:/Users/victor/git/divedata/kb_enriched.ttl"
of = "kb_enriched_more.ttl"
g.serialize(of, format='turtle') 
