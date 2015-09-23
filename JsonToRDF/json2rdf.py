#!/usr/bin/python

"""JSON to RDF conversion tool
Converts a JSON file from CrowdTruth to an RDF file for DIVE.
Some example input files are "v1.json" and "oi_oana_data.json".

Usage:
    json2Rdf <input> <output>
"""

from docopt import docopt
import sys
import json
import rdflib
from rdflib import Graph, Literal, BNode, Namespace, RDF, URIRef
from rdflib.namespace import DC, FOAF, RDF, RDFS, DCTERMS

DIVEStr = "http://purl.org/collections/nl/dive/"
DIVE = Namespace(DIVEStr)

OA = Namespace("http://www.w3.org/ns/oa#")
SEM = Namespace("http://semanticweb.cs.vu.nl/2009/11/sem/")

# For an Oana JSON file, get all kinds of DIVE triples out.
def getContent(fileName):
   json_string = open(fileName)
   data = json.load(json_string)

   # new graph
   g = rdflib.Graph()   # add other graphs (for annotations?)
   print "Found %d records, converting." % len(data)
   for record in data:
      print('.'),
      MOURI = URIRef(DIVEStr + record["hash"]) # media object uri (hash or something nicer?)

      g.add((MOURI, RDF.type, DIVE.MediaObject)) #type
      g.add((MOURI, DCTERMS.identifier, Literal(record["title"])))   # id at OI
      g.add((MOURI, DIVE.source, URIRef(record["metadataContent"]["metadata"]["online_url"])))   # actual video URI
      g.add((MOURI, DIVE.placeholder, URIRef(record["metadataContent"]["metadata"]["medium"]["thumbnail"][0])))   # placeholder img URI
      g.add((MOURI, DIVE.datestamp, URIRef(record["metadataContent"]["datestamp"]))) #date of video
      g.add((MOURI, DCTERMS.description, Literal(record["metadataContent"]["metadata"]["description"]["nl"], lang="nl")))
      g.add((MOURI, DCTERMS.abstract, Literal(record["metadataContent"]["metadata"]["abstract"]["nl"], lang="nl"))) # only NL for now
      g.add((MOURI, RDFS.label, Literal(record["metadataContent"]["metadata"]["title"]["nl"])))

      for entity in record["hasEnrichment"]:
         EURI = URIRef(DIVEStr + "entity/" + entity["id"])

         #rdf type
         rdftype = entity["diveEntityType"]
         if rdftype == "Actor":
            g.add((EURI, RDF.type, SEM.Actor))
         elif rdftype == "Location":
             g.add((EURI, RDF.type, SEM.Place))
         elif rdftype == "Time":
             g.add((EURI, RDF.type, SEM.Time))
         elif rdftype == "Event":
             g.add((EURI, RDF.type, SEM.Event))
         elif rdftype == "Object":
             g.add((EURI, RDF.type, SKOS.Concept))
         else :
             g.add((EURI, RDF.type, DIVE.Entity))


         g.add((EURI, RDFS.label, Literal(entity["label"],lang="nl")))
         g.add((EURI, DIVE.depictedBy, MOURI))

         if(entity["type"]!= "null"):
            g.add((EURI, DIVE.dbpediaType, Literal(entity["type"])))
            g.add((EURI, DIVE.dbpediaResource, Literal(entity["resource"])))
            g.add((EURI, DIVE.hasExternalLink, Literal(entity["resource"])))

         if("hasTimeStamp" in entity):
            g.add((EURI, DIVE.hasTimeStamp, Literal(entity["hasTimeStamp"])))

         if("hasTime" in entity):
            if(entity["hasTime"] != "null"):
               g.add((EURI, DIVE.hasTime, Literal(entity["hasTime"])))

         # links between entities
         if("hasLinks" in entity):
            for link in entity["hasLinks"]:
               TOURI = URIRef(DIVEStr + "entity/" + link["hasTarget"])
               linktype = link["type"]
               if (linktype =="Actor"):
                  g.add((EURI, SEM.hasActor, TOURI))
               elif (linktype =="Location"):
                  g.add((EURI, SEM.hasPlace, TOURI))
               elif (linktype =="Time"):
                  g.add((EURI, SEM.hasTime, TOURI))
               elif (linktype =="Object"):
                  g.add((EURI, DIVE.relatedConcept, TOURI))
               elif (linktype =="Event"):
                  g.add((EURI, DIVE.relatedEvent, TOURI))
               else:
                  g.add((EURI, DIVE.isRelatedTo, TOURI))

         #Annotation triples
         ANURI = URIRef(DIVEStr + "annotation/" + entity["id"])
         g.add((ANURI, RDF.type, OA.Annotation))
         g.add((ANURI, OA.hasBody, EURI))
         g.add((ANURI, OA.hasTarget, MOURI))
         g.add((ANURI, DIVE.startoffset, Literal(entity["startOffset"])))
         g.add((ANURI, DIVE.endOffset, Literal(entity["endOffset"])))
         g.add((ANURI, DIVE.prov, Literal(entity["prov"])))

   return g

def convert(inputFileName, outputFileName):
    print "Loading graph from " + inputFileName
    g = getContent(inputFileName)
    print "\ndone. Saving to " + outputFileName + "..."
    g.serialize(outputFileName, format='turtle')

if __name__ == '__main__':
    args = docopt(__doc__, sys.argv[1:])
    convert(args['<input>'], args['<output>'])
