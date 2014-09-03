#!/usr/bin/python

"""
OpenBeelden OAI harvester for the B&G set

PyOAI docs: 
	https://svn.infrae.com/pyoai/tag/pyoai-2.4/doc/API.html
	
	
	
"""
import xlwt
from oaipmh.client import Client
from oaipmh.metadata import MetadataRegistry, oai_dc_reader, MetadataReader
from oaipmh.server import oai_dc_writer
import itertools
from elasticsearch import Elasticsearch
import codecs
from dateutil.parser import parse
from openskos_handler import *

#ElasticSearch client
es_immix = Elasticsearch(hosts=[{'host' : 'elastic-a.beeldengeluid.nl', 'port' : 80}])

#Een specifieke reader voor de oai_oi namespace van de Openbeelden OAI data provider
oai_oi_reader = MetadataReader(
    fields={
    'title':       ('textList', 'oai_oi:oi/oi:title/text()'),
    'alternative':       ('textList', 'oai_oi:oi/oi:alternative/text()'),
    'creator':     ('textList', 'oai_oi:oi/oi:creator/text()'),
    'subject':     ('textList', 'oai_oi:oi/oi:subject/text()'),
    'description': ('textList', 'oai_oi:oi/oi:description/text()'),
    'abstract': ('textList', 'oai_oi:oi/oi:abstract/text()'),
    'publisher':   ('textList', 'oai_oi:oi/oi:publisher/text()'),
    'contributor': ('textList', 'oai_oi:oi/oi:contributor/text()'),
    'date':        ('textList', 'oai_oi:oi/oi:date/text()'),
    'type':        ('textList', 'oai_oi:oi/oi:type/text()'),
    'extent':        ('textList', 'oai_oi:oi/oi:extent/text()'),
    'medium':        ('textList', 'oai_oi:oi/oi:medium/text()'),    
    'identifier':  ('textList', 'oai_oi:oi/oi:identifier/text()'),
    'source':      ('textList', 'oai_oi:oi/oi:source/text()'),
    'language':    ('textList', 'oai_oi:oi/oi:language/text()'),
    'references':    ('textList', 'oai_oi:oi/oi:references/text()'),
    'spatial':    ('textList', 'oai_oi:oi/oi:spatial/text()'),
    'attributionName':    ('textList', 'oai_oi:oi/oi:attributionName/text()'),
    'attributionURL':    ('textList', 'oai_oi:oi/oi:attributionURL/text()'),
    'license':      ('textList', 'oai_oi:oi/oi:license/text()')
    },
    
    #TODO notitie maken van het feit dat oai_oi dezelfde ns heeft als oai_dc
    namespaces={
    'oai_oi': 'http://www.openbeelden.nl/feeds/oai/', #'http://www.openarchives.org/OAI/2.0/oai_oi/',
    'oi': 'http://www.openbeelden.nl/oai/'}
    )

#URL = 'http://www.openbeelden.nl/oip-test/feeds/oai/'
URL = 'http://www.openbeelden.nl/feeds/oai/'
#URL = 'http://oai.tuxic.nl/oai/'

#Initieer de OAI client
registry = MetadataRegistry()
registry.registerReader('oai_oi', oai_oi_reader)
client = Client(URL, registry)
x = client.updateGranularity()

#Controleer of de OAI service goed geidentificeerd kan worden
x = client.identify()
print 'identity %s' % x.repositoryName()
print 'identity %s' % x.protocolVersion()
print 'identity %s' % x.baseURL()

OUTPUT_DIR = '/Users/jblom/temp'


print 'Firing up the openSKOSHandler'
osh = OpenSKOSHandler()

def processOpenbeelden():
	i=0
	iarecs = []
	#for y in client.listRecords(metadataPrefix='oai_oi', from_=parse('2011-01-01'), until=parse('2011-11-01')):
	extent = None
	secs = 0
	
	wbk = xlwt.Workbook()
	sheet = wbk.add_sheet('sheet 1')
	i = 1
	
	sheet.write(0,0,'#')
	sheet.write(0,1,'TITLE')
	sheet.write(0,2,'ALTERNATIVE')
	sheet.write(0,3,'CREATOR')
	sheet.write(0,4,'SUBJECT')
	sheet.write(0,5,'DESCRIPTION')
	sheet.write(0,6,'ABSTRACT')
	sheet.write(0,7,'PUBLISHER')
	sheet.write(0,8,'CONTRIBUTOR')
	sheet.write(0,9,'DATE')
	sheet.write(0,10,'TYPE')
	sheet.write(0,11,'EXTENT')
	sheet.write(0,12,'MEDIUM')
	sheet.write(0,13,'IDENTIFIER')
	sheet.write(0,14,'SOURCE')
	sheet.write(0,15,'LANGUAGE')
	sheet.write(0,16,'REFERENCES')
	sheet.write(0,17,'SPATIAL')
	sheet.write(0,18,'ATTRIBUTION NAME')
	sheet.write(0,19,'ATTRIBUTION URL')
	sheet.write(0,20,'LICENSE')
	sheet.write(0,21,'DATESTAMP')
	sheet.write(0,22,'GTAA_EXACT_MATCH')
	sheet.write(0,23,'GTAA_FUZZY_MATCH')
	
	date_xf = xlwt.easyxf(num_format_str='DD-MM-YYYY')
	
	for rec in client.listRecords(metadataPrefix=u'oai_oi', set=u'beeldengeluid'):
		header, metadata, about = rec
				
		"""Load the metadata fields from OAI"""
		title = getFieldData(metadata, 'title')				
		alternative = getFieldData(metadata, 'alternative')				
		creator = getFieldData(metadata, 'creator')			
		subject = getFieldData(metadata, 'subject')						
		description = getFieldData(metadata, 'description')					
		abstract = getFieldData(metadata, 'abstract')
		publisher = getFieldData(metadata, 'publisher')
		contributor = getFieldData(metadata, 'contributor')	
		date = getFieldData(metadata, 'date')
		type = getFieldData(metadata, 'type')		
		extent = metadata.getField('extent')[0]		
		medium = getFieldData(metadata, 'medium')
		identifier = getFieldData(metadata, 'identifier')		
		source = getFieldData(metadata, 'source')				
		language = getFieldData(metadata, 'language')
		references = getFieldData(metadata, 'references')
		spatial = getFieldData(metadata, 'spatial')						
		attributionName = getFieldData(metadata, 'attributionName')
		attributionURL = getFieldData(metadata, 'attributionURL')
		license = getFieldData(metadata, 'license')
		
		"""Get the GTAA terms related to the subject"""
		"""
		gtaaTerms = getGTAATermsBasedOnSubjectAndLocation(subject, spatial)				
		"""
		
		"""If there is no identifier, try to fetch the taakID from iMMix ES"""
		"""
		if identifier == '' and source != '':
			print 'No taakID!'
			taakID = getTaakIDBasedOnSource(source)
			if taakID:
				print 'assigning taakID to the identifier' 
				identifier = taakID
		"""
		
		"""Write the found data in the Excel sheet"""
		sheet.write(i,0,i)
		sheet.write(i,1,title)
		sheet.write(i,2,alternative)
		sheet.write(i,3,creator)
		sheet.write(i,4,subject)
		sheet.write(i,5,description)
		sheet.write(i,6,abstract)
		sheet.write(i,7,publisher)
		sheet.write(i,8,contributor)
		sheet.write(i,9,date)
		sheet.write(i,10,type)
		sheet.write(i,11,getExtentInSeconds(extent))
		sheet.write(i,12,medium)
		sheet.write(i,13,identifier)
		sheet.write(i,14,source)
		sheet.write(i,15,language)
		sheet.write(i,16,references)
		sheet.write(i,17,spatial)
		sheet.write(i,18,attributionName)
		sheet.write(i,19,attributionURL)
		sheet.write(i,20,license)
		sheet.write(i,21, header.datestamp(), date_xf)
		"""
		sheet.write(i,22, ';'.join(gtaaTerms[0]))#exact matches
		sheet.write(i,23, ';'.join(gtaaTerms[1]))#fuzzy matches
		"""
		i += 1
		secs += getExtentInSeconds(extent)
		#if i == 10:
			#break
			
	wbk.save('%s/openbeelden_beng.xls' % OUTPUT_DIR)
	print 'saved'
	print secs
	print 'TOTAL TIME: %s' % secsToTimeString(secs)
	"""
	for s in client.listSets():
		print s
	"""

def getFieldData(metadata, fn):
	return '; '.join(metadata.getField(fn))

def getExtentInSeconds(ext):
	secs = 0
	if ext and ext.find('PT') != -1:
		ext = ext[2:len(ext)]
		if ext.find('H') != -1:			
			secs = int(ext[0:ext.find('H')]) * 3600
			ext = ext[ext.find('H') + 1:len(ext)]
		if ext.find('M') != -1:			
			secs = int(ext[0:ext.find('M')]) * 60
			ext = ext[ext.find('M') + 1:len(ext)]
		if ext.find('S') != -1:			
			secs += int(ext[0:ext.find('S')])
	return secs

def secsToTimeString(secs):
	h = m = s = 0
	while secs - 3600 >= 0:
		h += 1
		secs -= 3600	
	while secs - 60 >= 0:
		m += 1
		secs -= 60
	return '%d:%d:%d' % (h, m, s)
#Run de hoofdfunctie

def getGTAATermsBasedOnSubjectAndLocation(subject, spatial):
	subs = None
	locs = None
	os_res = None
	gtaaExact = []
	gtaaFuzzy = []
	
	"""First add GTAA terms based on the subject(s)"""
	if subject:
		subs = subject.split(';')
		for s in subs:
			 os_res = osh.autoCompleteTable(s)
			 if os_res:
				 if len(os_res) == 1:
					gtaaExact.append('%s,%s' % (os_res[0]['label'], os_res[0]['value']))
				 elif len(os_res) > 1:
					for r in os_res:
						gtaaFuzzy.append('%s,%s' % (r['label'], r['value']))
						
	"""Append the GTAA terms based on the location(s)"""						
	if spatial:
		locs = spatial.split(';')
		for l in locs:
			 os_res = osh.autoCompleteTable(l, 'http://data.beeldengeluid.nl/gtaa/GeografischeNamen')
			 if os_res:
				 if len(os_res) == 1:
					gtaaExact.append('%s,%s' % (os_res[0]['label'], os_res[0]['value']))
				 elif len(os_res) > 1:
					for r in os_res:
						gtaaFuzzy.append('%s,%s' % (r['label'], r['value']))
	
	#print 'Exact matches'
	#print gtaaExact
	#print 'Fuzzy matches'
	#print gtaaFuzzy
	return (gtaaExact, gtaaFuzzy)

def getImmixMetadataBasedOnDrager(drager):
	global tot
	query = {"query":{"bool":{"must":[{"query_string":{"default_field":"positie.dragernummer","query":"\"%s\"" % drager}}],"must_not":[],"should":[]}}}
	#print query
	resp = es_immix.search(index="search_expressie", doc_type="searchable_expressie", body=query, timeout="10s")
	#print resp
	if resp and resp['hits']['total'] == 1:
		for hit in resp['hits']['hits']:
			return hit
	elif resp and resp['hits']['total'] > 1:
		print 'more than one hit...'
		print resp
	return None
	
def getTaakIDBasedOnSource(source):
	dragernrs = str(source).split('; ')
	drager = None
	
	"""Get the drager from the source (sometimes there are two, but most of the times they are the same)"""
	if len(dragernrs) == 2:		
		if dragernrs[0] != dragernrs[1]:
			print dragernrs
			print '>>>>>>>>>> There are two dragers...'
		else:
			drager = dragernrs[0]
	else:
		drager = dragernrs[0]
	
	"""Try to find the taakID related to the drager"""
	if drager:		
		md = getImmixMetadataBasedOnDrager(drager)
		if md:
			taakID = md['_source']['expressie']['niveau']['taakID']
			if taakID:
				print 'Found a taakID: %s\t%s' % (drager, taakID)
				return taakID
	return None


processOpenbeelden()


