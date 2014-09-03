#!/usr/bin/python

from elasticsearch import Elasticsearch

class LowresInputFileMaker():

	def __init__(self):
		print '- Initializing LowresInputFileMaker -'		
		self.host = 'lbes1.beeldengeluid.nl'
		self.port = 9200
		self.indexName = 'zoekflex'
		self.docType = 'doc'
		self.user = 'admin'
		self.password = 'yk4search'
		self.es_immix = Elasticsearch(hosts=[{
			'host' : self.host,
			'port' : self.port,
			'http_auth' : '%s:%s' % (self.user, self.password)
		}])

	def fetchExpressieOfTaakId(self, taakId):
		item = None
		query = {"query":{"bool":{"must":[{"term":{"doc.expressie.niveaus.taakID.low":"%s" % taakId}}]
			,"must_not":[],"should":[]}},"from":0,"size":2,"sort":[],"facets":{}}
		resp = self.es_immix.search(index=self.indexName, doc_type=self.docType, body=query, timeout="10s")		
		if resp:
			total = resp['hits']['total']			
			for hit in resp['hits']['hits']:
				if hit['_source']['expressie'].has_key('posities'):
					for pos in hit['_source']['expressie']['posities']:
						if pos.has_key('dmguid'):						
							if pos.has_key('beginopdrager') and pos.has_key('eindeopdrager') and pos.has_key('sof'):
								duration = (pos['eindeopdrager'] - int(pos['sof'])) - (pos['beginopdrager'] - int(pos['sof']))							
								item = {
									'dmguid' : pos['dmguid'],
									'dragernummer' : pos['dragernummer'],
									'sof' : pos['sof'],
									'beginopdrager' : pos['beginopdrager'],
									'eindeopdrager' : pos['eindeopdrager'],
									'duration' : duration,
									'taakid' : taakId
								}
								#print item
								return item
								

	def storeAsBoukeFile(self, items, fn):
		#dmguid transcodingStart transcodingEnd outputBestandsNaam
		#20131228004207608007904507980C16E66DA380000004784B00000D0F092765        5000    3006440 5_JAAR_LATER_-NPS00004GWQ__expressie_4206489_5000_3006440.MPG
		f = open(fn, 'w+')
		for item in items:
			if item:
				start = item['beginopdrager'] - int(item['sof'])
				f.write('%s\t%d\t%d\t%s__expressie_%s_%d_%d.MPEG\n' % (
					item['dmguid'],
					start,
					item['duration'],
					item['dragernummer'],
					item['taakid'],
					start,
					item['duration'])
				)

	def run(self):
		print 'starting doing stuff'
		f = open('input-small.txt')
		taakId = None
		items = []
		#i = 0
		for l in f:
			taakId = l.replace('\n', '')			
			items.append(self.fetchExpressieOfTaakId(taakId))
			#i += 1
			#if i == 10:
			#	break
		self.storeAsBoukeFile(items, 'MM-MPEG-input-no-regional.txt')


"""Run the whole thing"""
lim = LowresInputFileMaker()		
lim.run()
