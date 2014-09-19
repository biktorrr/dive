:- module(run_dive,
	  [ run_oi/0
	  ]).

user:file_search_path(data,       'C:/Users/vdboer/git/dive/data/').

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_library)).


:- rdf_register_ns(dive,	   'http://purl.org/collections/nl/dive/').
:- rdf_register_ns(sem,            'http://semanticweb.cs.vu.nl/2009/11/sem/').
:- rdf_attach_library('C:/Users/vdboer/git/divedata/void.ttl').

:- use_module([ library(xmlrdf/xmlrdf),
		library(semweb/rdf_cache),
		library(semweb/rdf_library),
		library(semweb/rdf_turtle_write)
	      ]).
:- use_module(rewrite_dive_kb).


:- initialization			% run *after* loading this file
	rdf_set_cache_options([ global_directory('cache/rdf'),
				create_global_directory(true)
			      ]).


load_oi:- rdf_load('C:/Users/vdboer/git/divedata/oi_enriched.ttl',[graph(data)]).

run_oi:-
	load_oi,
	rewrite_oi,
	save_oi.

save_oi:-
	absolute_file_name(data('oi_enriched.ttl'), File,
			   [ access(write)
			   ]),
	rdf_save_turtle(File,[graph(data)]).

clean_all:-
	rdf_retractall(_,_,_).
