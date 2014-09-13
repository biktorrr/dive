:- module(run_dive,
	  [ run_kb/0
	  ]).

user:file_search_path(data,       'C:/Users/vdboer/git/divedata/').

:- use_module(library(semweb/rdf_db)).

:- rdf_register_ns(dive,	   'http://purl.org/collections/nl/dive/').

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


run_kb:-
	rewrite_kb,
	save_kb.

save_kb:-
	absolute_file_name(data('kb_enriched.ttl'), File,
			   [ access(write)
			   ]),
	rdf_save_turtle(File,[graph(kb)]),
	absolute_file_name(data('kb_to_oi_links.ttl'), File1,
			   [ access(write)
			   ]),
	rdf_save_turtle(File1,[graph(kblinks)]).
