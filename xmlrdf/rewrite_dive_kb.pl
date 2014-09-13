:- module(rewrite_dive_kb,
	  [ rewrite_kb/0,
	    rewrite_kb/1,
	    rewrite_kb/2,
	    list_rules/0
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(xmlrdf/rdf_convert_util)).
:- use_module(library(xmlrdf/cvt_vocabulary)).
:- use_module(library(xmlrdf/rdf_rewrite)).

:- debug(rdf_rewrite).

%%	rewrite
%
%	Apply all rules on the graph =data=

rewrite_kb :-
	rdf_rewrite(kb).

%%	rewrite(+Rule)
%
%	Apply the given rule on the graph =data=

rewrite_kb(Rule) :-
	rdf_rewrite(kb, Rule).

%%	rewrite(+Graph, +Rule)
%
%	Apply the given rule on the given graph.

rewrite_kb(Graph, Rule) :-
	rdf_rewrite(Graph, Rule).

%%	list_rules
%
%	List the available rules to the console.

list_rules :-
	rdf_rewrite_rules.

:- discontiguous
	rdf_mapping_rule/5.


% if actors with same name: produce link

link_actor
@@
{S,rdf:type,sem:'Actor'},
{S1,rdf:type,sem:'Actor'},
{S,rdfs:label, Lab},
{S1,rdfs:label, Lab}
==>
S\=S1,
{S, owl:sameAs, S1} >> kblinks.


map_to_thesaurus @@
 {P, rdf:type, rdf:'ContentSubject'},
 {_, rdf:contentSubject,P}
 <=>
 true.
map_to_thesaurus @@
 {P, rdf:type, rdf:'ContentSubject'},
 {_, rdf:contentSubject,P}
 <=>
 true.
