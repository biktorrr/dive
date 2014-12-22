:- use_module(library(http/http_open)).


rdf_register_ns(sem, 'http://semanticweb.cs.vu.nl/2009/11/sem/').
rdf_register_ns(dive, 'http://purl.org/collections/nl/dive/').

link_all:-
	forall(sameThing(A,B),
	       rdf_assert(A,owl:sameAs,B,oi_to_kb_links)).

link_more:-
	forall((sameThing(A,B),
		not(rdf(A,owl:sameAs,B))),
		       rdf_assert(A,owl:sameAs,B,more_oi_to_kb_links)).

sameThing(A,B):-
	    rdf_has(A,rdf:type,sem:'Actor'),
	    rdf_has(B,rdf:type,sem:'Actor'),
	    rdf(A,rdfs:label,LAB),
	    rdf(B,rdfs:label,LAB),
	    A\=B.

sameThing(A,B):-
	    rdf(A,rdf:type,sem:'Event'),
	    rdf(B,rdf:type,sem:'Event'),
	    rdf(A,rdfs:label,LAB),
	    rdf(B,rdfs:label,LAB),
	    A\=B.

sameThing(A,B):-
	    rdf(A,rdf:type,sem:'Place'),
	    rdf(B,rdf:type,sem:'Place'),
	    rdf(A,rdfs:label,LAB),
	    rdf(B,rdfs:label,LAB),
	    A\=B.

sameThing(A,B):-
	    rdf(A,rdf:type,sem:'Time'),
	    rdf(B,rdf:type,sem:'Time'),
	    rdf(A,rdfs:label,LAB),
	    rdf(B,rdfs:label,LAB),
	    A\=B.



