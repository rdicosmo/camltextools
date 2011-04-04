CAMLC=ocamlc
CAMLLEX=ocamllex
CAMLYACC=ocamlyacc
FLAGS=
CAMLCOPT=ocamlopt

texexpand: texexpand.ml
	$(CAMLCOPT) -o $@ unix.cmxa $<

clean:
	rm -f *.cmo *.cmi *.cmx *.o texexpand texexpand.ml

# generic rules :
#################
 
.SUFFIXES: .mll .mly .mli .ml .cmi .cmo .cmx .bin
 
.mll.mli:
	$(CAMLLEX) $<

.mll.ml:
	$(CAMLLEX) $<

.mly.ml:
	$(CAMLYACC) $<

.mli.cmi:
	$(CAMLC) -c $(FLAGS) $<
 
.ml.cmi:
	$(CAMLC) -c $(FLAGS) $<
 
.ml.cmo:
	$(CAMLC) -c $(FLAGS) $<
 
.ml.o:
	$(CAMLCOPT) -c $(FLAGS) $<
 
.ml.cmx:
	$(CAMLCOPT) -c $(PROFILE) $(FLAGS) $<

.DEFAULT:

%:  %.cmo
	$(CAMLC) -o $* $<
