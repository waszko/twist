#!/bin/bash
ocamlbuild -use-ocamlfind -pkg unix -pkg str -pkg ocamlgraph test/test.native
