#!/bin/bash
ocamlbuild -use-ocamlfind -pkgs unix -pkgs str -lib graph test/test.native
