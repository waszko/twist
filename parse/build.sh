#!/bin/bash
ocamlbuild -use-ocamlfind -pkgs unix -pkgs str fol.native
