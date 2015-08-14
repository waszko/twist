#!/bin/bash
ocamlbuild -use-ocamlfind -pkgs unix -pkgs str twist.native
