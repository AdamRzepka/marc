#!/usr/bin/sbcl --script

(require :asdf)
(push (truename ".") asdf:*CENTRAL-REGISTRY*)
(asdf:load-system :marc)

(in-package :marc)

(sb-ext:save-lisp-and-die "bin/marc" :executable t :toplevel #'main)