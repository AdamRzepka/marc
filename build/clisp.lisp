#!/usr/bin/clisp

(require :asdf)
(push (truename ".") asdf:*CENTRAL-REGISTRY*)
(asdf:load-system :marc)

(in-package :marc)

(ext:saveinitmem "bin/marc" :quiet t :init-function #'marc:main :script t :executable t)