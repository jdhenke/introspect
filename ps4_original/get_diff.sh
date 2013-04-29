#!/bin/sh
for file in *.scm; do diff $file ../ps4/$file; done