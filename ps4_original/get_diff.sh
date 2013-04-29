#!/bin/sh
for file in *; do diff $file ../ps4/$file; done