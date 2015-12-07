#!/bin/sh
cat $@ | docker run -i --rm --name tolerant_tokenizer tolerant_tokenizer -s
